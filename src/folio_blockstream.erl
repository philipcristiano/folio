-module(folio_blockstream).

-include_lib("kernel/include/logger.hrl").
-include_lib("decimal/include/decimal.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(folio_account_provider).

-define(DOPTS, #{precision => 10, rounding => round_floor}).

-export([folio_init/0]).

-export([balance/1]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    folio_throttle:setup(?MODULE, 10, per_second).

setup_properties() ->
    [
        #{
            fields => #{
                address => #{}
            }
        },
        #{
            fields => #{
                xyzpub => #{},
                format => #{
                    choices => #{
                        p2pkh => #{description => <<"Addresses starting with 1">>},
                        p2sh => #{description => <<"Addresses starting with 3">>}
                        %#{bech32 => #{description => <<"Addresses starting with bc1">>}}
                    }
                }
            }
        }
    ].

add(IntegrationID, #{address := Addr}) ->
    Credentials = #{address => Addr},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok;
add(IntegrationID, #{derivation := D, format := AddrFormat, xyzpub := Pub}) ->
    Credentials = #{deriviation => D, format => address_format_to_atom(AddrFormat), xyzpub => Pub},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

address_format_to_atom(<<"p2sh">>) -> p2sh;
address_format_to_atom(<<"p2pkh">>) -> p2pkh.

accounts_init(Integration = #{id := IntegrationID}) ->
    Creds = folio_credentials_store:get_credentials(IntegrationID),
    accounts_init_by_credentials(Integration, Creds).

accounts_init_by_credentials(_Integration = #{id := IntegrationID}, #{address := Addr}) ->
    #{
        address => Addr,
        integration_id => IntegrationID
    };
accounts_init_by_credentials(_Integration = #{id := IntegrationID}, #{
    xyzpub := Pub, format := Format
}) ->
    #{
        format => address_format_to_atom(Format),
        xyzpub => Pub,
        derivations => [
            #{
                account => 0,
                index => 0,
                gap_remaining => 20
            },
            #{
                account => 1,
                index => 0,
                gap_remaining => 20
            }
        ],
        integration_id => IntegrationID
    }.

-spec sats_to_btc(integer()) -> decimal:decimal().
sats_to_btc(Sats) ->
    SatD = decimal:to_decimal(Sats, #{precision => 10, rounding => round_floor}),
    SatsPerBTC = decimal:to_decimal(100000000, ?DOPTS),
    decimal:divide(SatD, SatsPerBTC, ?DOPTS).

% If no derviations remaining, move accounts is complete
accounts(State = #{xyzpub := _Pub, derivations := []}) ->
    {complete, [], State};
% If no gap remaining, move on to the next derivation
accounts(State = #{xyzpub := _Pub, derivations := [#{gap_remaining := GR} | TD]}) when GR < 0 ->
    accounts(State#{derivations => TD});
% Derivations and gap remaining, check 1 address
accounts(State = #{format := Format, xyzpub := Pub, derivations := [HD | TD]}) ->
    #{
        account := AccountNum,
        index := IndexNum,
        gap_remaining := GR
    } = HD,

    Derivation = derivation(AccountNum, IndexNum),

    Addr = pub_to_address(Pub, Derivation, Format),

    {ok, #{
        <<"chain_stats">> := #{
            <<"funded_txo_sum">> := InSats,
            <<"spent_txo_sum">> := OutSats
        }
    }} = balance(Addr),

    % If there are transactions, reset gap limit
    NextDerivations =
        case {InSats, OutSats} of
            {0, 0} ->
                [
                    HD#{
                        gap_remaining => GR - 1,
                        index => IndexNum + 1
                    }
                    | TD
                ];
            {_, _} ->
                [
                    HD#{
                        gap_remaining => 20,
                        index => IndexNum + 1
                    }
                    | TD
                ]
        end,

    Balance = sats_to_balance(InSats, OutSats),
    Accounts = [to_account(Addr, Balance)],

    {incomplete, Accounts, State#{derivations => NextDerivations}};
accounts(State = #{address := Addr}) ->
    {ok, #{
        <<"chain_stats">> := #{
            <<"funded_txo_sum">> := InSats,
            <<"spent_txo_sum">> := OutSats
        }
    }} = balance(Addr),

    Balance = sats_to_balance(InSats, OutSats),
    Accounts = [to_account(Addr, Balance)],

    {complete, Accounts, State}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    % #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    State = #{
        account_id => AccountID,
        address => AccountID,
        integration_id => IntegrationID,
        % 0 can be used to instead of omitting. The code is simpler without having to handle if this exists or not
        last_seen_txid => <<"0">>
    },
    State.

account_transactions(State) ->
    {Transactions, State1} = transactions(State),
    Completeness =
        case length(Transactions) of
            0 -> complete;
            _ -> incomplete
        end,
    {Completeness, Transactions, State1}.

balance(Address) when is_binary(Address) ->
    Path = <<<<"/api/address/">>/binary, Address/binary>>,
    {ok, _, _, D} = request(Path),
    {ok, D}.

transactions(State = #{address := Address, last_seen_txid := LSTXID}) ->
    Path = <<<<"/api/address/">>/binary, Address/binary, <<"/txs/chain/">>/binary, LSTXID/binary>>,
    {ok, _, _, D} = request(Path),
    {Transactions, State1} = blockstream_txs_to_transactions(D, State),
    {lists:flatten(Transactions), State1}.

blockstream_txs_to_transactions(Data, State) ->
    lists:foldl(
        fun(TX, {ExistingTXs, AccState}) ->
            {NewTXs, AccState1} = blockstream_tx_to_transactions(TX, AccState),
            {ExistingTXs ++ NewTXs, AccState1}
        end,
        {[], State},
        Data
    ).

-spec blockstream_tx_to_transactions(map(), any()) ->
    {folio_integration:account_transactions(), any()}.
blockstream_tx_to_transactions(#{<<"status">> := #{<<"confirmed">> := false}}, State) ->
    {[], State};
blockstream_tx_to_transactions(
    #{
        <<"txid">> := TXID,
        <<"vin">> := Vin,
        <<"vout">> := Vout,
        <<"status">> := #{<<"confirmed">> := true, <<"block_time">> := BlockTimeSinceEpoch}
    },
    State = #{address := Addr}
) ->
    State1 = State#{last_seen_txid => TXID},
    BlockTime = qdate:to_date(BlockTimeSinceEpoch),

    % Inputs to the btc transaction means "out" of an account
    AddrIns = lists:filter(
        fun(#{<<"prevout">> := #{<<"scriptpubkey_address">> := PrevoutAddr}}) ->
            PrevoutAddr == Addr
        end,
        Vin
    ),

    OutTransactions = lists:filtermap(
        fun(AddrInfo) ->
            {Include, TXAttrs} = addrinfo_to_attrs(Addr, AddrInfo),
            TX = maps:merge(TXAttrs, #{
                source_id => TXID,
                line => <<"">>,
                datetime => BlockTime,
                type => undefined,
                symbol => <<"BTC">>,
                direction => out,
                description => <<"">>
            }),
            {Include, TX}
        end,
        AddrIns
    ),

    % Outputs to the BTC transaction means into an account
    AddrOuts = lists:filter(
        fun(#{<<"scriptpubkey_address">> := VoutAddr}) ->
            VoutAddr == Addr
        end,
        Vout
    ),

    InTransactions = lists:filtermap(
        fun(#{<<"value">> := Value, <<"scriptpubkey_address">> := TXAddr}) ->
            Include = TXAddr == Addr,
            TX = #{
                source_id => TXID,
                line => <<"">>,
                datetime => BlockTime,
                amount => sats_to_btc(Value),
                type => undefined,
                symbol => <<"BTC">>,
                direction => in,
                description => <<"">>
            },
            {Include, TX}
        end,
        AddrOuts
    ),

    ?LOG_DEBUG(#{
        what => blockstream_addr_ins,
        ins => AddrIns,
        outs => AddrOuts,
        tx_out => OutTransactions,
        tx_in => InTransactions
    }),
    {OutTransactions ++ InTransactions, State1}.

addrinfo_to_attrs(Addr, #{<<"value">> := Value, <<"scriptpubkey_address">> := TXAddr}) ->
    Include = TXAddr == Addr,
    Val = sats_to_btc(Value),
    {Include, #{amount => Val}};
addrinfo_to_attrs(Addr, #{
    <<"prevout">> := #{<<"value">> := Value, <<"scriptpubkey_address">> := TXAddr}
}) ->
    Include = TXAddr == Addr,
    Val = sats_to_btc(Value),
    {Include, #{amount => Val}}.

request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Blockstream request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://blockstream.info">>,
    Url = <<BasePath/binary, PathQS/binary>>,
    Headers = [],
    rate_limit(),
    ?LOG_INFO(#{
        message => blockstream_request,
        url => Url
    }),
    EF = fun() -> request(PathQS, Opts#{attempts_remaining => AR - 1}) end,
    folio_http:request(get, Url, Headers, [], EF).

rate_limit() ->
    folio_throttle:rate_limit(?MODULE, key).

derivation(Account, Index) ->
    AccountBin = erlang:integer_to_binary(Account),
    IndexBin = erlang:integer_to_binary(Index),
    <<<<"M/">>/binary, AccountBin/binary, <<"/">>/binary, IndexBin/binary>>.

pub_to_address(Pub, Derivation, p2sh) ->
    btcau:pub_to_p2wpkh_in_p2sh(Pub, Derivation);
pub_to_address(Pub, Derivation, p2pkh) ->
    btcau:pub_to_p2pkh(Pub, Derivation).

sats_to_balance(InSats, OutSats) ->
    In = sats_to_btc(InSats),
    Out = sats_to_btc(OutSats),

    Balance = decimal:sub(In, Out),
    Balance.

to_account(Addr, Balance) ->
    #{
        id => Addr,
        balances => [
            #{
                balance => Balance,
                symbol => <<"BTC">>
            }
        ]
    }.
