-module(folio_blockstream).

-include_lib("kernel/include/logger.hrl").
-include_lib("decimal/include/decimal.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-behavior(folio_integration).

-define(DOPTS, #{precision => 10, rounding => round_floor}).

-export([folio_init/0]).

-export([balance/1]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 10, per_second),
    ok.

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
                derivation => #{},
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

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    #{
        address => Addr,
        integration_id => IntegrationID
    }.

-spec sats_to_btc(integer()) -> decimal:decimal().
sats_to_btc(Sats) ->
    SatD = decimal:to_decimal(Sats, #{precision => 10, rounding => round_floor}),
    SatsPerBTC = decimal:to_decimal(100000000, ?DOPTS),
    decimal:divide(SatD, SatsPerBTC, ?DOPTS).

accounts(State = #{address := Addr}) ->
    {ok, #{
        <<"chain_stats">> := #{
            <<"funded_txo_sum">> := InSats,
            <<"spent_txo_sum">> := OutSats
        }
    }} = balance(Addr),

    In = sats_to_btc(InSats),
    Out = sats_to_btc(OutSats),

    % Blockstream returns in sats, divide to get whole BTC.
    Balance = decimal:sub(In, Out),

    Accounts = [
        #{
            id => Addr,
            balances => [
                #{
                    balance => Balance,
                    symbol => <<"BTC">>
                }
            ]
        }
    ],
    {complete, Accounts, State}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    State = #{
        account_id => AccountID,
        address => Addr,
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
    {ok, D} = request(Path),
    {ok, D}.

transactions(State = #{address := Address, last_seen_txid := LSTXID}) ->
    Path = <<<<"/api/address/">>/binary, Address/binary, <<"/txs/chain/">>/binary, LSTXID/binary>>,
    {ok, D} = request(Path),
    {Transactions, State1} = blockstream_txs_to_transactions(D, State),
    io:format("RM tx ~p~n", [Transactions]),
    {lists:flatten(Transactions), State1}.

blockstream_txs_to_transactions(Data, State) ->
    lists:foldl(
        fun(TX, {ExistingTXs, AccState}) ->
            {NewTXs, AccState1} = blockstream_tx_to_transactions(TX, AccState),
            io:format("New TX ~p~n", [NewTXs]),
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

    ?LOG_INFO(#{
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

-spec request(binary()) -> {ok, map() | list()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

-spec request(binary(), map()) -> {ok, map() | list()} | {error, binary()}.
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
    case hackney:request(get, Url, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, jsx:decode(Body, [return_maps])};
                false -> {error, Body}
            end
    end.

rate_limit() ->
    case throttle:check(?MODULE, key) of
        {ok, _RemainingAttempts, _TimeToReset} ->
            ok;
        {limit_exceeded, _, TimeToReset} ->
            ChosenTime = time_to_reset(TimeToReset),
            ?LOG_DEBUG(#{
                message => "Rate limit would be exceeded",
                time_to_reset => TimeToReset,
                time_to_sleep => ChosenTime,
                pid => self()
            }),
            timer:sleep(ChosenTime),
            rate_limit()
    end.

time_to_reset(I) when I < 2 ->
    rand:uniform(30);
time_to_reset(N) ->
    N.
