-module(folio_ethplorer).

-include_lib("kernel/include/logger.hrl").
-include_lib("decimal/include/decimal.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(folio_account_provider).

-define(DOPTS, #{precision => 10, rounding => round_floor}).

-export([folio_init/0]).

-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    throttle:setup(?MODULE, 1, per_second).

setup_properties() ->
    [
        #{
            fields => #{
                address => #{}
            }
        }
    ].

add(IntegrationID, #{address := Addr}) ->
    Credentials = #{address => Addr},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

accounts_init(Integration = #{id := IntegrationID}) ->
    Creds = folio_credentials_store:get_credentials(IntegrationID),
    accounts_init_by_credentials(Integration, Creds).

accounts_init_by_credentials(_Integration = #{id := IntegrationID}, #{address := Addr}) ->
    #{
        address => Addr,
        integration_id => IntegrationID
    }.

accounts(State = #{address := Addr}) ->
    {ok,
        #{
            <<"ETH">> := #{
                <<"rawBalance">> := EthRawBalance
            }
        } = Resp} = request_balance(Addr),
    Tokens = maps:get(<<"tokens">>, Resp, []),

    EthBalance = to_balance(<<"native">>, <<"ETH">>, EthRawBalance, 18),

    TokenBalances = lists:map(
        fun(
            #{
                <<"tokenInfo">> := #{
                    <<"address">> := TokenAddr, <<"symbol">> := Symbol, <<"decimals">> := Decimals
                },
                <<"rawBalance">> := TokenRawBalance
            }
        ) ->
            to_balance(TokenAddr, Symbol, TokenRawBalance, Decimals)
        end,
        Tokens
    ),

    Accounts = [to_account(Addr, [EthBalance | TokenBalances])],

    {complete, Accounts, State}.

to_balance(TokenAddr, Symbol, RawBalance, Decimal) ->
    Balance = to_value(RawBalance, Decimal),
    #{
        balance => Balance,
        asset => #{symbol => Symbol, id => TokenAddr}
    }.

to_value(RawBalance, DecimalBin) when is_binary(DecimalBin) ->
    Decimal = erlang:binary_to_integer(DecimalBin),
    to_value(RawBalance, Decimal);
to_value(RawBalance, Decimal) ->
    NativeBalance = folio_math:to_decimal(RawBalance),
    Balance = folio_math:divide(NativeBalance, {1, Decimal}),
    Balance.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    % #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    State = #{
        account_id => AccountID,
        address => AccountID,
        integration_id => IntegrationID,
        to_sync => [
            #{
                type => get_address_history
            },
            #{
                type => get_address_tx
            }
        ]
    },
    State.

account_transactions(State = #{to_sync := []}) ->
    {complete, [], State};
account_transactions(
    State = #{
        address := Addr, to_sync := [#{type := get_address_history} | RestToSync]
    }
) ->
    {ok, #{<<"operations">> := Ops}} = request_address_history(Addr),

    TXLists = lists:map(
        fun(Op) ->
            op_to_txs(Addr, Op)
        end,
        Ops
    ),
    TXs = lists:flatten(TXLists),

    {incomplete, TXs, State#{to_sync => RestToSync}};
account_transactions(
    State = #{
        address := Addr, to_sync := [#{type := get_address_tx} | RestToSync]
    }
) ->
    {ok, APITXs} = request_address_transactions(Addr),

    TXLists = lists:map(
        fun(Op) ->
            api_tx_to_txs(Addr, Op)
        end,
        APITXs
    ),
    TXs = lists:flatten(TXLists),

    {incomplete, TXs, State#{to_sync => RestToSync}}.

op_to_txs(Addr, #{
    <<"timestamp">> := Timestamp,
    <<"transactionHash">> := TXHash,
    <<"tokenInfo">> := #{
        <<"address">> := TokenAddr, <<"symbol">> := Symbol, <<"decimals">> := Decimals
    },
    <<"type">> := <<"transfer">> = Type,
    <<"value">> := Value,
    <<"from">> := From,
    <<"to">> := _To
}) ->
    DValue = to_value(Value, Decimals),
    Direction =
        case string:lowercase(Addr) == From of
            true -> out;
            false -> in
        end,
    [
        #{
            source_id => TXHash,
            line => <<"">>,
            datetime => qdate:to_date(Timestamp),
            direction => Direction,
            asset => #{symbol => Symbol, id => TokenAddr},
            amount => DValue,
            type => undefined,
            description => Type
        }
    ].

api_tx_to_txs(Addr, #{
    <<"timestamp">> := Timestamp,
    <<"hash">> := TXHash,
    <<"value">> := Value,
    <<"from">> := From,
    <<"to">> := _To
}) ->
    DValue = folio_math:to_decimal(Value),
    Direction =
        case string:lowercase(Addr) == From of
            true -> out;
            false -> in
        end,
    [
        #{
            source_id => TXHash,
            line => <<"">>,
            datetime => qdate:to_date(Timestamp),
            direction => Direction,
            asset => #{symbol => <<"ETH">>, id => <<"native">>},
            amount => DValue,
            type => undefined,
            description => <<"">>
        }
    ].

request_balance(Address) when is_binary(Address) ->
    Path = <<<<"/getAddressInfo/">>/binary, Address/binary, <<"?apiKey=freekey">>/binary>>,
    {ok, _, _, D} = request(Path),
    {ok, D}.

request_address_history(Address) ->
    Path =
        <<<<"/getAddressHistory/">>/binary, Address/binary,
            <<"?apiKey=freekey&limit=1000">>/binary>>,
    {ok, _, _, D} = request(Path),
    {ok, D}.

request_address_transactions(Address) ->
    Path =
        <<<<"/getAddressTransactions/">>/binary, Address/binary,
            <<"?apiKey=freekey&limit=1000">>/binary>>,
    {ok, _, _, D} = request(Path),
    {ok, D}.

request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Blockstream request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://api.ethplorer.io">>,
    Url = <<BasePath/binary, PathQS/binary>>,
    Headers = [],
    rate_limit(),
    ?LOG_INFO(#{
        message => ethplorer_request,
        url => Url
    }),
    EF = fun() -> request(PathQS, Opts#{attempts_remaining => AR - 1}) end,
    folio_http:request(get, Url, Headers, [], EF).

rate_limit() ->
    folio_throttle:rate_limit(?MODULE, key).

to_account(Addr, Balances) ->
    #{
        id => Addr,
        balances => Balances
    }.
