-module(folio_loopring).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-behavior(folio_integration).

-export([folio_init/0]).

-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = folio_throttle:setup(?MODULE, 1, 5500).

setup_properties() ->
    [
        #{
            fields => #{
                accountId => #{},
                apiKey => #{}
            }
        }
    ].

add(IntegrationID, #{apiKey := Key, accountId := AID}) ->
    Credentials = #{key => Key, account_id => AID},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{account_id := AID} = folio_credentials_store:get_credentials(IntegrationID),
    #{
        integration_id => IntegrationID,
        account_id => AID,
        tokens => undefined
    }.

accounts(State = #{tokens := undefined}) ->
    {ok, TokenList} = request_token_list(),

    ?LOG_INFO(#{
        message => loopring_tokens,
        tokens => TokenList
    }),
    TokenMap = lists:foldl(
        fun(T = #{<<"tokenId">> := TID}, M) ->
            M#{TID => T}
        end,
        #{},
        TokenList
    ),

    {incomplete, [], State#{tokens => TokenMap, next_account_id => 1}};
accounts(State = #{account_id := AccountID}) ->
    {ok, APIBalances} = request_account_balance(AccountID, State),

    ?LOG_INFO(#{
        message => loopring_balances,
        balances => APIBalances
    }),

    Balances = api_balances_to_balances(APIBalances, State),
    Acct = #{
        id => AccountID,
        balances => Balances
    },

    {complete, [Acct], State}.

api_balances_to_balances(APIB, _State = #{tokens := TokenMap}) ->
    lists:map(
        fun(#{<<"tokenId">> := TID, <<"total">> := Total}) ->
            #{
                <<"decimals">> := Decimals,
                <<"symbol">> := Symbol
            } = maps:get(TID, TokenMap),
            to_balance(Symbol, Total, Decimals)
        end,
        APIB
    ).

to_balance(Symbol, RawBalance, Decimal) ->
    Balance = to_value(RawBalance, Decimal),
    #{
        balance => Balance,
        symbol => Symbol
    }.

to_value(RawBalance, DecimalBin) when is_binary(DecimalBin) ->
    Decimal = erlang:binary_to_integer(DecimalBin),
    to_value(RawBalance, Decimal);
to_value(RawBalance, Decimal) ->
    NativeBalance = folio_math:to_decimal(RawBalance),
    Balance = folio_math:divide(NativeBalance, {1, Decimal}),
    Balance.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    State = #{
        account_id => AccountID,
        integration_id => IntegrationID,
        tokens => undefined,
        to_sync => [
            #{
                type => transfers,
                start => 0
            }
        ]
    },
    State.

account_transactions(State = #{to_sync := []}) ->
    {complete, [], State};
account_transactions(State = #{tokens := undefined}) ->
    {ok, TokenList} = request_token_list(),

    ?LOG_INFO(#{
        message => loopring_tokens,
        tokens => TokenList
    }),
    TokenMap = lists:foldl(
        fun(T = #{<<"tokenId">> := TID}, M) ->
            M#{TID => T}
        end,
        #{},
        TokenList
    ),

    {incomplete, [], State#{tokens => TokenMap}};
account_transactions(
    State = #{
        account_id := AccountID, to_sync := [#{type := transfers, start := Start} | RestToSync]
    }
) ->
    {ok, APITransfers} = request_account_transfers(AccountID, Start, State),

    ?LOG_INFO(#{
        message => loopring_transfers,
        balances => APITransfers
    }),
    TXLists = transfers_to_txs(APITransfers, State),
    TXs = lists:flatten(TXLists),

    {incomplete, TXs, State#{to_sync => RestToSync}}.

transfers_to_txs(
    #{<<"transactions">> := Transfers}, _State = #{account_id := AccountID, tokens := TokenMap}
) ->
    lists:map(
        fun(Transfer) ->
            transfer_to_txs(Transfer, AccountID, TokenMap)
        end,
        Transfers
    ).

transfer_to_txs(
    #{
        <<"timestamp">> := TimestampMS,
        <<"receiver">> := ReceivingAccountID,
        <<"hash">> := TXHash,
        <<"symbol">> := Symbol,
        <<"amount">> := Value,
        <<"memo">> := Description,
        <<"storageInfo">> := #{<<"tokenId">> := TokenID}
    },
    AccountID,
    TokenMap
) ->
    #{<<"decimals">> := Decimals} = maps:get(TokenID, TokenMap),
    Amount = to_value(Value, Decimals),
    Direction =
        case erlang:binary_to_integer(AccountID) == ReceivingAccountID of
            true -> in;
            false -> out
        end,
    [
        #{
            source_id => TXHash,
            datetime => qdate:to_date(trunc(TimestampMS / 1000)),
            direction => Direction,
            symbol => Symbol,
            amount => Amount,
            type => undefined,
            description => Description
        }
    ].

request_token_list() ->
    Path = <<"/api/v3/exchange/tokens">>,
    request(Path).

request_account_balance(AccountID, State) ->
    Path = <<<<"/api/v3/user/balances?accountId=">>/binary, AccountID/binary>>,
    request(Path, State).

request_account_transfers(AccountID, _StartInt, State) ->
    %Start = erlang:integer_to_binary(StartInt),
    Path =
        <<<<"/api/v3/user/transfers?accountId=">>/binary, AccountID/binary>>,

    request(Path, State).

-spec request(binary()) -> {ok, map() | list()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{}, #{attempts_remaining => 3}).

request(PathQS, State) ->
    request(PathQS, State, #{attempts_remaining => 3}).

-spec request(binary(), any(), map()) -> {ok, map() | list()} | {error, binary()}.
request(PathQS, _State, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Loopring request failed",
        path => PathQS
    }),
    {error, "No more attempts remaining"};
request(PathQS, State, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://api3.loopring.io">>,
    Url = <<BasePath/binary, PathQS/binary>>,

    Headers = state_to_headers(State),

    rate_limit(),
    ?LOG_INFO(#{
        message => loopring_request,
        url => Url
    }),
    case hackney:request(get, Url, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, State, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, jsx:decode(Body, [return_maps])};
                false -> {error, Body}
            end
    end.

state_to_headers(#{integration_id := IID}) ->
    #{key := Key} = folio_credentials_store:get_credentials(IID),
    [{<<"X-API-KEY">>, Key}];
state_to_headers(_State) ->
    [].

rate_limit() ->
    folio_throttle:rate_limit(?MODULE, key).
