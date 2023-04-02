-module(folio_loopring).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-behavior(folio_integration).

-export([folio_init/0]).

-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 1, 5500),
    ok.

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
        message => loopring_Balances,
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
        integration_id => IntegrationID
    },
    State.

account_transactions(State) ->
    {complete, [], State}.

%op_to_txs(Addr, #{
%    <<"timestamp">> := Timestamp,
%    <<"transactionHash">> := TXHash,
%    <<"tokenInfo">> := #{<<"symbol">> := Symbol, <<"decimals">> := Decimals},
%    <<"type">> := <<"transfer">> = Type,
%    <<"value">> := Value,
%    <<"from">> := From,
%    <<"to">> := _To
%}) ->
%    DValue = to_value(Value, Decimals),
%    Direction =
%        case string:lowercase(Addr) == From of
%            true -> out;
%            false -> in
%        end,
%    [
%        #{
%            source_id => TXHash,
%            datetime => qdate:to_date(Timestamp),
%            direction => Direction,
%            symbol => Symbol,
%            amount => DValue,
%            type => undefined,
%            description => Type
%        }
%    ].
%
%api_tx_to_txs(Addr, #{
%    <<"timestamp">> := Timestamp,
%    <<"hash">> := TXHash,
%    <<"value">> := Value,
%    <<"from">> := From,
%    <<"to">> := _To
%}) ->
%    DValue = folio_math:to_decimal(Value),
%    Direction =
%        case string:lowercase(Addr) == From of
%            true -> out;
%            false -> in
%        end,
%    [
%        #{
%            source_id => TXHash,
%            datetime => qdate:to_date(Timestamp),
%            direction => Direction,
%            symbol => <<"ETH">>,
%            amount => DValue,
%            type => undefined,
%            description => <<"">>
%        }
%    ].

request_token_list() ->
    Path = <<"/api/v3/exchange/tokens">>,
    request(Path).

request_account_balance(AccountID, State) ->
    Path = <<<<"/api/v3/user/balances?accountId=">>/binary, AccountID/binary>>,

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
