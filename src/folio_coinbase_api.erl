-module(folio_coinbase_api).

-include_lib("kernel/include/logger.hrl").

-behavior(folio_integration).

-export([folio_init/0]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 5, per_second),
    ok.

setup_properties() ->
    [
        #{
            fields => #{
                key => #{},
                secret => #{}
            }
        }
    ].

add(IntegrationID, #{key := K, secret := S}) ->
    Credentials = #{key => K, secret => S},
    State = #{integration_id => IntegrationID},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    {ok, _, _State} = user(State),
    ok.

user(State) ->
    case request(<<"/v2/user">>, State) of
        {ok, Resp, State1} ->
            User = maps:get(<<"data">>, Resp),
            {ok, User, State1};
        Else ->
            Else
    end.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{
        next_uri => <<"/v2/accounts?limit=100">>,
        integration_id => IntegrationID
    }.

accounts(State = #{next_uri := NextURI}) ->
    {ok, AccountResp, State1} = request(NextURI, State),

    Accounts = maps:get(<<"data">>, AccountResp),

    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % Determine if this is complete
    Complete =
        case NextNextURI of
            null -> complete;
            _ -> incomplete
        end,

    State2 = State1#{next_uri => NextNextURI},
    FAccounts = lists:map(fun cb_to_account/1, Accounts),
    {Complete, FAccounts, State2}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    State = #{
        account_id => AccountID,
        integration_id => IntegrationID,
        next_uri => transaction_path(AccountID)
    },
    State.

account_transactions(State = #{next_uri := NextURI}) ->
    {ok, Resp, State1} = request(NextURI, State),
    Data = maps:get(<<"data">>, Resp),

    Pagination = maps:get(<<"pagination">>, Resp, #{}),
    NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % Determine if this is complete
    Complete =
        case NextNextURI of
            null -> complete;
            _ -> incomplete
        end,
    FolioTXs = lists:flatten(lists:map(fun cb_to_tx/1, Data)),
    State2 = State1#{next_uri => NextNextURI},
    {Complete, FolioTXs, State2}.

cb_to_account(#{
    <<"id">> := SourceID,
    <<"balance">> := #{<<"amount">> := BalanceF, <<"currency">> := SourceSymbol}
}) ->
    Balance = to_decimal(BalanceF),
    #{
        id => SourceID,
        balances => [
            #{
                balance => Balance,
                symbol => SourceSymbol
            }
        ]
    }.

% https://docs.cloud.coinbase.com/sign-in-with-coinbase/docs/api-transactions
-spec type_to_partial_transaction(binary()) -> map().
type_to_partial_transaction(T = <<"buy">>) ->
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"staking_reward">>) ->
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"send">>) ->
    #{
        direction => out,
        description => T
    };
type_to_partial_transaction(T = <<"fiat_deposit">>) ->
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"fiat_withdrawal">>) ->
    #{
        direction => out,
        description => T
    };
type_to_partial_transaction(T = <<"exchange_deposit">>) ->
    % Deposit money into Coinbase Pro
    #{
        direction => out,
        description => T
    };
type_to_partial_transaction(T = <<"exchange_withdrawal">>) ->
    % Withdraw money from Coinbase Pro
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"inflation_reward">>) ->
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"pro_deposit">>) ->
    #{
        direction => in,
        description => T
    };
type_to_partial_transaction(T = <<"pro_withdrawal">>) ->
    #{
        direction => out,
        description => T
    }.

cb_to_tx(
    CB = #{
        <<"type">> := Type,
        <<"created_at">> := CreatedAt,
        <<"id">> := SourceID,
        <<"amount">> := #{<<"amount">> := Amount, <<"currency">> := Symbol}
    }
) ->
    Partial = type_to_partial_transaction(Type),
    DT = qdate:to_date(CreatedAt),
    ?LOG_DEBUG(#{
        message => coinbase_transaction,
        cb => CB
    }),
    [
        maps:merge(Partial, #{
            datetime => DT,
            source_id => SourceID,
            amount => decimal:abs(to_decimal(Amount)),
            symbol => Symbol,
            type => undefined
        })
    ].

transaction_path(AccountId) ->
    <<<<"/v2/accounts/">>/binary, AccountId/binary, <<"/transactions">>/binary>>.

coinbase_credentials(_State = #{integration_id := ID}) ->
    #{key := Key, secret := Secret} = folio_credentials_store:get_credentials(ID),
    {Key, Secret}.

-spec request(binary(), any()) -> {ok, map(), any()} | {error, binary(), any()}.
request(PathQS, State) ->
    request(PathQS, #{attempts_remaining => 3}, State).

-spec request(binary(), map(), any()) -> {ok, map(), any()} | {error, binary(), any()}.
request(PathQS, #{attempts_remaining := AR}, State) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Coinbase request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining", State};
request(PathQS, Opts = #{attempts_remaining := AR}, State) ->
    BasePath = <<"https://api.coinbase.com">>,
    Url = <<BasePath/binary, PathQS/binary>>,

    rate_limit(),
    % coinbase_sign requires a timestamp so rate limiting before it can cause
    % signing errors if the request is delayed too long
    Headers = coinbase_sign(get, PathQS, State),

    case hackney:request(get, Url, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1}, State);
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, jsx:decode(Body, [return_maps]), State};
                false -> {error, Body, State}
            end
    end.

coinbase_sign(get, Path, State) ->
    Now = os:system_time(second),

    {Key, Secret} = coinbase_credentials(State),

    NowBin = erlang:integer_to_binary(Now),

    SigMesg = <<NowBin/binary, <<"GET">>/binary, Path/binary>>,
    SigUpper = binary:encode_hex(crypto:mac(hmac, sha256, Secret, SigMesg)),
    Sig = string:lowercase(SigUpper),

    Headers = [
        {<<"CB-ACCESS-KEY">>, Key},
        {<<"CB-ACCESS-TIMESTAMP">>, NowBin},
        {<<"CB-ACCESS-SIGN">>, Sig},
        {<<"User-Agent">>, <<"folio">>},
        {<<"CB-VERSION">>, <<"2022-05-20">>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Headers.

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

to_decimal(F) when is_binary(F) ->
    L = size(F),
    decimal:to_decimal(F, #{precision => L, rounding => round_floor}).
