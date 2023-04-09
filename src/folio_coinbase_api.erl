-module(folio_coinbase_api).

-include_lib("kernel/include/logger.hrl").

-behavior(folio_integration).

-export([folio_init/0]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    folio_throttle:setup(?MODULE, 5, per_second).

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
        {ok, _, _, Resp} ->
            User = maps:get(<<"data">>, Resp),
            {ok, User, State};
        Else ->
            Else
    end.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{
        next_uri => <<"/v2/accounts?limit=100">>,
        integration_id => IntegrationID
    }.

accounts(State = #{next_uri := NextURI}) ->
    {ok, _, _, AccountResp} = request(NextURI, State),

    Accounts = maps:get(<<"data">>, AccountResp),

    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % Determine if this is complete
    Complete =
        case NextNextURI of
            null -> complete;
            _ -> incomplete
        end,

    State1 = State#{next_uri => NextNextURI},
    FAccounts = lists:map(fun cb_to_account/1, Accounts),
    {Complete, FAccounts, State1}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    State = #{
        account_id => AccountID,
        integration_id => IntegrationID,
        next_uri => transaction_path(AccountID)
    },
    State.

account_transactions(State = #{next_uri := NextURI}) ->
    {ok, _, _, Resp} = request(NextURI, State),
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
    State1 = State#{next_uri => NextNextURI},
    {Complete, FolioTXs, State1}.

cb_to_account(#{
    <<"id">> := SourceID,
    <<"balance">> := #{<<"amount">> := BalanceF, <<"currency">> := SourceSymbol}
}) ->
    Balance = folio_math:to_decimal(BalanceF),
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
            amount => decimal:abs(folio_math:to_decimal(Amount)),
            symbol => Symbol,
            type => undefined
        })
    ].

transaction_path(AccountId) ->
    <<<<"/v2/accounts/">>/binary, AccountId/binary, <<"/transactions">>/binary>>.

coinbase_credentials(_State = #{integration_id := ID}) ->
    #{key := Key, secret := Secret} = folio_credentials_store:get_credentials(ID),
    {Key, Secret}.

request(PathQS, State) ->
    request(PathQS, #{attempts_remaining => 3}, State).

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

    EF = fun() -> request(PathQS, Opts#{attempts_remaining => AR - 1}, State) end,
    folio_http:request(get, Url, Headers, [], EF).

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
    folio_throttle:rate_limit(?MODULE, key).
