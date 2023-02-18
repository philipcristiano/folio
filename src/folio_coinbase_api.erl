-module(folio_coinbase_api).

-include_lib("kernel/include/logger.hrl").

-behavior(folio_exchange_integration).

-export([folio_init/0]).
-export([accounts_init/0, accounts/1]).
-export([account_transactions_init/1, account_transactions/1]).
-export([user/0]).

folio_init() ->
    ok = throttle:setup(?MODULE, 10, per_second),
    ok.

user() ->
    case request(<<"/v2/user">>) of
        {ok, Resp} ->
            User = maps:get(<<"data">>, Resp),
            {ok, User};
        Else ->
            Else
    end.

accounts_init() ->
    #{next_uri => <<"/v2/accounts?limit=100">>}.

-type sync_completeness() :: complete | incomplete.
-spec accounts(any()) -> {sync_completeness(), list(), map()}.
accounts(State = #{next_uri := NextURI}) ->
    {ok, AccountResp} = request(NextURI),

    Accounts = maps:get(<<"data">>, AccountResp),

    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % Determine if this is complete
    Complete =
        case NextNextURI of
            null -> complete;
            _ -> incomplete
        end,

    NewState = State#{next_uri => NextNextURI},
    FAccounts = lists:map(fun cb_to_account/1, Accounts),
    {Complete, FAccounts, NewState}.

account_transactions_init(#{id := AccountID}) ->
    State = #{
        account_id => AccountID,
        next_uri => transaction_path(AccountID)
    },
    State.

account_transactions(State = #{next_uri := NextURI}) ->
    {ok, Resp} = request(NextURI),
    Data = maps:get(<<"data">>, Resp),

    Pagination = maps:get(<<"pagination">>, Resp, #{}),
    NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % Determine if this is complete
    Complete =
        case NextNextURI of
            null -> complete;
            _ -> incomplete
        end,
    FolioTXs = lists:map(fun cb_to_tx/1, Data),
    NewState = State#{next_uri => NextNextURI},
    {Complete, FolioTXs, NewState}.

cb_to_account(#{
    <<"id">> := SourceID,
    <<"balance">> := #{<<"amount">> := Balance, <<"currency">> := SourceSymbol}
}) ->
    LBalanace = binary_to_list(Balance),
    FBalance = list_to_float(LBalanace),
    #{
        id => SourceID,
        balance => FBalance,
        symbol => SourceSymbol
    }.

cb_to_tx(#{<<"type">> := <<"buy">>, <<"created_at">> := CreatedAt, <<"id">> := SourceID}) ->
    #{
        datetime => CreatedAt,
        source_id => SourceID,
        type => buy
    };
cb_to_tx(#{
    <<"type">> := <<"exchange_deposit">>,
    <<"created_at">> := CreatedAt,
    <<"id">> := SourceID
}) ->
    #{
        datetime => CreatedAt,
        source_id => SourceID,
        type => deposit
    };
cb_to_tx(#{<<"type">> := <<"pro_deposit">>, <<"created_at">> := CreatedAt, <<"id">> := SourceID}) ->
    #{
        datetime => CreatedAt,
        source_id => SourceID,
        type => deposit
    };
cb_to_tx(#{<<"type">> := <<"pro_withdrawal">>, <<"created_at">> := CreatedAt, <<"id">> := SourceID}) ->
    #{
        datetime => CreatedAt,
        source_id => SourceID,
        type => withdraw
    };
cb_to_tx(#{<<"type">> := <<"send">>, <<"created_at">> := CreatedAt, <<"id">> := SourceID}) ->
    #{
        datetime => CreatedAt,
        source_id => SourceID,
        type => send
    }.

transaction_path(AccountId) ->
    <<<<"/v2/accounts/">>/binary, AccountId/binary, <<"/transactions">>/binary>>.

coinbase_credentials() ->
    #{key := Key, secret := Secret} = folio_credentials_store:get_credentials(coinbase),
    {Key, Secret}.

-spec request(binary()) -> {ok, map()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

-spec request(binary(), map()) -> {ok, map()} | {error, binary()}.
request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Coinbase request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://api.coinbase.com">>,

    Url = <<BasePath/binary, PathQS/binary>>,

    rate_limit(),
    % coinbase_sign requires a timestamp so rate limiting before it can cause
    % signing errors if the request is delayed too long
    Headers = coinbase_sign(get, PathQS),

    case hackney:request(get, Url, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, jsx:decode(Body, [return_maps])};
                false -> {error, Body}
            end
    end.

coinbase_sign(get, Path) ->
    Now = os:system_time(second),

    {Key, Secret} = coinbase_credentials(),

    NowBin = erlang:integer_to_binary(Now),

    SigMesg = <<NowBin/binary, <<"GET">>/binary, Path/binary>>,
    % SigUpper = hmac:hexlify(crypto:mac(hmac, sha256, Secret, SigMesg)),
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
