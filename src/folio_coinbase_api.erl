-module(folio_coinbase_api).

-include_lib("kernel/include/logger.hrl").

-export([folio_init/0]).
-export([accounts_init/0, accounts/1]).
-export([run/1]).
-export([transactions/2]).
-export([user/0]).

folio_init() ->
    ok = throttle:setup(?MODULE, 10, per_second),
    ok.

run(Callback) ->
    case user() of
        {error, _} -> ok;
        {ok, User} -> Callback({user, User})
    end,
    F = fun({accounts, CallbackAccounts}) ->
        Callback({accounts, CallbackAccounts}),

        ?LOG_DEBUG(#{
            what => "Can sync transactions here"
        }),
        H = fun(Account) ->
            erlang:spawn(folio_coinbase_api, transactions, [Account, Callback])
        end,
        lists:foreach(H, CallbackAccounts)
    end,
    accounts(F).

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

-spec accounts(any()) -> {complete, list()} | {incomplete, list()}.
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

-spec transactions(map(), function()) -> ok.
transactions(Account = #{id := AccountID}, Callback) when is_map(Account) ->
    transactions(AccountID, Callback, <<"">>).

-spec transactions(binary(), function(), atom | binary()) -> ok.
transactions(_AccountID, Callback, StartingAfternull) when is_atom(StartingAfternull) ->
    Callback({transactions, complete}),
    ok;
transactions(AccountID, Callback, StartingAfter) when is_binary(StartingAfter) ->
    Headers =
        case StartingAfter of
            <<"">> -> [limit_header()];
            Else -> [limit_header(), {<<"starting_after">>, Else}]
        end,
    Path = transaction_path(AccountID),
    {ok, Resp} = request(Path, Headers),
    Pagination = maps:get(<<"pagination">>, Resp, #{}),
    NextStartingAfter = maps:get(<<"next_starting_after">>, Pagination, undefined),
    Data = maps:get(<<"data">>, Resp),

    % ?LOG_INFO(#{
    %     what => "Transactions",
    %     transactions => Data
    % }),
    ok =
        case Data of
            [] ->
                ok;
            _ ->
                FolioTXs = lists:map(fun cb_to_tx/1, Data),
                Callback({transactions, FolioTXs})
        end,
    transactions(AccountID, Callback, NextStartingAfter).

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
cb_to_tx(#{<<"type">> := <<"pro_withdraw">>, <<"created_at">> := CreatedAt, <<"id">> := SourceID}) ->
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
    {ok, AppConfig} = application:get_env(folio, credentials),
    CoinbaseConfig = maps:get(coinbase, AppConfig, #{}),
    Key = maps:get(key, CoinbaseConfig),
    Sec = maps:get(secret, CoinbaseConfig),

    {Key, Sec}.

-spec request(binary()) -> {ok, map()} | {error, binary()}.
-spec request(binary(), list()) -> {ok, map()} | {error, binary()}.

request(Path, QParams) ->
    QueryString =
        case hackney_url:qs(QParams) of
            <<>> -> <<>>;
            QS -> <<<<"?">>/binary, QS/binary>>
        end,
    PathQS = <<Path/binary, QueryString/binary>>,
    request(PathQS).
request(PathQS) ->
    BasePath = <<"https://api.coinbase.com">>,

    Url = <<BasePath/binary, PathQS/binary>>,

    rate_limit(),
    % coinbase_sign requires a timestamp so rate limiting before it can cause
    % signing errors if the request is delayed too long
    Headers = coinbase_sign(get, PathQS),
    io:format("Headers ~p~n", [{PathQS, Headers}]),

    {ok, _RespCode, _RespHeaders, Body} = hackney:request(get, Url, Headers, [], [with_body]),

    case jsx:is_json(Body) of
        true -> {ok, jsx:decode(Body, [return_maps])};
        false -> {error, Body}
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

limit_header() ->
    limit_header(100).

limit_header(N) ->
    {<<"limit">>, integer_to_binary(N)}.

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
