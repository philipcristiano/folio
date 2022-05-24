-module(folio_coinbase_api).

-include_lib("kernel/include/logger.hrl").
-export([accounts/1]).
-export([run/1]).
-export([transactions/2]).
-export([user/0]).

run(Callback) ->
    {ok, User} = user(),
    Callback({user, User}),
    F = fun({accounts, CallbackAccounts}) ->
        Callback({accounts, CallbackAccounts}),
        H = fun(Account) ->
            erlang:spawn(folio_coinbase_api, transactions, [Account, Callback])
        end,
        lists:foreach(H, CallbackAccounts)
    end,
    accounts(F),
    transactions(<<"foo">>, Callback).

user() ->
    {ok, UserResp} = request(<<"/v2/user">>),
    User = maps:get(<<"data">>, UserResp),
    {ok, User}.

accounts(Callback) ->
    {ok, AccountResp} = request(<<"/v2/accounts">>, [limit_header()]),
    Accounts = maps:get(<<"data">>, AccountResp),
    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    StartingAfter = maps:get(<<"next_starting_after">>, Pagination, null),
    Callback({accounts, Accounts}),
    accounts(Callback, StartingAfter).

accounts(_Callback, null) ->
    [];
accounts(Callback, StartingAfter) ->
    {ok, AccountResp} = request(<<"/v2/accounts">>, [limit_header(), {<<"starting_after">>, StartingAfter}]),
    Accounts = maps:get(<<"data">>, AccountResp),
    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    NextStartingAfter = maps:get(<<"next_starting_after">>, Pagination, undefined),
    io:format("Accounts ~p~n", [StartingAfter]),
    Callback({accounts, Accounts}),
    accounts(Callback, NextStartingAfter).

transactions(#{<<"id">> := AccountID}, Callback) ->
    transactions(AccountID, Callback, start).
transactions(_AccountID, _Callback, null) ->
    [];
transactions(AccountID, Callback, StartingAfter) ->
    Headers = case StartingAfter of
        start -> [limit_header()];
        Else -> [limit_header(),{<<"starting_after">>, Else}]
    end,
    Path = transaction_path(AccountID),
    {ok, Resp} = request(Path, Headers),
    Pagination = maps:get(<<"pagination">>, Resp, #{}),
    NextStartingAfter = maps:get(<<"next_starting_after">>, Pagination, undefined),
    Data = maps:get(<<"data">>, Resp),

    ?LOG_INFO(#{
        what => "Transactions",
        transactions => Data
    }),
    ok = case Data of
        [] -> ok;
        _ -> Callback({transactions, Data})
    end,
    transactions(AccountID, Callback, NextStartingAfter).


transaction_path(AccountId) ->
    << <<"/v2/accounts/">>/binary, AccountId/binary, <<"/transactions">>/binary >>.

coinbase_credentials() ->
    {ok, AppConfig} = application:get_env(folio, credentials),
    CoinbaseConfig = maps:get(coinbase, AppConfig, #{}),
    Key = maps:get(key, CoinbaseConfig),
    Sec = maps:get(secret, CoinbaseConfig),

    {Key, Sec}.

request(Path) ->
    request(Path, []).

request(Path, QParams) ->
    BasePath = <<"https://api.coinbase.com">>,
    QueryString =
        case hackney_url:qs(QParams) of
            <<>> -> <<>>;
            QS -> <<<<"?">>/binary, QS/binary>>
        end,
    PathQS = <<Path/binary, QueryString/binary>>,

    Url = <<BasePath/binary, PathQS/binary>>,
    Headers = coinbase_sign(get, PathQS),

    {ok, RespCode, RespHeaders, ClientRef} = hackney:request(get, Url, Headers, [], []),
    {ok, Body} = hackney:body(ClientRef),
    ParsedBody =
        case hackney_headers:header_value(<<"Content-Type">>, RespHeaders) of
            <<"application/json; charset=utf-8">> ->
                jsx:decode(Body, [return_maps]);
            _ ->
                jsx:decode(Body, [return_maps])
        end,
    RespStatus = case RespCode of
        200 -> ok;
        _ -> error
    end,
    {RespStatus, ParsedBody}.

coinbase_sign(get, Path) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    Now = MegaSecs * 1000000 + Secs,

    {Key, Secret} = coinbase_credentials(),

    NowBin = erlang:integer_to_binary(Now),

    SigMesg = <<NowBin/binary, <<"GET">>/binary, Path/binary>>,
    SigUpper = hmac:hexlify(crypto:mac(hmac, sha256, Secret, SigMesg)),
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
    {<<"limit">>, N}.
