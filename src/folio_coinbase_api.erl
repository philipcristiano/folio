-module(folio_coinbase_api).

-export([accounts/0]).
-export([user/0]).

user() ->
    {ok, UserResp} = request(<<"/v2/user">>),
    User = maps:get(<<"data">>, UserResp),
    io:format("User ~p~n", [User]),
    {ok, User}.

accounts() ->
    {ok, AccountResp} = request(<<"/v2/accounts">>),
    Accounts = maps:get(<<"data">>, AccountResp),
    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    StartingAfter = maps:get(<<"next_starting_after">>, Pagination, undefined),
    io:format("Accounts ~p~n", [{Accounts, StartingAfter}]),
    {ok, Accounts ++ accounts(StartingAfter)}.

accounts(undefined) ->
    [];
accounts(null) ->
    [];

accounts(StartingAfter) ->
    {ok, AccountResp} = request(<<"/v2/accounts">>, [{<<"starting_after">>, StartingAfter}]),
    Accounts = maps:get(<<"data">>, AccountResp),
    Pagination = maps:get(<<"pagination">>, AccountResp, #{}),
    NextStartingAfter = maps:get(<<"next_starting_after">>, Pagination, undefined),
    io:format("Accounts ~p~n", [StartingAfter]),
    Accounts ++ accounts(NextStartingAfter).

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
    io:format("Resp ~p~n", [{RespCode, RespHeaders}]),
    {ok, Body} = hackney:body(ClientRef),
    ParsedBody =
        case hackney_headers:header_value(<<"Content-Type">>, RespHeaders) of
            <<"application/json; charset=utf-8">> ->
                jsx:decode(Body, [return_maps]);
            _ ->
                jsx:decode(Body, [return_maps])
        end,
    io:format("Resp ~p~n", [{ParsedBody}]),
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

    io:format("K S ~p~n", [{Key, Secret}]),
    SigMesg = <<NowBin/binary, <<"GET">>/binary, Path/binary>>,
    io:format("SigMesg ~p~n", [SigMesg]),

    SigA = hmac:hexlify(hmac:hmac256(Secret, SigMesg)),
    io:format("Sig A ~p~n", [SigA]),
    SigB = hmac:hexlify(crypto:mac(hmac, sha256, Secret, SigMesg)),
    io:format("Sig B ~p~n", [SigB]),

    Sig = string:lowercase(SigA),

    Headers = [
        {<<"CB-ACCESS-KEY">>, Key},
        {<<"CB-ACCESS-TIMESTAMP">>, NowBin},
        {<<"CB-ACCESS-SIGN">>, Sig},
        {<<"User-Agent">>, <<"folio">>},
        {<<"CB-VERSION">>, <<"2022-05-20">>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Headers.
