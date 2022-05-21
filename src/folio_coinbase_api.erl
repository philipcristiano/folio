-module(folio_coinbase_api).

-export([run/0]).

run() ->
    {ok, User} = request(<<"/v2/user">>),
    io:format("User ~p~n", [User]),
    {ok, User}.

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
    QueryString = case hackney_url:qs(QParams) of
        <<>> -> <<>>;
        QS -> << <<"?">>/binary, QS/binary >>
    end,
    PathQS = << Path/binary, QueryString/binary >>,

    Url = << BasePath/binary, PathQS/binary >>,
    Headers = coinbase_sign(get, PathQS),

    {ok, RespCode, RespHeaders, ClientRef} = hackney:request(get, Url, Headers, [], []),
    io:format("Resp ~p~n", [{RespCode, RespHeaders}]),
    {ok, Body} = hackney:body(ClientRef),
    ParsedBody = case hackney_headers:header_value(<<"Content-Type">>, RespHeaders) of
        <<"application/json; charset=utf-8">> ->
            jsx:decode(Body, [return_maps]);
        _ ->
            jsx:decode(Body, [return_maps])
    end,
    User = maps:get(<<"data">>, ParsedBody),
    {ok, User}.

coinbase_sign(get, Path) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    Now = MegaSecs * 1000000 + Secs,

    {Key, Secret} = coinbase_credentials(),

    NowBin = erlang:integer_to_binary(Now),

    io:format("K S ~p~n", [{Key, Secret}]),
    SigMesg = << NowBin/binary, <<"GET">>/binary, Path/binary >>,
    io:format("SigMesg ~p~n", [SigMesg]),

    SigA = hmac:hexlify(hmac:hmac256(Secret, SigMesg)),
    io:format("Sig A ~p~n", [SigA]),
    SigB = hmac:hexlify(crypto:mac(hmac, sha256, Secret, SigMesg)),
    io:format("Sig B ~p~n", [SigB]),

    Sig = string:lowercase(SigA),

    Headers = [{<<"CB-ACCESS-KEY">>, Key},
               {<<"CB-ACCESS-TIMESTAMP">>, NowBin},
               {<<"CB-ACCESS-SIGN">>, Sig},
               {<<"User-Agent">>, <<"folio">>},
               {<<"CB-VERSION">>, <<"2022-05-20">>},
               {<<"Content-Type">>, <<"application/json">>}
              ],
    Headers.
