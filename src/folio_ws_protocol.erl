-module(folio_ws_protocol).

-export([init/0]).
-export([handle_data/3]).
-include_lib("kernel/include/logger.hrl").

-record(state, {username}).

init() ->
    #state{}.

handle_data(<<"token login">>, Data, State=#state{}) ->
    Token = maps:get(<<"token">>, Data),

    {ok, JWTKey} = application:get_env(folio, jwt_key),

    {Reply, State0} = case jwt:decode(Token, JWTKey) of
        {ok, Claims} ->
            Username = maps:get(<<"username">>, Claims),
            ?LOG_INFO(#{what=>"Token login succeeded",
                        username=>Username}),
            {#{what=><<"logged in">>,
              token=>Token,
              username=>Username},
              State#state{username=Username}};
        {error, invalid_token} ->
            ?LOG_INFO(#{what=>"Token login failed",
                        token=>Token}),
            {#{what=><<"login failed">>,
              token=>Token},
             State}
    end,
    Json = jsx:encode(Reply),
    {reply, {text, Json}, State0};
handle_data(<<"login">>, Data, State=#state{}) ->
    Username = maps:get(<<"username">>, Data),
    Password = maps:get(<<"password">>, Data),
    {ok, JWTKey} = application:get_env(folio, jwt_key),
    ?LOG_INFO(#{what=>"Login attempt",
                username=>Username,
                password=>Password}),
    {Msg, State0} = case {Username, Password} of
        {<<"admin">>, <<"admin">>} ->
            ?LOG_INFO(#{what=>"Login succeeded",
                        username=>Username}),
            Claims = [{<<"username">>, Username }],
            {ok, Token} = jwt:encode(<<"HS256">>, Claims, JWTKey),
            Reply = #{what=><<"logged in">>,
                      token=>Token,
                      username=>Username},
            {Reply, State#state{username=Username}};
        _ ->
            ?LOG_INFO(#{what=>"Login failed",
                        username=>Username,
                        password=>Password}),
            Reply = #{what=><<"login failed">>},
            {Reply, State}
    end,
    Json = jsx:encode(Msg),
    {reply, {text, Json}, State0};
handle_data(Msg, Data, State=#state{username=undefined}) ->
    ?LOG_INFO(#{what=>"Unauthenticated message",
                msg=>Msg,
                data=>Data}),
    {ok, State};
handle_data(<<"start">>, _Data, State) ->
    {ok, User} = folio_coinbase_api:run(),
    Msg = #{what => coinbase_user, user => User},
    ?LOG_INFO(Msg),
    {reply, {to_json, Msg}, State};
handle_data(What, Data, State) ->
    ?LOG_INFO(#{what=>"Unhandled frame",
                message=>What,
                frame=>Data}),
    {ok, State}.
