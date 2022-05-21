-module(folio_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Pid} = folio_sup:start_link(),
    %  pgapp:connect(#{host => "localhost", size => 10, database => "loc", username => "loc_app"}),
    Dispatch = cowboy_router:compile([
        {'_', [{"/accounts", folio_handler_accounts, []}]}
    ]),
    {ok, _} = cowboy:start_clear(folio,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid}.

stop(_State) ->
    ok.
