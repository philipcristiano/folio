-module(folio_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    App = folio,
    {ok, Pid} = folio_sup:start_link(),
    IsLocalDev = application:get_env(App, local_dev, true),

    StaticRoute = [
        {"/", cowboy_static, cowboy_priv_path_for_file(IsLocalDev, App, "public/index.html")},
        {"/[...]", cowboy_static, cowboy_priv_path_for_dir(IsLocalDev, App, "public")}
    ],
    AppRoutes = [
        {"/accounts", folio_handler_accounts, []},
        {"/ws", folio_ws, #{module => folio_ws_protocol}}
    ],
    AllRoutes = AppRoutes ++ StaticRoute,
    Dispatch = cowboy_router:compile([
        {'_', AllRoutes}
    ]),
    {ok, _} = cowboy:start_clear(
        folio,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid}.

stop(_State) ->
    ok.

cowboy_priv_path_for_file(_IsLocalDev = true, _App, Path) ->
    {file, "priv/" ++ Path};
cowboy_priv_path_for_file(_IsLocalDev = false, App, Path) ->
    {priv_file, App, Path}.

cowboy_priv_path_for_dir(_IsLocalDev = true, _App, Path) ->
    {dir, "priv/" ++ Path};
cowboy_priv_path_for_dir(_IsLocalDev = false, App, Path) ->
    {priv_dir, App, Path}.
