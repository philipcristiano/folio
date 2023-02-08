-module(folio_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    App = folio,
    {ok, Pid} = folio_sup:start_link(),
    IsLocalDev = application:get_env(App, local_dev, true),

    ok = init_folio_modules(),

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

init_folio_modules() ->
    AvailableMods = code:all_available(),

    lists:foreach(
        fun({ModL, _, _}) ->
            Mod = list_to_atom(ModL),
            try
                Exports = Mod:module_info(exports),

                case lists:member({folio_init, 0}, Exports) of
                    true -> Mod:folio_init();
                    false -> ok
                end
            catch
                error:undef -> ok
            end
        end,
        AvailableMods
    ),
    ok.
