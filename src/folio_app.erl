-module(folio_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    App = folio,
    IsLocalDev = application:get_env(App, local_dev, false),

    % Setup database
    {ok, Conn} = fdb:connect(),
    fdb:run(Conn, fdb:schema()),

    ok = init_folio_modules(),
    {ok, Pid} = folio_sup:start_link(),

    AppRoutes = [],
    StaticRoute = [
        {"/", cowboy_static, cowboy_priv_path_for_file(IsLocalDev, App, "public/index.html")},
        {"/[...]", cowboy_static, cowboy_priv_path_for_dir(IsLocalDev, App, "public")}
    ],
    AllRoutes = AppRoutes ++ setup_trails() ++ StaticRoute,
    Dispatch = trails:single_host_compile(AllRoutes),

    Port = folio_config:hosting_port(),
    ?LOG_INFO(#{
        message => "HTTP Port",
        port => Port
    }),

    APIPort = list_to_integer(os:getenv("API_PORT", Port)),

    {ok, _} = cowboy:start_clear(
        folio,
        [{port, APIPort}],
        #{
            env => #{dispatch => Dispatch},
            stream_handlers => [cowboy_telemetry_h, cowboy_stream_h]
        }
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

mod_strings() ->
    AvailableMods = code:all_available(),
    ModStrings = lists:map(fun({ModString, _, _}) -> ModString end, AvailableMods),
    ModStrings.

init_folio_modules() ->
    ?LOG_INFO(#{message => "Loading folio modules to auto-init"}),
    ModStrings = mod_strings(),
    FolioModStrings = lists:filter(
        fun(ModString) -> lists:prefix("folio_", ModString) end, ModStrings
    ),
    ModAtoms = lists:map(fun erlang:list_to_atom/1, FolioModStrings),
    ModsWithInit = lists:filter(
        fun(Mod) -> mod_has_function_signature({folio_init, 0}, Mod) end, ModAtoms
    ),

    lists:foreach(fun(Mod) -> Mod:folio_init() end, ModsWithInit),
    ok.

mod_has_function_signature(Signature, Mod) ->
    try
        Exports = Mod:module_info(exports),
        lists:member(Signature, Exports)
    catch
        error:undef -> false
    end.

trails_handlers() ->
    ModStrings = mod_strings(),

    FolioModStrings = lists:filter(
        fun(ModString) -> lists:prefix("folio_handler_", ModString) end, ModStrings
    ),
    ModAtoms = lists:map(fun erlang:list_to_atom/1, FolioModStrings),
    ModAtoms ++ [cowboy_swagger_handler].

setup_trails() ->
    Handlers = trails_handlers(),
    Trails = trails:trails(Handlers),
    trails:store(Trails),
    Trails.
