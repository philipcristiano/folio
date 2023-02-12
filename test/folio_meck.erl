-module(folio_meck).

-export([history_calls/1, load/1, unload/1]).

history_calls(Mod) ->
    History = meck:history(Mod),
    ExtractFunAndArgs = fun({_Pid, {_CallMod, CallFun, CallArgs}, _Returned}) ->
        {CallFun, CallArgs}
    end,

    lists:map(ExtractFunAndArgs, History).

load(Mods) ->
    lists:foreach(fun mock_mod/1, Mods),
    ok.

unload(Mods) ->
    lists:foreach(
        fun unmock_mod/1,
        Mods
    ),
    ok.

mock_mod({Mod, Opts}) ->
    try
        ok = meck:new(Mod, Opts),
        ok
    catch
        _:{already_started, _Pid} ->
            ok = meck:unload(Mod),
            ok = meck:new(Mod, Opts),
            ok
    end;
mock_mod(Mod) ->
    try
        ok = meck:new(Mod),
        ok
    catch
        _:{already_started, _Pid} ->
            ok = meck:unload(Mod),
            ok = meck:new(Mod),
            ok
    end.

unmock_mod({Mod, _Opts}) ->
    true = meck:validate(Mod),
    ok = meck:unload(Mod),
    ok;
unmock_mod(Mod) ->
    unmock_mod({Mod, []}).
