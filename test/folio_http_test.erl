-module(folio_http_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_http).
-define(MOCK_MODS, [hackney, folio_throttle, {fake_mod, [non_strict]}]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

http_request_429_test() ->
    load(),

    Method = get,
    URL = <<"http://example.com">>,
    Headers = [],
    Body = [],

    ok = meck:expect(
        folio_throttle,
        sleep,
        ['_'],
        ok
    ),
    ok = meck:expect(
        hackney,
        request,
        [
            {[get, URL, [], [], [with_body]], {ok, 429, [], <<>>}}
        ]
    ),
    ok = meck:expect(
        fake_mod,
        error_fun,
        [],
        ok
    ),

    ?MUT:request(Method, URL, Headers, Body, fun fake_mod:error_fun/0),

    [{sleep, [1000]}] = folio_meck:history_calls(folio_throttle),
    [{request, RequestArgs}] = folio_meck:history_calls(hackney),
    [{error_fun, []}] = folio_meck:history_calls(fake_mod),

    ?assertMatch([get, URL, [], [], [with_body]], RequestArgs),

    folio_meck:unload(?MOCK_MODS).
