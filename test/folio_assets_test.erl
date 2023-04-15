-module(folio_assets_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_assets).
-define(MOCK_MODS, [fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

get_assets_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),

    Filters = make_ref(),
    Return = make_ref(),

    meck:expect(fdb, select, [
        {[Conn, assets, Filters, '_'], {ok, Return}}
    ]),

    {ok, Return} = ?MUT:get_assets(Conn, Filters),

    [
        {select, [Conn, assets, Filters, [{order_by, external_id, asc}]]}
    ] = folio_meck:history_calls(fdb),

    folio_meck:unload(?MOCK_MODS).