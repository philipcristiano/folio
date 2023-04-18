-module(folio_assets_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_assets).
-define(MOCK_MODS, [fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

get_annotated_assets_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),

    Filters = make_ref(),
    Return = make_ref(),

    meck:expect(fdb, select, [
        {[Conn, v_assets, Filters, '_'], {ok, Return}}
    ]),

    {ok, Return} = ?MUT:get_annotated_assets(Conn, Filters),

    [
        {select, [Conn, v_assets, Filters, [{order_by, last_price, desc}]]}
    ] = folio_meck:history_calls(fdb),

    folio_meck:unload(?MOCK_MODS).
