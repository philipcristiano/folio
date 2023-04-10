-module(folio_integration_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_integration).
-define(MOCK_MODS, [fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

set_integration_state_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    Int = #{id => <<"id1">>, provider_name => <<"name1">>},

    ?MUT:set_integration_state(Int, starting),

    [
        {checkout, []},
        {write, WriteArgs},
        {checkin, [Conn]}
    ] = folio_meck:history_calls(fdb),

    ?assertMatch(
        [Conn, integration_sync_states, #{integration_id := <<"id1">>, state := <<"starting">>}],
        WriteArgs
    ),

    folio_meck:unload(?MOCK_MODS).

get_integration_state_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    Int1 = #{id => <<"id1">>, provider_name => <<"name1">>},
    Int2 = #{id => <<"id2">>, provider_name => <<"name2">>},
    State1 = starting,

    ok = meck:expect(
        fdb,
        select,
        [
            {['_', '_', #{integration_id => <<"id1">>}, '_'], {ok, [#{state => State1}]}},
            {['_', '_', #{integration_id => <<"id2">>}, '_'], {ok, []}}
        ]
    ),

    {ok, starting} = ?MUT:get_integration_state(Int1),
    {ok, undefined} = ?MUT:get_integration_state(Int2),

    fdb_test:assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).
