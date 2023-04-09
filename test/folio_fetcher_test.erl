-module(folio_fetcher_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_fetcher).
-define(MOCK_MODS, [folio_integration, fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

gen_server_sync_no_accounts_test() ->
    load(),

    Conn = expect_fdb_checkout(),
    expect_fdb_checkin(Conn),
    expect_timer_apply_interval(),
    expect_set_integration_state(),

    Int1 = #{id => <<"id1">>, provider_name => <<"name1">>},
    Int2 = #{id => <<"id1">>, provider_name => <<"name1">>},
    Integrations = [Int1, Int2],

    ok = meck:expect(
        folio_integration,
        integrations,
        [Conn],
        {ok, Integrations}
    ),
    ok = meck:expect(
        folio_integration,
        fetch_integration_accounts,
        ['_'],
        {ok, []}
    ),

    {ok, _Pid} = ?MUT:start_link(),
    ?MUT:sync(),

    meck:wait(folio_integration, set_integration_state, [Int1, complete], 1000),
    meck:wait(folio_integration, set_integration_state, [Int2, complete], 1000),

    ok = ?MUT:stop(),
    3 = assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).

expect_fdb_checkout() ->
    R = make_ref(),
    ok = meck:expect(fdb, checkout, [], R),
    R.

expect_fdb_checkin(R) ->
    ok = meck:expect(fdb, checkin, [R], ok).

assert_checkout_checkin(Conn) ->
    [First | Rest] = folio_meck:history_calls(fdb),
    Last = lists:last(Rest),

    ?assertEqual({checkout, []}, First),
    ?assertEqual({checkin, [Conn]}, Last),
    ok.

expect_timer_apply_interval() ->
    ok = meck:expect(
        bi,
        timer_apply_interval,
        ['_', ?MUT, '_', '_'],
        {ok, fake_tref}
    ).

expect_set_integration_state() ->
    ok = meck:expect(
        folio_integration,
        set_integration_state,
        ['_', '_'],
        ok
    ).

assert_checkouts_matches_checkins() ->
    Calls = folio_meck:history_calls(fdb),

    D = lists:foldl(
        fun({Method, _Args}, C = #{checkouts := CO, checkins := CI}) ->
            R =
                case Method of
                    checkout -> C#{checkouts => CO + 1};
                    checkin -> C#{checkins => CI + 1};
                    _ -> C
                end,
            R
        end,
        #{checkouts => 0, checkins => 0},
        Calls
    ),
    #{checkouts := NumCheckouts, checkins := NumCheckins} = D,
    ?assertEqual(NumCheckouts, NumCheckins),
    NumCheckouts.
