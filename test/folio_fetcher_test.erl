-module(folio_fetcher_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_fetcher).
-define(MOCK_MODS, [folio_integration, folio_account_provider, fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

gen_server_sync_no_accounts_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    expect_set_integration_state(),
    expect_timer_apply_interval(),
    expect_write_accounts(),
    expect_write_account_transactions(),

    Int1 = #{id => <<"id1">>, provider_name => <<"name1">>},
    Integrations = [Int1],

    ok = meck:expect(
        folio_integration,
        integrations,
        [Conn],
        {ok, Integrations}
    ),
    ok = meck:expect(
        folio_integration,
        get_integration_state,
        ['_'],
        {ok, running}
    ),
    ok = meck:expect(
        folio_account_provider,
        fetch_integration_accounts,
        ['_'],
        {ok, []}
    ),

    {ok, _Pid} = ?MUT:start_link(),
    pong = ?MUT:ping(),
    ?MUT:sync(),

    meck:wait(folio_integration, set_integration_state, [Int1, complete], 3000),

    ok = ?MUT:stop(),
    2 = fdb_test:assert_checkouts_matches_checkins(),
    [
        {integrations, [Conn]},
        {set_integration_state, [Int1, starting]},
        {write_accounts, [Conn, Int1, []]},
        {get_integration_state, [Int1]},
        {set_integration_state, [Int1, complete]}
    ] =
        folio_meck:history_calls(folio_integration),

    folio_meck:unload(?MOCK_MODS).

gen_server_sync_no_accounts_with_previous_error_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    expect_set_integration_state(),
    expect_timer_apply_interval(),
    expect_write_accounts(),
    expect_write_account_transactions(),

    Int1 = #{id => <<"id1">>, provider_name => <<"name1">>},
    Integrations = [Int1],

    ok = meck:expect(
        folio_integration,
        integrations,
        [Conn],
        {ok, Integrations}
    ),
    ok = meck:expect(
        folio_integration,
        get_integration_state,
        ['_'],
        {ok, error}
    ),
    ok = meck:expect(
        folio_account_provider,
        fetch_integration_accounts,
        ['_'],
        {ok, []}
    ),

    {ok, _Pid} = ?MUT:start_link(),
    pong = ?MUT:ping(),
    ?MUT:sync(),

    meck:wait(folio_integration, set_integration_state, [Int1, error], 3000),

    ok = ?MUT:stop(),
    2 = fdb_test:assert_checkouts_matches_checkins(),
    [
        {integrations, [Conn]},
        {set_integration_state, [Int1, starting]},
        {write_accounts, [Conn, Int1, []]},
        {get_integration_state, [Int1]},
        {set_integration_state, [Int1, error]}
    ] =
        folio_meck:history_calls(folio_integration),

    folio_meck:unload(?MOCK_MODS).

gen_server_sync_accounts_no_txs_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),
    expect_timer_apply_interval(),
    expect_set_integration_state(),
    expect_write_accounts(),
    expect_write_account_transactions(),

    Int2 = #{id => <<"id2">>, provider_name => <<"name2">>},
    Integrations = [Int2],

    Bal1 = #{balance => {15, 2}, symbol => <<"BTC">>},
    Bal2 = #{balance => {7, -2}, symbol => <<"ETH">>},
    Acct2 = #{id => <<"account_2">>, balances => [Bal1, Bal2]},

    ok = meck:expect(
        folio_integration,
        integrations,
        [Conn],
        {ok, Integrations}
    ),
    ok = meck:expect(
        folio_integration,
        get_integration_state,
        ['_'],
        {ok, running}
    ),
    ok = meck:expect(
        folio_account_provider,
        fetch_integration_accounts,
        [
            {[Int2], {ok, [Acct2]}}
        ]
    ),
    ok = meck:expect(
        folio_account_provider,
        fetch_integration_account_transactions,
        ['_', '_', '_'],
        ok
    ),

    {ok, _Pid} = ?MUT:start_link(),
    pong = ?MUT:ping(),
    ?MUT:sync(),

    meck:wait(folio_integration, set_integration_state, [Int2, starting], 3000),
    meck:wait(folio_integration, set_integration_state, [Int2, complete], 3000),

    ok = ?MUT:stop(),
    2 = fdb_test:assert_checkouts_matches_checkins(),

    [
        {integrations, [Conn]},
        {set_integration_state, [Int2, starting]},
        {write_accounts, [
            Conn,
            Int2,
            [
                #{
                    balances :=
                        [
                            #{balance := {15, 2}, symbol := <<"BTC">>},
                            #{balance := {7, -2}, symbol := <<"ETH">>}
                        ],
                    id := <<"account_2">>
                }
            ]
        ]},
        {get_integration_state, [Int2]},
        {set_integration_state, [Int2, running]},
        {get_integration_state, [Int2]},
        {set_integration_state, [Int2, complete]}
    ] =
        folio_meck:history_calls(folio_integration),

    folio_meck:unload(?MOCK_MODS).

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

expect_write_accounts() ->
    ok = meck:expect(
        folio_integration,
        write_accounts,
        ['_', '_', '_'],
        ok
    ).

expect_write_account_transactions() ->
    ok = meck:expect(
        folio_integration,
        write_account_transactions,
        ['_', '_', '_', '_'],
        ok
    ).
