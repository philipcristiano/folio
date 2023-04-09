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
    Int2 = #{id => <<"id2">>, provider_name => <<"name2">>},
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
    pong = ?MUT:ping(),
    ?MUT:sync(),

    meck:wait(folio_integration, set_integration_state, [Int1, complete], 3000),
    meck:wait(folio_integration, set_integration_state, [Int2, complete], 3000),

    ok = ?MUT:stop(),
    3 = assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).

gen_server_sync_accounts_no_txs_test() ->
    load(),

    Conn = expect_fdb_checkout(),
    expect_fdb_checkin(Conn),
    expect_fdb_writes(Conn),
    expect_timer_apply_interval(),
    expect_set_integration_state(),

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
        fetch_integration_accounts,
        [
            {[Int2], {ok, [Acct2]}}
        ]
    ),
    ok = meck:expect(
        folio_integration,
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
    2 = assert_checkouts_matches_checkins(),
    assert_fdb_writes([
        [
            Conn,
            integration_accounts,
            #{external_id => <<"account_2">>, integration_id => <<"id2">>}
        ],
        [
            Conn,
            integration_account_balances,
            #{
                balance => <<"1500.0">>,
                external_id => <<"account_2">>,
                integration_id => <<"id2">>,
                symbol => <<"BTC">>
            }
        ],
        [
            Conn,
            integration_account_balances,
            #{
                balance => <<"0.07">>,
                external_id => <<"account_2">>,
                integration_id => <<"id2">>,
                symbol => <<"ETH">>
            }
        ]
    ]),

    folio_meck:unload(?MOCK_MODS).

expect_fdb_checkout() ->
    R = make_ref(),
    ok = meck:expect(fdb, checkout, [], R),
    R.

expect_fdb_checkin(R) ->
    ok = meck:expect(fdb, checkin, [R], ok).

expect_fdb_writes(R) ->
    ok = meck:expect(fdb, write, [R, '_', '_'], {ok, #{}}).

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

assert_fdb_writes(Writes) ->
    Calls = folio_meck:history_calls(fdb),

    WriteCalls = lists:filtermap(
        fun({M, Args}) ->
            case M of
                write -> {true, Args};
                _ -> false
            end
        end,
        Calls
    ),
    io:format("Write calls ~p~n", [WriteCalls]),

    lists:zipwith(
        fun(A, B) ->
            ?assertEqual(A, B)
        end,
        Writes,
        WriteCalls
    ).
