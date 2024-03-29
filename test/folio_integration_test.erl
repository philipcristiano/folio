-module(folio_integration_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_integration).
-define(MOCK_MODS, [fdb, bi]).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok.

transactions_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    T1 =
        #{
            amount => <<"10.0">>,
            description => <<"test description">>,
            direction => in,
            external_id => <<"aid1">>,
            integration_id => <<"id1">>,
            line => <<"line">>,
            source_id => <<"source_id">>,
            provider_name => <<"name1">>,
            symbol => <<"BTC">>,
            timestamp => {{1, 2, 3}, {4, 5, 6}},
            type => undefined
        },
    T2 =
        #{
            amount => <<"12.0">>,
            description => <<"test description">>,
            direction => in,
            external_id => <<"aid1">>,
            integration_id => <<"id2">>,
            line => <<"line">>,
            provider_name => <<"name2">>,
            source_id => <<"source_id">>,
            symbol => <<"BTC">>,
            timestamp => {{1, 2, 3}, {4, 5, 7}},
            type => undefined
        },

    meck:expect(fdb, select, [
        {[Conn, v_annotated_transactions, '_', '_'], {ok, [T1, T2]}}
    ]),

    {ok, [TR1, TR2]} = ?MUT:transactions(Conn, #{}),

    ?assertMatch(
        #{
            amount := <<"10.0">>,
            description := <<"test description">>,
            direction := in,
            external_id := <<"aid1">>,
            integration_id := <<"id1">>,
            line := <<"line">>,
            source_id := <<"source_id">>,
            symbol := <<"BTC">>,
            timestamp := {{1, 2, 3}, {4, 5, 6}},
            type := undefined,
            provider_name := <<"name1">>
        },
        TR1
    ),

    ?assertMatch(
        #{
            amount := <<"12.0">>,
            description := <<"test description">>,
            direction := in,
            external_id := <<"aid1">>,
            integration_id := <<"id2">>,
            line := <<"line">>,
            source_id := <<"source_id">>,
            symbol := <<"BTC">>,
            timestamp := {{1, 2, 3}, {4, 5, 7}},
            type := undefined,
            provider_name := <<"name2">>
        },
        TR2
    ),

    folio_meck:unload(?MOCK_MODS).

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

write_accounts_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    Int = #{id => <<"id1">>, provider_name => <<"name1">>},
    Acct1 = #{id => <<"aid1">>, balances => []},
    Acct2 = #{
        id => <<"aid2">>, balances => [#{balance => {1, 2}, asset => #{symbol => <<"BTC">>}}]
    },
    Acct3 = #{
        id => <<"aid3">>,
        balances => [#{balance => {1, 3}, asset => #{symbol => <<"ETH">>, id => <<"native">>}}]
    },

    ok = ?MUT:write_accounts(Conn, Int, [Acct1, Acct2, Acct3]),

    [
        {write, [
            Conn,
            integration_accounts,
            #{
                external_id := <<"aid1">>,
                integration_id := <<"id1">>
            }
        ]},
        {write, [
            Conn,
            integration_accounts,
            #{
                external_id := <<"aid2">>,
                integration_id := <<"id1">>
            }
        ]},
        {write, [
            Conn,
            integration_account_balances,
            #{
                balance := <<"100.0">>,
                external_id := <<"aid2">>,
                integration_id := <<"id1">>,
                symbol := <<"BTC">>
            }
        ]},
        {write, [
            Conn,
            integration_accounts,
            #{
                external_id := <<"aid3">>,
                integration_id := <<"id1">>
            }
        ]},
        {write, [
            Conn,
            integration_account_balances,
            #{
                balance := <<"1000.0">>,
                external_id := <<"aid3">>,
                integration_id := <<"id1">>,
                symbol := <<"ETH">>,
                provider_asset_id := <<"native">>
            }
        ]}
    ] = folio_meck:history_calls(fdb),

    fdb_test:assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).

write_account_transactions_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    Int = #{id => <<"id1">>, provider_name => <<"name1">>},
    Acct = #{id => <<"aid1">>, balances => []},
    T1 = #{
        source_id => <<"source_id">>,
        line => <<"line">>,
        datetime => {{1, 2, 3}, {4, 5, 6}},
        direction => in,
        asset => #{symbol => <<"BTC">>},
        amount => {1, 1},
        type => undefined,
        description => <<"test description">>
    },

    ok = ?MUT:write_account_transactions(Conn, Int, Acct, [T1]),

    [
        {write, [
            Conn,
            integration_account_transactions,
            #{
                amount := <<"10.0">>,
                description := <<"test description">>,
                direction := in,
                external_id := <<"aid1">>,
                integration_id := <<"id1">>,
                line := <<"line">>,
                source_id := <<"source_id">>,
                symbol := <<"BTC">>,
                timestamp := {{1, 2, 3}, {4, 5, 6}},
                type := undefined
            }
        ]}
    ] = folio_meck:history_calls(fdb),

    fdb_test:assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).

write_account_transactions_with_asset_id_test() ->
    load(),

    Conn = fdb_test:expect_fdb_checkout(),
    fdb_test:expect_fdb_checkin(Conn),
    fdb_test:expect_fdb_writes(Conn),

    Int = #{id => <<"id1">>, provider_name => <<"name1">>},
    Acct = #{id => <<"aid1">>, balances => []},
    T1 = #{
        source_id => <<"source_id">>,
        line => <<"line">>,
        datetime => {{1, 2, 3}, {4, 5, 6}},
        direction => in,
        asset => #{symbol => <<"BTC">>, id => <<"native">>},
        amount => {1, 1},
        type => undefined,
        description => <<"test description">>
    },

    ok = ?MUT:write_account_transactions(Conn, Int, Acct, [T1]),

    [
        {write, [
            Conn,
            integration_account_transactions,
            #{
                amount := <<"10.0">>,
                description := <<"test description">>,
                direction := in,
                external_id := <<"aid1">>,
                integration_id := <<"id1">>,
                line := <<"line">>,
                source_id := <<"source_id">>,
                symbol := <<"BTC">>,
                provider_asset_id := <<"native">>,
                timestamp := {{1, 2, 3}, {4, 5, 6}},
                type := undefined
            }
        ]}
    ] = folio_meck:history_calls(fdb),

    fdb_test:assert_checkouts_matches_checkins(),

    folio_meck:unload(?MOCK_MODS).
