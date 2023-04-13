-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

-export([expect_fdb_checkout/0, expect_fdb_checkin/1, expect_fdb_writes/1]).
-export([assert_checkouts_matches_checkins/0, assert_writes/1]).

-define(MUT, fdb).
-define(MOCK_MODS, [epgsql]).

-define(STATEMENT_GET_TABLE, "SELECT * FROM information_schema.tables WHERE table_name = $1").
-define(STATEMENT_TABLES,
    "SELECT * FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_schema NOT IN ('information_schema', 'pg_catalog');"
).
-define(STATEMENT_VIEWS,
    "SELECT * FROM information_schema.tables WHERE table_type = 'VIEW' AND table_schema NOT IN ('information_schema', 'pg_catalog');"
).
-define(STATEMENT_GET_COLUMNS,
    "SELECT * FROM information_schema.columns WHERE table_name = $1 ORDER BY \"ordinal_position\" ASC"
).

create_table_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table, name => TableName, columns => [#{name => "id", type => "integer"}]
    },

    ok = meck:expect(epgsql, equery, [Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, [], []}),

    ok = meck:expect(epgsql, squery, [
        {[Conn, ?STATEMENT_TABLES], {ok, [], []}},
        {[Conn, '_'], {ok, [], []}}
    ]),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, [Conn, ?STATEMENT_GET_TABLE, _GetTableArg]},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "CREATE TABLE test_table( id integer );"], SqueryArgs),

    folio_meck:unload(?MOCK_MODS).

create_table_with_primary_key_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table,
        name => TableName,
        columns => [
            #{
                name => "id",
                type =>
                    "integer"
            }
        ],
        primary_key => ["id"]
    },

    ok = meck:expect(epgsql, equery, [Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, [], []}),

    ok = meck:expect(epgsql, squery, [
        {[Conn, ?STATEMENT_TABLES], {ok, [], []}},
        {[Conn, '_'], {ok, [], []}}
    ]),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, [Conn, ?STATEMENT_GET_TABLE, _GetTableArg]},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "CREATE TABLE test_table( id integer, PRIMARY KEY ( id ) );"], SqueryArgs),

    folio_meck:unload(?MOCK_MODS).

drop_table_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table, name => TableName, columns => [#{name => "id", type => "integer"}]
    },

    TablesColumnRet = [{column, <<"table_name">>}],
    TablesDataRet = [{<<"test_table">>}],

    ColumnsColumnRet = [{column, <<"column_name">>}],
    ColumnsDataRet = [{<<"id">>}],

    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [
        {[Conn, ?STATEMENT_TABLES], {ok, TablesColumnRet, TablesDataRet ++ [{<<"remove_table">>}]}},
        {[Conn, '_'], {ok, [], []}}
    ]),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs},
        {squery, SQueryArgs}
    ] = folio_meck:history_calls(epgsql),

    folio_meck:unload(?MOCK_MODS).

table_exists_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table, name => TableName, columns => [#{name => "id", type => "integer"}]
    },

    TablesColumnRet = [{column, <<"table_name">>}],
    TablesDataRet = [{<<"test_table">>}],

    ColumnsColumnRet = [{column, <<"column_name">>}],
    ColumnsDataRet = [{<<"id">>}],

    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    folio_meck:unload(?MOCK_MODS).

table_exists_create_column_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table,
        name => TableName,
        columns => [#{name => "id", type => "integer"}, #{name => "new_column", type => "integer"}]
    },

    TablesColumnRet = [{column, <<"table_name">>}],
    TablesDataRet = [{<<"test_table">>}],

    ColumnsColumnRet = [{column, <<"column_name">>}],
    ColumnsDataRet = [{<<"id">>}],

    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "ALTER TABLE test_table ADD COLUMN new_column integer;"], SqueryArgs),

    folio_meck:unload(?MOCK_MODS).

table_exists_delete_column_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table, name => TableName, columns => [#{name => "id", type => "integer"}]
    },

    TablesColumnRet = [{column, <<"table_name">>}],
    TablesDataRet = [{<<"test_table">>}],

    ColumnsColumnRet = [{column, <<"column_name">>}],
    ColumnsDataRet = [{<<"id">>}, {<<"remove_column">>}],

    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "ALTER TABLE test_table DROP COLUMN remove_column;"], SqueryArgs),

    folio_meck:unload(?MOCK_MODS).

create_view_test() ->
    folio_meck:load(?MOCK_MODS),

    ViewName = "test_view",

    Conn = make_ref(),
    TestViewSchema = #{
        type => view,
        name => ViewName,
        column_names => ["a", "b", "c"],
        query => "SELECT 1 as a, 2 as b, 3 as C;"
    },

    ok = meck:expect(epgsql, squery, [
        {[Conn, '_'], {ok, [], []}}
    ]),

    ok = ?MUT:run(Conn, [TestViewSchema]),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch(
        [Conn, "CREATE OR REPLACE VIEW test_view ( a, b, c ) AS SELECT 1 as a, 2 as b, 3 as C;"],
        SqueryArgs
    ),

    folio_meck:unload(?MOCK_MODS).

drop_view_test() ->
    folio_meck:load(?MOCK_MODS),

    TablesColumnRet = [{column, <<"table_name">>}],

    Conn = make_ref(),

    ok = meck:expect(epgsql, squery, [
        {[Conn, ?STATEMENT_VIEWS], {ok, TablesColumnRet, [{<<"remove_view">>}]}},
        {[Conn, '_'], {ok, [], []}}
    ]),

    ok = ?MUT:run(Conn, []),

    [
        {squery, [Conn, ?STATEMENT_TABLES]},
        {squery, [Conn, ?STATEMENT_VIEWS]},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch(
        [Conn, "DROP VIEW remove_view;"],
        SqueryArgs
    ),

    folio_meck:unload(?MOCK_MODS).

write_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement =
        "INSERT INTO example_table (foo, one) VALUES ($1, $2) ON CONFLICT ON CONSTRAINT example_table_pkey DO UPDATE set foo = $1, one = $2 RETURNING *;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, 1, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:write(Conn, example_table, #{foo => <<"bar">>, one => 1}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, #{foo := <<"bar">>, one := 1, id := <<"retid">>}}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement = "SELECT * FROM example_table WHERE foo = $1 AND one = $2;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{foo => <<"bar">>, one => 1}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_order_by_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement = "SELECT * FROM example_table WHERE foo = $1 AND one = $2 ORDER BY one DESC;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{foo => <<"bar">>, one => 1}, [{order_by, one, desc}]),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_order_by_and_limit_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement =
        "SELECT * FROM example_table WHERE foo = $1 AND one = $2 ORDER BY one DESC LIMIT 1;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{foo => <<"bar">>, one => 1}, [
        {order_by, one, desc}, {limit, 1}
    ]),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_greater_than_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement = "SELECT * FROM example_table WHERE foo = $1 AND one > $2;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{foo => <<"bar">>, one => {'>', 1}}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_less_than_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement = "SELECT * FROM example_table WHERE foo = $1 AND one < $2;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{foo => <<"bar">>, one => {'<', 1}}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

select_all_table_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    ColumnRet = [{column, <<"foo">>}, {column, <<"one">>}, {id, <<"id">>}],
    DataRet = [{<<"bar">>, 1, <<"retid">>}],

    Statement = "SELECT * FROM example_table;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                []
            ],
            {ok, ColumnRet, DataRet}
        }
    ]),

    Ret = ?MUT:select(Conn, example_table, #{}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, [#{foo := <<"bar">>, one := 1, id := <<"retid">>}]}, Ret),
    ?assertMatch([Conn, Statement, []], Args),
    folio_meck:unload(?MOCK_MODS).

delete_test() ->
    folio_meck:load(?MOCK_MODS),
    Conn = make_ref(),

    Statement = "DELETE FROM example_table WHERE foo = $1 AND one = $2;",
    ok = meck:expect(epgsql, equery, [
        {
            [
                Conn,
                '_',
                [<<"bar">>, 1]
            ],
            {ok, 1}
        }
    ]),

    Ret = ?MUT:delete(Conn, example_table, #{foo => <<"bar">>, one => 1}),

    [{equery, Args}] = folio_meck:history_calls(epgsql),

    ?assertMatch({ok, 1}, Ret),
    ?assertMatch([Conn, Statement, [<<"bar">>, 1]], Args),
    folio_meck:unload(?MOCK_MODS).

%%%
%
% Helpers for other modules to use for expecting/asserting fdb
%
%%%

expect_fdb_checkout() ->
    R = make_ref(),
    ok = meck:expect(fdb, checkout, [], R),
    R.

expect_fdb_checkin(R) ->
    ok = meck:expect(fdb, checkin, [R], ok).

expect_fdb_writes(R) ->
    ok = meck:expect(fdb, write, [R, '_', '_'], {ok, #{}}).

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

assert_checkout_checkin(Conn) ->
    [First | Rest] = folio_meck:history_calls(fdb),
    Last = lists:last(Rest),

    ?assertEqual({checkout, []}, First),
    ?assertEqual({checkin, [Conn]}, Last),
    ok.

assert_writes(Writes) ->
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
