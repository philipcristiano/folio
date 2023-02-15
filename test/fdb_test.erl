-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, fdb).
-define(MOCK_MODS, [epgsql]).

-define(STATEMENT_GET_TABLE, "SELECT * FROM information_schema.tables WHERE table_name = $1").
-define(STATEMENT_TABLES,
    "SELECT * FROM information_schema.tables WHERE table_schema NOT IN ('information_schema', 'pg_catalog');"
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
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "ALTER TABLE test_table DROP COLUMN remove_column;"], SqueryArgs),

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
