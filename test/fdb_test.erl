-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, fdb).
-define(MOCK_MODS, [epgsql]).

-define(STATEMENT_GET_TABLE, "SELECT * FROM information_schema.tables WHERE table_name = $1").
-define(STATEMENT_TABLES,
    "SELECT * FROM information_schema.tables WHERE table_schema NOT IN ('information_schema', 'pg_catalog');"
).
-define(STATEMENT_GET_COLUMNS,
    "select * from information_schema.columns where table_name = $1 ORDER BY \"ordinal_position\" ASC"
).

create_table_test() ->
    folio_meck:load(?MOCK_MODS),

    TableName = "test_table",

    Conn = make_ref(),
    TestTableSchema = #{
        type => table, name => TableName, columns => [#{name => "id", type => "integer"}]
    },

    io:format("Conn ~p~n", [Conn]),
    ok = meck:expect(epgsql, equery, [Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, [], []}),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {equery, _EqueryArgs},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "CREATE TABLE test_table( id integer );"], SqueryArgs),

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

    io:format("Conn ~p~n", [Conn]),
    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
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

    io:format("Conn ~p~n", [Conn]),
    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
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

    io:format("Conn ~p~n", [Conn]),
    ok = meck:expect(epgsql, equery, [
        {[Conn, ?STATEMENT_GET_TABLE, [TableName]], {ok, TablesColumnRet, TablesDataRet}},
        {[Conn, ?STATEMENT_GET_COLUMNS, [TableName]], {ok, ColumnsColumnRet, ColumnsDataRet}}
    ]),

    ok = meck:expect(epgsql, squery, [Conn, '_'], {ok, [], []}),

    ok = ?MUT:run(Conn, [TestTableSchema]),

    [
        {equery, _TableEqueryArgs},
        {equery, _ColumnsEqueryArgs},
        {squery, SqueryArgs}
    ] = folio_meck:history_calls(epgsql),

    ?assertMatch([Conn, "ALTER TABLE test_table DROP COLUMN remove_column;"], SqueryArgs),

    folio_meck:unload(?MOCK_MODS).
