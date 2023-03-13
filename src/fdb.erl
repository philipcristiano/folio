-module(fdb).

-include_lib("kernel/include/logger.hrl").

-export([connect/0, close/1, schema/0, run/2]).
-export([write/3, select/3, delete/3]).

% -type user() :: #{
%     id := user_id(),
%     org_id := binary()
% }.

connect() ->
    {ok, Conn} = epgsql:connect(#{
        host => "localhost",
        username => "folio_admin",
        password => "pass",
        database => "folio",
        timeout => 4000
    }),
    {ok, Conn}.

close(C) ->
    epgsql:close(C).

schema() ->
    [
        #{
            type => table,
            name => "integrations",
            columns => [
                #{name => "id", type => "uuid"},
                #{name => "provider_name", type => "text"}
            ],
            primary_key => ["id"]
        },
        #{
            type => table,
            name => "integration_credentials",
            columns => [
                #{name => "integration_id", type => "uuid"},
                #{name => "credentials", type => "bytea"}
            ],
            primary_key => ["integration_id"]
        },
        #{
            type => table,
            name => "integration_accounts",
            columns => [
                % TODO: Rename external_account_id
                #{name => "external_id", type => "text"},
                #{name => "integration_id", type => "uuid"}
            ],
            primary_key => ["integration_id", "external_id"]
        },
        #{
            type => table,
            name => "integration_account_balances",
            columns => [
                #{name => "integration_id", type => "uuid"},
                #{name => "external_id", type => "text"},
                #{name => "symbol", type => "text"},
                #{name => "balance", type => "numeric"}
            ],
            primary_key => ["integration_id", "external_id", "symbol"]
        },
        #{
            type => table,
            name => "integration_account_transactions",
            columns => [
                #{name => "integration_id", type => "uuid"},
                #{name => "external_id", type => "text"},
                #{name => "source_id", type => "text"},
                #{name => "direction", type => "text"},
                #{name => "timestamp", type => "timestamp"},
                #{name => "symbol", type => "text"},
                #{name => "amount", type => "numeric"},
                #{name => "type", type => "text"},
                #{name => "description", type => "text"}
            ],
            primary_key => ["integration_id", "external_id", "source_id"]
        }
    ].

-spec get_table(epgsql:connection(), binary()) -> {ok, list()}.
get_table(DBRef, Name) ->
    Statement = "SELECT * FROM information_schema.tables WHERE table_name = $1",
    {ok, C, D} = epgsql:equery(
        DBRef, Statement, [Name]
    ),
    {ok, serialise(C, D)}.

get_tables(DBRef) ->
    Statement =
        "SELECT * FROM information_schema.tables WHERE table_schema NOT IN ('information_schema', 'pg_catalog');",
    {ok, C, D} = epgsql:squery(DBRef, Statement),
    {ok, serialise(C, D)}.

get_table_columns(DBRef, Name) ->
    {ok, C, D} = epgsql:equery(
        DBRef,
        "SELECT * FROM information_schema.columns WHERE table_name = $1 ORDER BY \"ordinal_position\" ASC",
        [Name]
    ),
    {ok, serialise(C, D)}.

run(Conn, Schema) ->
    DefinedTables = lists:filter(
        fun(#{type := Type}) when is_atom(Type) ->
            Type == table
        end,
        Schema
    ),

    {ok, ExistingTables} = get_tables(Conn),
    {_, ToRemove} = set_differences(
        DefinedTables,
        fun(#{name := N}) -> erlang:list_to_binary(N) end,
        ExistingTables,
        fun(#{table_name := N}) -> N end
    ),

    CreateAndModifyStatements = determine_migrations(Conn, Schema),
    DropTableStatements = lists:map(
        fun(#{table_name := NBin}) ->
            Name = erlang:binary_to_list(NBin),
            drop_table_statement(Name)
        end,
        ToRemove
    ),

    Statements = CreateAndModifyStatements ++ DropTableStatements,

    lists:foreach(
        fun(Statement) ->
            ?LOG_INFO(#{
                message => "Database migration",
                statement => Statement
            }),
            {ok, _, _} = epgsql:squery(Conn, Statement)
        end,
        Statements
    ),

    ok.

-spec write(epgsql:connection(), atom(), map()) -> {ok, map()}.
write(Conn, Table, Data) when is_map(Data) ->
    Fun = fun(K, V, {Count, Keys, Pos, Vals}) ->
        FKeys = Keys ++ [erlang:atom_to_list(K)],
        FCount = Count + 1,
        FPos = Pos ++ ["$" ++ erlang:integer_to_list(FCount)],
        FVal = Vals ++ [V],
        {FCount, FKeys, FPos, FVal}
    end,
    {_C, SQLKeys, SQLPos, SQLVals} = maps:fold(Fun, {0, [], [], []}, Data),

    PKey = lists:flatten([erlang:atom_to_list(Table), "_pkey"]),

    KeysStatement = ["(", lists:join(", ", SQLKeys), ")"],
    PosStatement = ["(", lists:join(", ", SQLPos), ")"],

    KVs = lists:zip(SQLKeys, SQLPos),
    UpdateStatements = lists:map(
        fun({K, V}) ->
            lists:flatten([K, " = ", V])
        end,
        KVs
    ),
    UpdateStatement = lists:join(", ", UpdateStatements),

    Statement = lists:flatten([
        "INSERT INTO ",
        erlang:atom_to_list(Table),
        " ",
        KeysStatement,
        " VALUES ",
        PosStatement,
        " ON CONFLICT ON CONSTRAINT ",
        PKey,
        " DO UPDATE set ",
        UpdateStatement,
        " RETURNING *;"
    ]),
    case epgsql:equery(Conn, Statement, SQLVals) of
        {ok, 1, RetCols, RetData} ->
            [RetMap] = serialise(RetCols, RetData),
            {ok, RetMap}
    end.

-spec select(epgsql:connection(), atom() | list(), map() | list()) -> {ok, list(map())}.
select(Conn, Table, Data) when is_atom(Table), is_map(Data) ->
    Fun = fun(K, V, {Count, Statements, Vals}) ->
        FCount = Count + 1,
        Statement = lists:flatten([
            erlang:atom_to_list(K), " = ", "$", erlang:integer_to_list(FCount)
        ]),
        FStatement = Statements ++ [Statement],
        FVal = Vals ++ [V],
        {FCount, FStatement, FVal}
    end,
    {_C, QueryStatements, SQLVals} = maps:fold(Fun, {0, [], []}, Data),
    QueryStatement = lists:join(" AND ", QueryStatements),
    Where =
        case QueryStatement of
            [] -> "";
            Else -> [" WHERE ", Else]
        end,

    Statement = lists:flatten([
        "SELECT * FROM ",
        erlang:atom_to_list(Table),
        Where,
        ";"
    ]),
    select(Conn, Statement, SQLVals);
select(Conn, Statement, PositionalValues) when is_list(Statement), is_list(PositionalValues) ->
    case epgsql:equery(Conn, Statement, PositionalValues) of
        {ok, RetCols, RetData} ->
            {ok, serialise(RetCols, RetData)}
    end.

delete(Conn, Table, Data) when is_atom(Table) ->
    Fun = fun(K, V, {Count, Statements, Vals}) ->
        FCount = Count + 1,
        Statement = lists:flatten([
            erlang:atom_to_list(K), " = ", "$", erlang:integer_to_list(FCount)
        ]),
        FStatement = Statements ++ [Statement],
        FVal = Vals ++ [V],
        {FCount, FStatement, FVal}
    end,
    {_C, QueryStatements, SQLVals} = maps:fold(Fun, {0, [], []}, Data),
    QueryStatement = lists:join(" AND ", QueryStatements),
    Where =
        case QueryStatement of
            [] -> "";
            Else -> [" WHERE ", Else]
        end,

    Statement = lists:flatten([
        "DELETE FROM ",
        erlang:atom_to_list(Table),
        Where,
        ";"
    ]),
    epgsql:equery(Conn, Statement, SQLVals).

determine_migrations(Conn, Schema) ->
    lists:foldl(fun(I, Acc) -> Acc ++ determine_migration(Conn, I) end, [], Schema).

determine_migration(Conn, Schema = #{type := table, name := TableName}) ->
    {ok, Data} = get_table(Conn, TableName),
    ?LOG_INFO(#{
        message => table_info,
        table => TableName,
        info => Data
    }),
    case Data of
        [] -> create_table_statement(Conn, Schema);
        [_] -> alter_table_statement(Conn, Schema)
    end.

create_table_statement(_Conn, TableSchema = #{type := table, name := TableName, columns := Cols}) ->
    ColStrings = lists:map(fun create_table_column/1, Cols),
    PrimaryKey = primary_key(TableSchema),

    TableComponents = ColStrings ++ PrimaryKey,
    ColString = lists:join(", ", TableComponents),

    Create = ["CREATE TABLE ", TableName, "( ", ColString, " );"],
    Statement = lists:flatten(Create),
    [Statement].

% Column ordering is not preserved if multiple columns are added
alter_table_statement(Conn, #{type := table, name := TableName, columns := Cols}) ->
    {ok, ColData} = get_table_columns(Conn, TableName),
    {ToAdd, ToRemove} = set_differences(
        Cols,
        fun(#{name := N}) -> erlang:list_to_binary(N) end,
        ColData,
        fun(#{column_name := N}) -> N end
    ),

    AddColumnsStatements = lists:map(
        fun(Col) ->
            add_table_column(TableName, Col)
        end,
        ToAdd
    ),

    DropColumnsStatements = lists:map(
        fun(_DBCol = #{column_name := Name}) ->
            Col = #{name => erlang:binary_to_list(Name)},
            drop_table_column(TableName, Col)
        end,
        ToRemove
    ),

    ?LOG_INFO(#{
        message => alter_table_statement,
        table => TableName,
        to_add => ToAdd,
        to_add_statements => AddColumnsStatements,
        to_remove => ToRemove,
        to_remove_statements => DropColumnsStatements
    }),
    AddColumnsStatements ++ DropColumnsStatements.

create_table_column(#{name := Name, type := Type}) ->
    [Name, " ", Type].

primary_key(#{type := table, primary_key := KeyElements}) ->
    ElementsString = lists:join(", ", KeyElements),

    S = ["PRIMARY KEY ( " ++ ElementsString ++ " )"],
    lists:join("", S);
primary_key(#{type := table}) ->
    [].

add_table_column(TableName, #{name := ColName, type := ColType}) ->
    lists:flatten(["ALTER TABLE ", TableName, " ADD COLUMN ", ColName, " ", ColType, ";"]).
drop_table_column(TableName, #{name := ColName}) ->
    lists:flatten(["ALTER TABLE ", TableName, " DROP COLUMN ", ColName, ";"]).

drop_table_statement(TableName) ->
    lists:flatten(["DROP TABLE ", TableName, ";"]).

serialise(Columns, Datas) ->
    Keys = lists:map(
        fun(Col) ->
            element(2, Col)
        end,
        Columns
    ),
    lists:map(
        fun(Val) ->
            lists:foldl(
                fun(K, Acc2) ->
                    Idx = map_size(Acc2) + 1,
                    maps:put(binary_to_atom(K), element(Idx, Val), Acc2)
                end,
                maps:new(),
                Keys
            )
        end,
        Datas
    ).

%%%
%
% Given 2 lists (and functions to indentify the comparable value for elements)
% find the set difference between the lists. Used for finding which operations
% are required to converge on a desired state.
%
%%%
set_differences(AList, AKeyFun, BList, BKeyFun) ->
    AMap = lists:foldl(fun(A, Ac) -> maps:put(AKeyFun(A), A, Ac) end, #{}, AList),
    BMap = lists:foldl(fun(B, Bc) -> maps:put(BKeyFun(B), B, Bc) end, #{}, BList),

    AIDs = maps:keys(AMap),
    BIDs = maps:keys(BMap),

    AIDsSet = sets:from_list(AIDs),
    BIDsSet = sets:from_list(BIDs),

    AIDsDiff = sets:to_list(sets:subtract(AIDsSet, BIDsSet)),
    BIDsDiff = sets:to_list(sets:subtract(BIDsSet, AIDsSet)),

    ADiff = lists:map(fun(AID) -> maps:get(AID, AMap) end, AIDsDiff),
    BDiff = lists:map(fun(BID) -> maps:get(BID, BMap) end, BIDsDiff),

    {ADiff, BDiff}.
