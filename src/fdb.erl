-module(fdb).

-include_lib("kernel/include/logger.hrl").

-export([schema/0, run/1]).

% -type user() :: #{
%     id := user_id(),
%     org_id := binary()
% }.

schema() ->
    [
        #{
            type => table,
            name => "example_table",
            columns => [
                #{name => "id", type => "integer"},
                #{name => "data", type => "text"},
                #{name => "next_column_a", type => "boolean"},
                #{name => "next_column_b", type => "boolean"},
                #{name => "next_column_c", type => "boolean"}
            ]
        }
    ].

-spec get_table(epgsql:connection(), binary()) -> {ok, list()}.
get_table(DBRef, Name) ->
    {ok, C, D} = epgsql:equery(
        DBRef, "select * from information_schema.tables where table_name = $1", [Name]
    ),
    {ok, serialise(C, D)}.

get_table_columns(DBRef, Name) ->
    {ok, C, D} = epgsql:equery(
        DBRef,
        "select * from information_schema.columns where table_name = $1 ORDER BY \"ordinal_position\" ASC",
        [Name]
    ),
    {ok, serialise(C, D)}.

run(Schema) ->
    {ok, Conn} = epgsql:connect(#{
        host => "localhost",
        username => "folio_admin",
        password => "pass",
        database => "folio",
        timeout => 4000
    }),
    {ok, C, D} = epgsql:squery(Conn, "SELECT 1 as c;"),
    io:format("Returned ~p~n", [serialise(C, D)]),
    StatementGroups = determine_migrations(Conn, Schema),

    lists:foreach(
        fun(Statements) ->
            ValidStatements = lists:filter(fun is_list/1, Statements),
            lists:foreach(
                fun(Statement) ->
                    ?LOG_INFO(#{
                        message => "Database migration",
                        statement => Statement
                    }),
                    {ok, _, _} = epgsql:squery(Conn, Statement)
                end,
                ValidStatements
            )
        end,
        StatementGroups
    ),

    ok.

determine_migrations(Conn, Schema) ->
    lists:map(fun(I) -> determine_migration(Conn, I) end, Schema).

determine_migration(Conn, Schema = #{type := table, name := TableName}) ->
    {ok, Data} = get_table(Conn, TableName),
    ?LOG_INFO(#{
        message => table_info,
        table => TableName,
        info => Data
    }),
    case Data of
        [] -> [create_table_statement(Conn, Schema)];
        [_] -> alter_table_statement(Conn, Schema)
    end.

create_table_statement(_Conn, #{type := table, name := TableName, columns := Cols}) ->
    ColStrings = lists:map(fun create_table_column/1, Cols),
    ColString = lists:join(", ", ColStrings),

    Create = ["CREATE TABLE ", TableName, "( ", ColString, " );"],
    Statement = lists:flatten(Create),
    Statement.

% Column ordering is not preserved if multiple columns are added
alter_table_statement(Conn, #{type := table, name := TableName, columns := Cols}) ->
    {ok, ColData} = get_table_columns(Conn, TableName),
    {ToAdd, ToRemove} = set_differences(
        Cols,
        fun(#{name := N}) -> erlang:list_to_binary(N) end,
        ColData,
        fun(#{column_name := N}) -> N end
    ),

    AddStatements = lists:map(
        fun(Col) ->
            add_table_column(TableName, Col)
        end,
        ToAdd
    ),

    ?LOG_INFO(#{
        message => alter_table_statement,
        table => TableName,
        to_add => ToAdd,
        to_add_statements => AddStatements,
        to_remove => ToRemove
    }),
    AddStatements.

% ColStrings = lists:map( fun create_table_column/1, Cols),
% ColString = lists:join(", ", ColStrings),

% Create = ["CREATE TABLE ", TableName, "( ", ColString, " );"],
% Statement = lists:flatten(Create),
% Statement.

create_table_column(#{name := Name, type := Type}) ->
    [Name, " ", Type].

add_table_column(TableName, #{name := ColName, type := ColType}) ->
    lists:flatten(["ALTER TABLE ", TableName, " ADD COLUMN ", ColName, " ", ColType, ";"]).

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
