-module(fdb).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([connect/0, close/1, schema/0, run/2]).
-export([write/3, select/3, select/4, delete/3]).

-export([start_link/1, checkout/0, child_spec/0, checkin/1]).

-behaviour(poolboy_worker).

%%% Poolboy

start_link([]) ->
    connect().

child_spec() ->
    DB = poolboy:child_spec(
        db,
        [
            {size, folio_config:pg_pool(size, "10")},
            {max_overflow, 20},
            {name, {local, db}},
            {worker_module, fdb}
        ],
        []
    ),
    DB.

checkout() ->
    ?with_span(
        <<"db_checkout">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            poolboy:checkout(db)
        end
    ).
checkin(Worker) ->
    ?with_span(
        <<"db_checkin">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            poolboy:checkin(db, Worker)
        end
    ).

%%% DB direct calls

connect() ->
    Host = folio_config:pg(host, "localhost"),
    User = folio_config:pg(user, "folio_admin"),
    DB = folio_config:pg(database, "folio"),
    Port = folio_config:pg(port, "5432"),
    ?LOG_INFO(#{
        message => "Connecting to database",
        host => Host,
        user => User,
        port => Port,
        database => DB
    }),

    {ok, Conn} = epgsql:connect(#{
        host => Host,
        username => User,
        password => folio_config:pg(password, "pass"),
        database => DB,
        port => Port,
        timeout => 4000
    }),
    {ok, Conn}.

close(C) ->
    epgsql:close(C).

schema() ->
    [
        #{
            type => table,
            name => "assets",
            columns => [
                #{name => "external_id", type => "text"},
                #{name => "symbol", type => "text"},
                #{name => "source", type => "text"},
                #{name => "name", type => "text"}
            ],
            primary_key => ["external_id", "source"]
        },
        #{
            type => table,
            name => "asset_prices",
            columns => [
                #{name => "source", type => "text"},
                #{name => "symbol", type => "text"},
                #{name => "external_id", type => "text"},
                #{name => "amount", type => "numeric"},
                #{name => "timestamp", type => "timestamp"},
                #{name => "fiat_symbol", type => "text"}
            ],
            primary_key => ["source", "external_id", "symbol", "fiat_symbol", "timestamp"]
        },
        #{
            type => view,
            name => "v_current_asset_prices",
            column_names => [
                "source",
                "external_id",
                "symbol",
                "fiat_symbol",
                "timestamp",
                "amount"
            ],
            query =>
                lists:join(
                    " ",
                    [
                        "SELECT",
                        "  ap.source AS source,",
                        "  ap.external_id AS external_id,",
                        "  ap.symbol AS symbol,",
                        "  ap.fiat_symbol AS fiat_symbol,",
                        "  ap.timestamp AS timestamp,",
                        "  ap.amount as amount",
                        "FROM",
                        "  (SELECT",
                        "    source,",
                        "    external_id,",
                        "    symbol,",
                        "    fiat_symbol,",
                        "    max(timestamp) AS timestamp",
                        "  FROM asset_prices",
                        "  WHERE",
                        "    source = 'cryptowatch'",
                        "  GROUP BY (",
                        "    source,",
                        "    external_id,",
                        "    symbol,",
                        "    fiat_symbol)) AS mrap",
                        "LEFT JOIN",
                        "  asset_prices ap",
                        "ON",
                        "  mrap.source = ap.source AND",
                        "  mrap.external_id = ap.external_id AND",
                        "  mrap.symbol = ap.symbol AND",
                        "  mrap.timestamp = ap.timestamp",
                        ";"
                    ]
                )
        },
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
            name => "integration_sync_states",
            columns => [
                #{name => "integration_id", type => "uuid"},
                #{name => "timestamp", type => "timestamp"},
                #{name => "state", type => "text"}
            ],
            primary_key => ["integration_id", "timestamp"]
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
                #{name => "balance", type => "numeric"},
                #{name => "provider_asset_id", type => "text"}
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
                #{name => "line", type => "text"},
                #{name => "direction", type => "text"},
                #{name => "timestamp", type => "timestamp"},
                #{name => "symbol", type => "text"},
                #{name => "amount", type => "numeric"},
                #{name => "type", type => "text"},
                #{name => "description", type => "text"},
                #{name => "provider_asset_id", type => "text"}
            ],
            primary_key => ["integration_id", "external_id", "source_id", "line"]
        },
        #{
            type => table,
            name => "provider_asset_id_mapping",
            columns => [
                #{name => "provider_name", type => "text"},
                #{name => "provider_asset_id", type => "text"},
                #{name => "asset_source", type => "text"},
                #{name => "asset_external_id", type => "text"}
            ],
            primary_key => [
                "provider_name", "provider_asset_id", "asset_source"
            ]
        },

        #{
            type => view,
            name => "v_annotated_transactions",
            column_names => [
                "integration_id",
                "external_id",
                "provider_name",
                "source_id",
                "line",
                "direction",
                "timestamp",
                "symbol",
                "amount",
                "type",
                "description"
            ],
            query =>
                lists:join(
                    " ",
                    [
                        "SELECT",
                        "iat.integration_id,",
                        "iat.external_id as account_id,",
                        "i.provider_name as provider_name,",
                        "iat.source_id as source_id,",
                        "iat.line as line,",
                        "iat.direction as direction,",
                        "iat.timestamp as timestamp,",
                        "iat.symbol as symbol,",
                        "iat.amount as amount,",
                        "iat.type as type,",
                        "iat.description as description",
                        "FROM integrations i",
                        "JOIN integration_account_transactions iat",
                        "ON i.id = iat.integration_id;"
                    ]
                )
        },
        #{
            type => view,
            name => "v_annotated_account_balances",
            column_names => [
                "provider_name",
                "integration_id",
                "external_id",
                "asset_balance",
                "symbol",
                "fiat_symbol",
                "last_price_timestamp",
                "fiat_value"
            ],
            query =>
                lists:join(
                    " ",
                    [
                        "SELECT",
                        "  i.provider_name AS provider_name,",
                        "  iab.integration_id AS integration_id,",
                        "  iab.external_id AS external_id,",
                        "  iab.balance AS asset_balance,",
                        "  iab.symbol AS symbol,",
                        "  cap.fiat_symbol AS fiat_symbol,",
                        "  cap.timestamp AS last_price_timestamp,",
                        "  cap.amount * iab.balance AS fiat_value",
                        "",
                        "FROM",
                        "  integration_account_balances IAB",
                        "JOIN",
                        "  integrations i on iab.integration_id = i.id",
                        "LEFT JOIN",
                        "  provider_asset_id_mapping paim ON",
                        "    i.provider_name = paim.provider_name AND",
                        "    iab.provider_asset_id = paim.provider_asset_id",
                        "LEFT JOIN",
                        "  v_current_asset_prices cap",
                        "ON",
                        "  paim.asset_source = cap.source AND",
                        "  paim.asset_external_id = cap.external_id",
                        ";"
                    ]
                )
        },
        #{
            type => view,
            name => "v_assets",
            column_names => [
                "source",
                "external_id",
                "symbol",
                "name",
                "last_price",
                "fiat_symbol",
                "last_price_timestamp"
            ],
            query =>
                lists:join(
                    " ",
                    [
                        "SELECT",
                        "  a.source AS source,",
                        "  a.external_id AS external_id,",
                        "  a.symbol AS symbol,",
                        "  a.name AS name,",
                        "  cap.amount as amount,",
                        "  cap.fiat_symbol AS fiat_symbol,",
                        "  cap.timestamp AS timestamp",
                        "FROM",
                        "  assets a",
                        "LEFT JOIN",
                        "  v_current_asset_prices cap",
                        "ON",
                        "  a.source = cap.source AND",
                        "  a.external_id = cap.external_id AND",
                        "  a.symbol = cap.symbol",
                        ";"
                    ]
                )
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
        "SELECT * FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_schema NOT IN ('information_schema', 'pg_catalog');",
    {ok, C, D} = epgsql:squery(DBRef, Statement),
    {ok, serialise(C, D)}.

get_views(DBRef) ->
    Statement =
        "SELECT * FROM information_schema.tables WHERE table_type = 'VIEW' AND table_schema NOT IN ('information_schema', 'pg_catalog');",
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
    DefinedViews = lists:filter(
        fun(#{type := Type}) when is_atom(Type) ->
            Type == view
        end,
        Schema
    ),

    {ok, ExistingTables} = get_tables(Conn),
    {ok, ExistingViews} = get_views(Conn),
    {_, TablesToRemove} = set_differences(
        DefinedTables,
        fun(#{name := N}) -> erlang:list_to_binary(N) end,
        ExistingTables,
        fun(#{table_name := N}) -> N end
    ),
    {_, ViewsToRemove} = set_differences(
        DefinedViews,
        fun(#{name := N}) -> erlang:list_to_binary(N) end,
        ExistingViews,
        fun(#{table_name := N}) -> N end
    ),

    CreateAndModifyStatements = determine_migrations(Conn, Schema),
    DropTableStatements = lists:map(
        fun(#{table_name := NBin}) ->
            Name = erlang:binary_to_list(NBin),
            drop_table_statement(Name)
        end,
        TablesToRemove
    ),
    DropViewsStatements = lists:map(
        fun(#{table_name := NBin}) ->
            Name = erlang:binary_to_list(NBin),
            drop_view_statement(Name)
        end,
        ViewsToRemove
    ),

    Statements = CreateAndModifyStatements ++ DropTableStatements ++ DropViewsStatements,

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
    ?with_span(
        <<"database_write">>,
        #{attributes => #{table => Table}},
        fun(_Ctx) ->
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
            end
        end
    ).

val_to_operator({'<', V}) -> {" < ", V};
val_to_operator({'<=', V}) -> {" <= ", V};
val_to_operator({'>', V}) -> {" > ", V};
val_to_operator({'>=', V}) -> {" >= ", V};
val_to_operator(V) -> {" = ", V}.

extra_to_sql({limit, N}) when is_integer(N) ->
    [" LIMIT ", erlang:integer_to_list(N)];
extra_to_sql({order_by, Name, asc}) ->
    [" ORDER BY ", erlang:atom_to_list(Name), " ASC"];
extra_to_sql({order_by, Name, desc}) ->
    [" ORDER BY ", erlang:atom_to_list(Name), " DESC"].

-spec select(epgsql:connection(), atom() | list(), map() | list(), list()) ->
    {ok, list(map())}.
select(Conn, Table, Data, Extras) when is_atom(Table), is_map(Data) ->
    ?with_span(
        <<"database_select">>,
        #{attributes => #{table => Table}},
        fun(_Ctx) ->
            Fun = fun(K, V, {Count, Statements, Vals}) ->
                FCount = Count + 1,
                {SQLOperator, Val} = val_to_operator(V),
                Statement = lists:flatten([
                    erlang:atom_to_list(K), SQLOperator, "$", erlang:integer_to_list(FCount)
                ]),
                FStatement = Statements ++ [Statement],
                FVal = Vals ++ [Val],
                {FCount, FStatement, FVal}
            end,
            {_C, QueryStatements, SQLVals} = maps:fold(Fun, {0, [], []}, Data),
            QueryStatement = lists:join(" AND ", QueryStatements),
            Where =
                case QueryStatement of
                    [] -> "";
                    Else -> [" WHERE ", Else]
                end,

            ExtraSQL = lists:map(fun extra_to_sql/1, Extras),

            Statement = lists:flatten([
                "SELECT * FROM ",
                erlang:atom_to_list(Table),
                Where,
                ExtraSQL,
                ";"
            ]),
            select(Conn, Statement, SQLVals)
        end
    ).

-spec select(epgsql:connection(), atom() | list(), map() | list()) -> {ok, list(map())}.
select(Conn, Table, Data) when is_atom(Table), is_map(Data) ->
    ?with_span(
        <<"database_select">>,
        #{attributes => #{table => Table}},
        fun(_Ctx) ->
            select(Conn, Table, Data, [])
        end
    );
select(Conn, Statement, PositionalValues) when is_list(Statement), is_list(PositionalValues) ->
    ?with_span(
        <<"database_select_sql">>,
        #{},
        fun(_Ctx) ->
            case epgsql:equery(Conn, Statement, PositionalValues) of
                {ok, RetCols, RetData} ->
                    {ok, serialise(RetCols, RetData)}
            end
        end
    ).

delete(Conn, Table, Data) when is_atom(Table) ->
    ?with_span(
        <<"database_delete">>,
        #{attributes => #{table => Table}},
        fun(_Ctx) ->
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
            epgsql:equery(Conn, Statement, SQLVals)
        end
    ).

determine_migrations(Conn, Schema) ->
    lists:foldl(fun(I, Acc) -> Acc ++ determine_migration(Conn, I) end, [], Schema).

determine_migration(
    _Conn, #{type := view, name := ViewName, column_names := CNs, query := Q}
) ->
    ?LOG_INFO(#{
        message => view_info,
        view => ViewName
    }),
    create_or_replace_view(ViewName, CNs, Q);
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

create_or_replace_view(Name, Columns, Query) ->
    ColumnsString = ["( ", lists:join(", ", Columns), " )"],

    [lists:flatten(["CREATE OR REPLACE VIEW ", Name, " ", ColumnsString, " AS ", Query])].

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

drop_view_statement(ViewName) ->
    lists:flatten(["DROP VIEW ", ViewName, ";"]).

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
