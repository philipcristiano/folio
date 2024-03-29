-module(folio_integration).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    add_integration/2,
    delete_integration/2
]).

-export([integrations/1, integration_by_id/2]).
-export([set_integration_state/2, get_integration_state/1, annotate_with_state/1]).

-export([
    write_account/3, write_accounts/3, write_account_transaction/4, write_account_transactions/4
]).

-export([transactions/1, transactions/2]).

-export_type([account/0]).
-type account() :: #{
    id := binary(),
    balances := list(account_balance())
}.

-type account_balance() :: #{
    balance := decimal:decimal(),
    asset := external_asset()
}.

-export_type([integration/0]).
-type integration() :: #{
    id := binary(),
    provider_name := binary()
}.

-export_type([id/0]).
-type id() :: binary().

-export_type([transaction_direction/0]).
-type transaction_direction() :: in | out.

-export_type([integration_sync_state/0]).
-type integration_sync_state() :: starting | running | complete | error.

-export_type([integration_account_transactions/0]).
-type integration_account_transactions() :: list(integration_account_transaction).

-export_type([external_asset/0]).
-type external_asset() ::
    #{
        symbol => binary()
    }
    | #{symbol => binary(), id => binary()}.

-export_type([integration_account_transaction/0]).
-type integration_account_transaction() :: #{
    source_id := binary(),
    line := binary(),
    datetime := calendar:datetime(),
    direction := transaction_direction(),
    asset := external_asset(),
    symbol := binary(),
    amount := decimal:decimal(),
    type := fee | undefined,
    description := binary()
}.

-export_type([account_transactions/0]).
-type account_transactions() :: list(account_transaction).

-export_type([account_transaction/0]).
-type account_transaction() :: #{
    integration_id := id(),
    external_id := binary(),
    line := binary(),
    datetime := calendar:datetime(),
    direction := transaction_direction(),
    symbol := binary(),
    amount := decimal:decimal(),
    type := fee | undefined,
    description := binary()
}.

-spec add_integration(binary(), map()) -> {ok, integration()}.
add_integration(Name, AccountProperties) ->
    #{mod := Mod} = folio_provider:provider_by_name(Name),
    IntegrationID = new_id(),
    Integration = #{id => IntegrationID, provider_name => Name},

    C = fdb:checkout(),
    {ok, _} = fdb:write(C, integrations, Integration),
    fdb:checkin(C),

    ok = Mod:add(IntegrationID, AccountProperties),
    {ok, Integration}.

-spec delete_integration(epgsql:connection(), id()) -> ok.
delete_integration(C, ID) ->
    IntegrationQuery = #{id => ID},
    RelatedQuery = #{integration_id => ID},
    {ok, _} = fdb:delete(C, integrations, IntegrationQuery),
    {ok, _} = fdb:delete(C, integration_credentials, RelatedQuery),
    {ok, _} = fdb:delete(C, integration_accounts, RelatedQuery),
    {ok, _} = fdb:delete(C, integration_account_balances, RelatedQuery),
    {ok, _} = fdb:delete(C, integration_account_transactions, RelatedQuery),
    ok.

-spec set_integration_state(integration(), integration_sync_state()) -> ok.
set_integration_state(#{id := ID}, State) when is_atom(State) ->
    Now = qdate:to_date(os:system_time(second)),
    D = #{
        integration_id => ID,
        timestamp => Now,
        state => erlang:atom_to_binary(State)
    },

    C = fdb:checkout(),
    {ok, _} = fdb:write(C, integration_sync_states, D),
    fdb:checkin(C),
    ok.

-spec get_integration_state(integration()) -> {ok | error, integration_sync_state()}.
get_integration_state(#{id := ID}) ->
    C = fdb:checkout(),
    {ok, Resp} = fdb:select(C, integration_sync_states, #{integration_id => ID}, [
        {order_by, timestamp, desc}, {limit, 1}
    ]),
    fdb:checkin(C),

    case Resp of
        [] -> {ok, undefined};
        [#{state := State}] -> {ok, State};
        R -> {error, R}
    end.

annotate_with_state(Int = #{id := _ID}) ->
    {ok, State} = get_integration_state(Int),
    Int#{state => State};
annotate_with_state(List) when is_list(List) ->
    lists:map(fun annotate_with_state/1, List).

-spec integrations(epgsql:connection()) -> {ok, [integration()]}.
integrations(C) ->
    {ok, A} = fdb:select(C, integrations, #{}),
    {ok, A}.

-spec integration_by_id(epgsql:connection(), id()) -> {ok, integration()}.
integration_by_id(C, ID) ->
    {ok, [I]} = fdb:select(C, integrations, #{id => ID}),
    {ok, I}.

-spec transactions(epgsql:connection()) -> {ok, [account_transactions()]}.
transactions(C) ->
    transactions(C, #{}).

-spec transactions(epgsql:connection(), map()) -> {ok, [account_transactions()]}.
transactions(C, Filters) ->
    {ok, T} = fdb:select(C, v_annotated_transactions, Filters, [
        {order_by, timestamp, desc}
    ]),
    {ok, T}.

-spec write_account(epgsql:connection(), integration(), account()) -> ok.
write_account(C, #{id := IntegrationID}, #{id := ID, balances := Balances}) ->
    AData = #{external_id => ID, integration_id => IntegrationID},
    {ok, _} = fdb:write(C, integration_accounts, AData),
    lists:foreach(
        fun(#{balance := Balance, asset := Asset}) ->
            AssetInfo = external_asset_to_asset_info(Asset),
            BalData = #{
                integration_id => IntegrationID,
                external_id => ID,
                balance => decimal:to_binary(Balance)
            },
            WriteBalData = maps:merge(BalData, AssetInfo),
            ?LOG_DEBUG(#{
                message => write_cb_account,
                account_data => AData,
                balance_data => WriteBalData
            }),
            {ok, _} = fdb:write(C, integration_account_balances, WriteBalData)
        end,
        Balances
    ),
    ok.

write_accounts(C, Integration = #{id := IntegrationID}, Accounts) ->
    ?with_span(
        <<"write_accounts">>,
        #{attributes => #{integration_id => IntegrationID}},
        fun(_Ctx) ->
            lists:foreach(
                fun(Account) ->
                    folio_integration:write_account(C, Integration, Account)
                end,
                Accounts
            )
        end
    ),
    ok.

-spec write_account_transaction(
    epgsql:connection(), integration(), account(), integration_account_transaction()
) -> {ok, map()}.

write_account_transaction(C, #{id := IntegrationID}, #{id := AccountID}, #{
    source_id := SourceID,
    line := Line,
    datetime := DT,
    direction := Direction,
    asset := Asset,
    amount := Amount,
    type := Type,
    description := Description
}) ->
    AssetInfo = external_asset_to_asset_info(Asset),
    Data = #{
        integration_id => IntegrationID,
        external_id => AccountID,
        source_id => SourceID,
        line => Line,
        timestamp => DT,
        direction => Direction,
        amount => decimal:to_binary(Amount),
        type => Type,
        description => Description
    },
    WriteData = maps:merge(Data, AssetInfo),
    fdb:write(C, integration_account_transactions, WriteData).

external_asset_to_asset_info(#{symbol := S, id := ID}) ->
    #{symbol => S, provider_asset_id => ID};
external_asset_to_asset_info(#{symbol := S}) ->
    #{symbol => S}.

-spec write_account_transactions(
    epgdql:connection(),
    folio_integration:integration(),
    folio_integration:account(),
    folio_integration:account_transactions()
) -> ok.
write_account_transactions(
    C,
    Integration = #{id := IntegrationID},
    Account = #{id := AccountID},
    Transactions
) ->
    ?with_span(
        <<"write_account_transactions">>,
        #{attributes => #{integration_id => IntegrationID, account_id => AccountID}},
        fun(_Ctx) ->
            lists:foreach(
                fun(T) ->
                    {ok, _} = folio_integration:write_account_transaction(
                        C, Integration, Account, T
                    )
                end,
                Transactions
            )
        end
    ),
    ok.

new_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).
