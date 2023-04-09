-module(folio_integration).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    providers/0,
    provider_by_name/1,
    provider_setup_properties/1,
    add_integration/2,
    delete_integration/2
]).
-export([
    fetch_integration_accounts/1,
    fetch_integration_account_transactions/2, fetch_integration_account_transactions/3
]).

-export([integrations/0, integrations/1, integration_by_id/2]).
-export([set_integration_state/2, get_integration_state/1, annotate_with_state/1]).
-export([integration_accounts/1, integration_accounts/2]).

-export([transactions/1]).

-export_type([state/0]).
-type state() :: any().

-export_type([account/0]).
-type account() :: #{
    id := binary(),
    balances := list(account_balance())
}.

-type account_balance() :: #{
    balance := decimal:decimal(),
    symbol := binary()
}.

-export_type([id/0]).
-type id() :: binary().

-export_type([integration/0]).
-type integration() :: #{
    id := binary(),
    provider_name := binary()
}.

-export_type([integration_sync_state/0]).
-type integration_sync_state() :: starting | running | complete | error.

-export_type([account_transactions/0]).
-type account_transactions() :: list(account_transaction).

-export_type([account_transaction/0]).
-type account_transaction() :: #{
    source_id := binary(),
    line := binary(),
    datetime := calendar:datetime(),
    direction := in | out,
    symbol := binary(),
    amount := decimal:decimal(),
    type := fee | undefined,
    description := binary()
}.

-export_type([provider/0]).
-type provider() :: #{
    name := binary(),
    type := exchange | chain,
    mod := atom()
}.

-export_type([setup_property_config/0]).
-type setup_property_config() :: #{
    fields := map()
}.

-type completeness() :: incomplete | complete.

%%%
%
%  Exchange Integration Behavior
%
%%%

-callback accounts_init(integration()) -> state().
-callback add(id(), map()) -> ok.
-callback accounts(state()) -> {completeness(), list(account()), state()}.
-callback setup_properties() -> list(setup_property_config()).
-callback account_transactions_init(integration(), account()) -> state().
-callback account_transactions(state()) -> {completeness(), account_transactions(), state()}.

%%%

providers() ->
    [
        #{
            name => <<"coinbase">>,
            type => exchange,
            mod => folio_coinbase_api
        },
        #{
            name => <<"coinbase_pro">>,
            type => exchange,
            mod => folio_coinbase_pro_api
        },
        #{
            name => <<"bitcoin">>,
            type => chain,
            mod => folio_blockstream
        },
        #{
            name => <<"gemini">>,
            type => exchange,
            mod => folio_gemini_api
        },
        #{
            name => <<"ethereum">>,
            type => chain,
            mod => folio_ethplorer
        },
        #{
            name => <<"loopring">>,
            type => chain,
            mod => folio_loopring
        }
    ].

-spec provider_by_name(binary()) -> provider().
provider_by_name(Name) ->
    Ints = providers(),
    case lists:search(fun(#{name := N}) -> N == Name end, Ints) of
        {value, V} -> V;
        _ -> throw(not_found)
    end.

-spec provider_setup_properties(binary()) -> map().
provider_setup_properties(Name) ->
    #{mod := Mod} = provider_by_name(Name),
    Props = Mod:setup_properties(),
    Props.

-spec add_integration(binary(), map()) -> {ok, integration()}.
add_integration(Name, AccountProperties) ->
    #{mod := Mod} = provider_by_name(Name),
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

-spec fetch_integration_accounts(integration()) -> any().
fetch_integration_accounts(Integration = #{provider_name := PN}) ->
    #{mod := Mod} = provider_by_name(PN),
    InitState = Mod:accounts_init(Integration),
    {ok, collect_accounts([], Mod, InitState)}.

integrations() ->
    C = fdb:checkout(),
    R = integrations(C),
    fdb:checkin(C),
    R.

-spec integrations(epgsql:connection()) -> {ok, [integration()]}.
integrations(C) ->
    {ok, A} = fdb:select(C, integrations, #{}),
    {ok, A}.

-spec integration_by_id(epgsql:connection(), id()) -> {ok, integration()}.
integration_by_id(C, ID) ->
    {ok, [I]} = fdb:select(C, integrations, #{id => ID}),
    {ok, I}.

-spec integration_accounts(epgsql:connection()) -> {ok, [account()]}.
integration_accounts(C) ->
    ?with_span(
        <<"integration_accounts">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            Query =
                "SELECT iab.integration_id as integration_id, iab.symbol as symbol, iab.balance as balance, iab.external_id as external_id FROM integration_account_balances As iab WHERE iab.balance > 0;",
            {ok, A} = fdb:select(C, Query, []),
            {ok, A}
        end
    ).

-spec integration_accounts(epgsql:connection(), id()) -> {ok, [account()]}.
integration_accounts(C, IntegrationID) ->
    ?with_span(
        <<"integration_accounts">>,
        #{attributes => #{integration_id => IntegrationID}},
        fun(_Ctx) ->
            Query =
                "SELECT iab.integration_id as integration_id, iab.symbol as symbol, iab.balance as balance, iab.external_id as external_id FROM integration_account_balances As iab WHERE iab.integration_id = $1 AND iab.balance > 0;",
            {ok, A} = fdb:select(C, Query, [IntegrationID]),
            %{ok, A} = fdb:select(C, integration_accounts, #{integration_id => IntegrationID}),
            {ok, A}
        end
    ).

-spec transactions(epgsql:connection()) -> {ok, [account_transactions()]}.
transactions(C) ->
    Query =
        "SELECT * FROM integration_account_transactions as iat JOIN integrations AS i ON iat.integration_id = i.id ORDER BY timestamp DESC;",
    {ok, A} = fdb:select(C, Query, []),
    {ok, A}.

collect_accounts(List, Mod, State) ->
    Resp = ?with_span(
        <<"integration_iteration">>,
        #{attributes => #{mod => Mod}},
        fun(_Ctx) ->
            Mod:accounts(State)
        end
    ),

    case Resp of
        {incomplete, NewAccounts, NewState} ->
            NewList = List ++ NewAccounts,
            collect_accounts(NewList, Mod, NewState);
        {complete, NewAccounts, _NewState} ->
            NewList = List ++ NewAccounts,
            NewList
    end.

-spec fetch_integration_account_transactions(integration(), account()) -> {ok, list()}.
fetch_integration_account_transactions(Integration = #{provider_name := PN}, Account) ->
    #{mod := Mod} = provider_by_name(PN),
    InitState = Mod:account_transactions_init(Integration, Account),
    {ok, collect_account_transactions([], Mod, InitState)}.

-spec fetch_integration_account_transactions(any(), integration(), account()) -> ok.
fetch_integration_account_transactions(Callback, Integration = #{provider_name := PN}, Account) ->
    #{mod := Mod} = provider_by_name(PN),
    InitState = Mod:account_transactions_init(Integration, Account),
    collect_account_transactions(Callback, Mod, InitState).

collect_account_transactions(List, Mod, State) when is_list(List) ->
    Resp = ?with_span(
        <<"integration_iteration">>,
        #{attributes => #{mod => Mod}},
        fun(_Ctx) ->
            Mod:account_transactions(State)
        end
    ),

    case Resp of
        {incomplete, NewTXs, NewState} ->
            NewList = List ++ NewTXs,
            collect_account_transactions(NewList, Mod, NewState);
        {complete, NewTXs, _NewState} ->
            NewList = List ++ NewTXs,
            NewList
    end;
collect_account_transactions(Callback, Mod, State) when is_function(Callback) ->
    Resp = ?with_span(
        <<"integration_iteration">>,
        #{attributes => #{mod => Mod}},
        fun(_Ctx) ->
            Mod:account_transactions(State)
        end
    ),

    case Resp of
        {incomplete, NewTXs, NewState} ->
            Callback(NewTXs),
            collect_account_transactions(Callback, Mod, NewState);
        {complete, NewTXs, _NewState} ->
            Callback(NewTXs),
            ok
    end.

new_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).
