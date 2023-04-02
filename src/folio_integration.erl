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
-export([fetch_integration_accounts/1, fetch_integration_account_transactions/2]).

-export([integrations/0, integrations/1, integration_by_id/2]).
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

-export_type([account_transactions/0]).
-type account_transactions() :: list(account_transaction).

-export_type([account_transaction/0]).
-type account_transaction() :: #{
    source_id := binary(),
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

    {ok, C} = fdb:connect(),
    {ok, _} = fdb:write(C, integrations, Integration),
    fdb:close(C),

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

-spec fetch_integration_accounts(integration()) -> any().
fetch_integration_accounts(Integration = #{provider_name := PN}) ->
    #{mod := Mod} = provider_by_name(PN),
    InitState = Mod:accounts_init(Integration),
    {ok, collect_accounts([], Mod, InitState)}.

integrations() ->
    {ok, C} = fdb:connect(),
    integrations(C).

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
    Query =
        "SELECT iab.integration_id as integration_id, iab.symbol as symbol, iab.balance as balance, iab.external_id as external_id FROM integration_account_balances As iab WHERE iab.balance > 0;",
    {ok, A} = fdb:select(C, Query, []),
    {ok, A}.

-spec integration_accounts(epgsql:connection(), id()) -> {ok, [account()]}.
integration_accounts(C, IntegrationID) ->
    Query =
        "SELECT iab.integration_id as integration_id, iab.symbol as symbol, iab.balance as balance, iab.external_id as external_id FROM integration_account_balances As iab WHERE iab.integration_id = $1 AND iab.balance > 0;",
    {ok, A} = fdb:select(C, Query, [IntegrationID]),
    %{ok, A} = fdb:select(C, integration_accounts, #{integration_id => IntegrationID}),
    {ok, A}.

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

collect_account_transactions(List, Mod, State) ->
    Resp = ?with_span(
        <<"integration_iteration">>,
        #{attributes => #{mod => Mod}},
        fun(_Ctx) ->
            Mod:account_transactions(State)
        end
    ),

    case Resp of
        {incomplete, NewAccounts, NewState} ->
            NewList = List ++ NewAccounts,
            collect_account_transactions(NewList, Mod, NewState);
        {complete, NewAccounts, _NewState} ->
            NewList = List ++ NewAccounts,
            NewList
    end.

new_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).
