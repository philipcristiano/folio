-module(folio_integration).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([providers/0, provider_by_name/1, provider_setup_properties/1, add_integration/2]).
-export([fetch_integration_accounts/1, fetch_integration_account_transactions/2]).

-export([integrations/0, integrations/1]).
-export([integration_accounts/2]).

-export_type([state/0]).
-type state() :: any().

-export_type([account/0]).
-type account() :: #{
    id := binary(),
    symbol := binary(),
    balance := number()
}.

-export_type([id/0]).
-type id() :: binary().

-export_type([integration/0]).
-type integration() :: #{
    id := binary(),
    provider_name := binary()
}.

-type account_transactions() :: #{}.

-type completeness() :: incomplete | complete.

%%%
%
%  Exchange Integration Behavior
%
%%%

-callback accounts_init(integration()) -> state().
-callback add(id(), map()) -> ok.
-callback accounts(state()) -> {completeness(), list(account()), state()}.
-callback setup_properties() -> map().
-callback account_transactions_init(integration(), account()) -> state().
-callback account_transactions(state()) -> {completeness(), list(account_transactions()), state()}.

%%%

providers() ->
    [
        #{
            name => <<"coinbase">>,
            type => exchange,
            mod => folio_coinbase_api
        },
        #{
            name => <<"bitcoin">>,
            type => chain,
            mod => folio_blockstream
        }
    ].

-spec provider_by_name(binary()) -> false | map().
provider_by_name(Name) ->
    Ints = providers(),
    case lists:search(fun(#{name := N}) -> N == Name end, Ints) of
        {value, V} -> V;
        _ -> false
    end.

provider_setup_properties(Name) ->
    case provider_by_name(Name) of
        #{mod := Mod} ->
            Props = Mod:setup_properties(),
            Props;
        false ->
            false
    end.

-spec add_integration(binary(), map()) -> {ok, integration()} | false.
add_integration(Name, AccountProperties) ->
    case provider_by_name(Name) of
        _Int = #{mod := Mod} ->
            IntegrationID = new_id(),
            {ok, C} = fdb:connect(),
            Integration = #{id => IntegrationID, provider_name => Name},
            {ok, _} = fdb:write(C, integrations, Integration),
            ok = Mod:add(IntegrationID, AccountProperties),
            {ok, Integration};
        false ->
            false
    end.

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

-spec integration_accounts(epgsql:connection(), id()) -> {ok, [account()]}.
integration_accounts(C, IntegrationID) ->
    Query =
        "SELECT iab.integration_id as integration_id, iab.symbol as symbol, iab.balance as balance, iab.external_id as external_id FROM integration_account_balances As iab WHERE iab.integration_id = $1 AND iab.balance > 0;",
    io:format("Query ~p~n", [{Query, IntegrationID}]),
    {ok, A} = fdb:select(C, Query, [IntegrationID]),
    %{ok, A} = fdb:select(C, integration_accounts, #{integration_id => IntegrationID}),
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
    uuid:to_string(uuid:uuid4()).
