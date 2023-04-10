-module(folio_account_provider).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    fetch_integration_accounts/1,
    fetch_integration_account_transactions/2, fetch_integration_account_transactions/3
]).

%%%
%
%  account_provider Behavior
%
%%%

-callback accounts_init(folio_integration:integration()) -> state().
-callback add(id(), map()) -> ok.
-callback accounts(state()) -> {completeness(), list(folio_integration:account()), state()}.
-callback setup_properties() -> list(folio_provider:setup_property_config()).
-callback account_transactions_init(folio_integration:integration(), folio_integration:account()) ->
    state().
-callback account_transactions(state()) ->
    {completeness(), folio_integration:integration_account_transactions(), state()}.

-type completeness() :: incomplete | complete.

-export_type([state/0]).
-type state() :: any().

-export_type([id/0]).
-type id() :: binary().

-spec fetch_integration_accounts(folio_integration:integration()) -> any().
fetch_integration_accounts(Integration = #{provider_name := PN}) ->
    #{mod := Mod} = folio_provider:provider_by_name(PN),
    InitState = Mod:accounts_init(Integration),
    {ok, collect_accounts([], Mod, InitState)}.

-spec fetch_integration_account_transactions(
    folio_integration:integration(), folio_integration:account()
) -> {ok, list()}.
fetch_integration_account_transactions(Integration = #{provider_name := PN}, Account) ->
    #{mod := Mod} = folio_provider:provider_by_name(PN),
    InitState = Mod:account_transactions_init(Integration, Account),
    {ok, collect_account_transactions([], Mod, InitState)}.

-spec fetch_integration_account_transactions(
    any(), folio_integration:integration(), folio_integration:account()
) -> ok.
fetch_integration_account_transactions(Callback, Integration = #{provider_name := PN}, Account) ->
    #{mod := Mod} = folio_provider:provider_by_name(PN),
    InitState = Mod:account_transactions_init(Integration, Account),
    collect_account_transactions(Callback, Mod, InitState).

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
