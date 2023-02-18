-module(folio_exchange_integration).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([integrations/0]).
-export([integration_accounts/1, integration_account_transactions/2]).

-export_type([state/0]).
-type state() :: any().

-export_type([account/0]).
-type account() :: #{
    id := binary(),
    symbol := binary()
}.

-type account_transactions() :: #{}.

-type completeness() :: incomplete | complete.

%%%
%
%  Exchange Integration Behavior
%
%%%

-callback accounts_init() -> state().
-callback accounts(state()) -> {completeness(), list(account()), state()}.

-callback account_transactions_init(account()) -> state().
-callback account_transactions(state()) -> {completeness(), list(account_transactions()), state()}.

%%%

integrations() ->
    [
        #{
            name => <<"coinbase">>,
            mod => folio_coinbase_api
        }
    ].

integration_accounts(Mod) ->
    InitState = Mod:accounts_init(),
    {ok, collect_accounts([], Mod, InitState)}.

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

-spec integration_account_transactions(atom(), account()) -> {ok, list()}.
integration_account_transactions(Mod, Account) ->
    InitState = Mod:account_transactions_init(Account),
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
