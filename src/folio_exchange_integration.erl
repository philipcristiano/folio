-module(folio_exchange_integration).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([integrations/0]).
-export([integration_accounts/1]).

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
