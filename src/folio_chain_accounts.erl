-module(folio_chain_accounts).

-export([add/1, add/2, account_balances/0, account_balances/1, sync_accounts/0]).

-export_type([account/0]).
-type account() :: #{
    address := binary(),
    chain := binary(),
    type := binary()
}.

-spec add(account()) -> ok.
add(Account) ->
    {ok, C} = fdb:connect(),
    add(C, Account).

-spec add(epgsql:connection(), account()) -> ok.
add(C, Account = #{address := _Addr, chain := _Chain, type := _Type}) ->
    {ok, _} = fdb:write(C, chain_accounts, Account),
    ok.

account_balances() ->
    {ok, C} = fdb:connect(),
    account_balances(C).

account_balances(C) ->
    Query = "select address as id, chain as symbol from chain_accounts as accounts;",
    {ok, Accounts} = fdb:select(C, Query, []),
    {ok, Accounts}.

sync_accounts() ->
    folio_fetcher:sync().
