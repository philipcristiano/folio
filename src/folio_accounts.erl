-module(folio_accounts).

-export([account_balances/0, account_balances/1, sync_accounts/0]).

account_balances() ->
    {ok, C} = fdb:connect(),
    account_balances(C).

account_balances(C) ->
    Query =
        "select * from coinbase_accounts as accounts join coinbase_account_balances as balances on accounts.id = balances.account_id WHERE balances.balance > 0 ORDER BY balances.balance DESC",
    {ok, Accounts} = fdb:select(C, Query, []),
    {ok, Accounts}.

sync_accounts() ->
    folio_fetcher:sync().
