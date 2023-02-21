-module(folio_accounts).

-export([account_balances/0, sync_accounts/0]).

account_balances() ->
    Query =
        "select * from coinbase_accounts as accounts join coinbase_account_balances as balances on accounts.id = balances.account_id WHERE balances.balance > 0 ORDER BY balances.balance DESC",
    {ok, C} = fdb:connect(),
    {ok, Accounts} = fdb:select(C, Query, []),
    {ok, Accounts}.

sync_accounts() ->
    folio_fetcher:sync().
