-module(folio_handler_accounts).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    folio_coinbase_api:run(),
    {ok, Req0, State}.
