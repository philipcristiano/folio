-module(folio_handler_accounts).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, _} = folio_coinbase_api:user(),
    {ok, Req0, State}.
