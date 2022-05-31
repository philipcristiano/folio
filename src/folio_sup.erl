-module(folio_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    TxStorage = #{
        id => folio_tx_storage,
        start => {folio_tx_storage, start_link, []}
    },

    Procs = [TxStorage],
    {ok, {{one_for_one, 1, 5}, Procs}}.
