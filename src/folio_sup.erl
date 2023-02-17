-module(folio_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Fetcher = #{
        id => folio_fetcher,
        start => {folio_fetcher, start_link, []}
    },

    Procs = [Fetcher],
    {ok, {{one_for_one, 1, 5}, Procs}}.
