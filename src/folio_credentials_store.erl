-module(folio_credentials_store).

-include_lib("kernel/include/logger.hrl").

-export([get_credentials/1, set_credentials/2]).

-spec get_credentials(folio_integration:id()) -> map() | undefined.
get_credentials(ID) ->
    C = fdb:checkout(),
    {ok, [#{credentials := CredJson}]} = fdb:select(C, integration_credentials, #{
        integration_id => ID
    }),
    fdb:checkin(C),
    Creds = jsx:decode(CredJson, [return_maps]),
    map_keys_to_atoms(Creds).

-spec set_credentials(folio_integration:id(), map()) -> ok.
set_credentials(ID, Creds) ->
    CredJson = jsx:encode(Creds),
    C = fdb:checkout(),
    {ok, _} = fdb:write(C, integration_credentials, #{integration_id => ID, credentials => CredJson}),
    fdb:checkin(C),
    ok.

map_keys_to_atoms(M) ->
    maps:fold(
        fun(K, V, AccIn) ->
            Ka = erlang:binary_to_atom(K),
            AccIn#{Ka => V}
        end,
        #{},
        M
    ).
