-module(folio_credentials_store).

-include_lib("kernel/include/logger.hrl").

-export([get_credentials/1]).

-spec get_credentials(atom()) -> map() | undefined.
get_credentials(Name) ->
    {ok, AppConfig} = application:get_env(folio, credentials),
    maps:get(Name, AppConfig, undefined).
