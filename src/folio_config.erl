-module(folio_config).

-export([
    domain/0,
    encryption_key/0,
    jwt_key/0,
    environment/0,
    hosting_port/0
]).

domain() ->
    read_secret("DOMAIN", <<"http://localhost:8000">>).

encryption_key() ->
    read_secret("ENCRYPTIION_KEY", <<"0000000000000001">>).

hosting_port() ->
    os:getenv("PORT", "8000").

environment() ->
    read_secret("ENVIRONMENT", <<"DEV">>).

jwt_key() ->
    read_secret("JWT_KEY", undefined).

read_secret(SecretName, Default) ->
    RootPath = os:getenv("SECRETS_PATH", "./secrets/"),
    Path = filename:join([RootPath, SecretName, "secret"]),

    case file:read_file(Path) of
        {ok, Value} -> trim_trailing_newline(Value);
        {error, enoent} -> Default
    end.

trim_trailing_newline(B) ->
    case binary:last(B) of
        10 -> binary:part(B, {0, byte_size(B) - 1});
        _ -> B
    end.