-module(folio_http_session).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    init/1,
    pre_req_jwt_cookie/4,
    set_jwt_cookie/2,
    unset_jwt_cookie/1,
    set_otel_context/0
]).

pre_req_jwt_cookie(Req, Params, Body, State) ->
    JWTKey = bizops_config:jwt_key(),
    Cookies = cowboy_req:parse_cookies(Req),
    ?LOG_DEBUG(#{message => cookies, cookies => Cookies}),
    case lists:keyfind(<<"bzjwt">>, 1, Cookies) of
        {_, Token} ->
            ValidClaims =
                case jwt:decode(Token, JWTKey) of
                    {ok, Claims} ->
                        ?LOG_INFO(#{message => <<"Claims">>, claims => Claims}),
                        ?set_attributes(Claims),
                        Claims;
                    {error, invalid_token} ->
                        ?LOG_WARNING(#{
                            message => "Token login failed",
                            token => Token
                        }),
                        undefined
                end,
            {Req, Params, Body, State#{claims => ValidClaims}};
        false ->
            {Req, Params, Body, State}
    end.

set_jwt_cookie(Req, Claims) ->
    JWTKey = bizops_config:jwt_key(),
    {ok, Token} = jwt:encode(<<"HS256">>, Claims, JWTKey),
    cowboy_req:set_resp_cookie(<<"bzjwt">>, Token, Req, #{
        http_only => true,
        max_age => 3600,
        path => <<"/">>
    }).

unset_jwt_cookie(Req) ->
    cowboy_req:set_resp_cookie(<<"bzjwt">>, <<"">>, Req, #{
        http_only => true,
        max_age => 0,
        path => <<"/">>
    }).

-dialyzer({nowarn_function, set_otel_context/0}).

init(Req) ->
    % Set trace value for GCP
    case cowboy_req:header(<<"x-cloud-trace-context">>, Req) of
        undefined ->
            ok;
        Value ->
            [TraceID, _Rest] = binary:split(Value, <<"/">>),
            ProjectID = bizops_config:gcp_project_id(),
            Trace =
                <<<<"projects/">>/binary, ProjectID/binary, <<"/traces/">>/binary, TraceID/binary>>,
            logger:update_process_metadata(#{<<"logging.googleapis.com/trace">> => Trace})
    end,

    set_otel_context(),
    ok.

-spec set_otel_context() -> ok.
set_otel_context() ->
    Ctx = opentelemetry_process_propagator:fetch_parent_ctx(),
    % There is a type conflict here and catching will prevent the error from popping up through the dialyzer stack
    % https://github.com/open-telemetry/opentelemetry-erlang/issues/443
    try
        otel_ctx:attach(Ctx)
    catch
        _ ->
            ?LOG_INFO(#{message => otel_attach_error}),
            ok
    end.
