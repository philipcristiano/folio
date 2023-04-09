-module(folio_http).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    bad_request/2,
    make_string_property/1,
    make_delete/3,
    make_get/2,
    make_get/3,
    make_json_post/2,
    make_json_post/3,
    path_spec_for_name/1,
    request/5
]).

bad_request(Req, Message) ->
    {Req, 400, #{status => error, message => Message}, #{}}.

make_string_property(Name) ->
    #{
        type => string,
        description => Name
    }.

make_json_post(ID, Schema) ->
    make_json_post(ID, [], Schema).

make_json_post(ID, PathParams, Schema) ->
    ParamSpecs = path_param_names_to_param_spec(PathParams),
    #{
        <<"post">> => #{
            operationId => ID,
            tags => [],
            description => ID,
            parameters => ParamSpecs,
            requestBody => #{
                content => #{
                    'application/json' => #{
                        schema => Schema
                    }
                }
            },
            responses => #{
                200 => #{
                    description => <<"">>,
                    content => #{
                        'application/json' => #{}
                    }
                }
            }
        }
    }.

make_delete(ID, PathParams, ReturnSchema) ->
    ParamSpecs = path_param_names_to_param_spec(PathParams),
    #{
        <<"delete">> => #{
            operationId => ID,
            tags => [],
            description => <<"ID">>,
            parameters => ParamSpecs,
            responses => #{
                200 => #{
                    description => <<"Response">>,
                    content => #{
                        'application/json' => #{
                            schema => ReturnSchema
                        }
                    }
                }
            }
        }
    }.

make_get(ID, ReturnSchema) ->
    make_get(ID, [], ReturnSchema).

make_get(ID, PathParams, ReturnSchema) ->
    ParamSpecs = path_param_names_to_param_spec(PathParams),
    #{
        <<"get">> => #{
            operationId => ID,
            tags => [],
            description => <<"ID">>,
            parameters => ParamSpecs,
            responses => #{
                200 => #{
                    description => <<"Response">>,
                    content => #{
                        'application/json' => #{
                            schema => ReturnSchema
                        }
                    }
                }
            }
        }
    }.

path_param_names_to_param_spec(ParamNames) ->
    lists:map(fun path_spec_for_name/1, ParamNames).

path_spec_for_name(Name) ->
    #{
        in => path,
        name => Name,
        description => "Default description",
        required => true,
        schema => #{type => string}
    }.

-spec request(get | post, binary(), list(), any(), fun()) ->
    {ok, integer(), list(), term()} | {error, binary()}.
request(Method, URL, Headers, ReqBody, ErrorFun) ->
    ?LOG_DEBUG(#{
        message => http_request,
        url => URL
    }),
    ?with_span(
        <<"hackney request">>,
        #{
            attributes => #{
                <<"http.method">> => Method,
                <<"http.url">> => URL
            }
        },
        fun(_Ctx) ->
            case hackney:request(Method, URL, Headers, ReqBody, [with_body]) of
                {error, closed} ->
                    ErrorFun();
                {error, timeout} ->
                    ErrorFun();
                {ok, RespCode, RespHeaders, Body} ->
                    ?set_attributes([{<<"https.status_code">>, RespCode}]),
                    case jsx:is_json(Body) of
                        true -> {ok, RespCode, RespHeaders, jsx:decode(Body, [return_maps])};
                        false -> {error, Body}
                    end
            end
        end
    ).
