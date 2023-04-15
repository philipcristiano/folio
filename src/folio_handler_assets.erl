-module(folio_handler_assets).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

trails() ->
    Params = params(),
    Metadata = folio_http:make_get(getAssets, Params, return_schema()),
    State = #{},
    [
        trails:trail(<<"/api/assets">>, ?MODULE, State, Metadata)
    ].

params() ->
    lists:map(
        fun(N) ->
            #{
                in => query,
                name => N,
                description => "Filter by property",
                required => false,
                schema => #{type => string}
            }
        end,
        filters()
    ).

filters() ->
    [integration_id].

return_schema() ->
    #{
        type => object,
        description => <<"Assets">>,
        properties => #{
            <<"assets">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of assets">>
            }
        }
    }.

init(Req, Opts) ->
    folio_http_session:init(Req),
    {specified_handler, Req, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    Params,
    _Body,
    State
) ->
    ?LOG_INFO(#{
        message => params,
        params => Params
    }),
    Filters = maps:filter(fun(_K, V) -> V /= undefined end, Params),
    C = fdb:checkout(),
    {ok, Assets} = folio_assets:get_assets(C, Filters),
    fdb:checkin(C),

    {Req, 200, #{assets => Assets}, State}.

post_req(_Response, _State) ->
    ok.
