-module(folio_handler_integration_sync).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/integrations/:integration_id/sync">>).

trails() ->
    Metadata = folio_http:make_json_post(syncIntegration, [integration_id], empty_post()),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

empty_post() ->
    #{
        required => [],
        properties => #{}
    }.

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"POST">>},
    _Params = #{integration_id := IntegrationID},
    _Body,
    State
) ->
    {ok, C} = fdb:connect(),
    {ok, Integration} = folio_integration:integration_by_id(C, IntegrationID),
    fdb:close(C),
    folio_fetcher:sync(Integration),

    {Req, 202, #{}, State}.

post_req(_Response, _State) ->
    ok.
