-module(folio_handler_integration_delete).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/api/integrations/:integration_id">>).

trails() ->
    Metadata = folio_http:make_delete(deleteIntegration, [integration_id], #{}),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"DELETE">>},
    _Params = #{integration_id := IntegrationID},
    _Body,
    State
) ->
    {ok, C} = fdb:connect(),
    ok = folio_integration:delete_integration(C, IntegrationID),
    fdb:close(C),

    {Req, 200, #{}, State}.

post_req(_Response, _State) ->
    ok.
