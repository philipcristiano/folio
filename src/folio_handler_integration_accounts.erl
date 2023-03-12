-module(folio_handler_integration_accounts).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/integrations/:integration_id/accounts">>).

trails() ->
    Metadata = folio_http:make_get(getAccounts, [integration_id], return_schema()),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

return_schema() ->
    #{
        type => object,
        description => <<"Accounts">>,
        properties => #{
            <<"accounts">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of accounts">>
            }
        }
    }.

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    _Params = #{integration_id := IntegrationID},
    _Body,
    State
) ->
    ?LOG_DEBUG(#{message => getAccounts}),

    {ok, C} = fdb:connect(),
    {ok, Accounts} = folio_integration:integration_accounts(C, IntegrationID),
    fdb:close(C),

    {Req, 200, #{accounts => Accounts}, State}.

post_req(_Response, _State) ->
    ok.
