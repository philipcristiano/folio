-module(folio_handler_balance).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/api/balance">>).

trails() ->
    Metadata = folio_http:make_get(getBalance, [], return_schema()),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

return_schema() ->
    #{
        type => object,
        description => <<"Balance">>,
        properties => #{
            <<"fiat_value">> => #{
                type => string,
                description => <<"USD-valued balance">>
            }
        }
    }.

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    _Params = #{},
    _Body,
    State
) ->
    ?LOG_DEBUG(#{message => getBalance}),

    {ok, C} = fdb:connect(),
    {ok, Accounts} = folio_integration:integration_accounts(C),
    AccountsWithFiat = folio_accounts:add_fiat_value_for_accounts(C, Accounts),
    FiatTotal = folio_accounts:fiat_value_of_accounts(AccountsWithFiat),

    fdb:close(C),

    {Req, 200, #{fiat_value => FiatTotal}, State}.

post_req(_Response, _State) ->
    ok.
