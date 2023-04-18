-module(folio_handler_holdings).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/api/holdings">>).

trails() ->
    Params = params(),
    Metadata = folio_http:make_get(getHoldings, Params, return_schema()),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

return_schema() ->
    #{
        type => object,
        description => <<"holdings">>,
        properties => #{
            <<"holdings">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of holdings">>
            }
        }
    }.

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

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    Params,
    _Body,
    State
) ->
    RequestedFilters = maps:filter(fun(_K, V) -> V /= undefined end, Params),
    HasPriceFilters = RequestedFilters#{asset_balance => {'>', "0"}},
    C = fdb:checkout(),
    {ok, AccountBalances} = folio_accounts:get_annotated_account_balances(C, HasPriceFilters),
    fdb:checkin(C),
    FiatTotal = folio_accounts:fiat_value_of_accounts(AccountBalances),

    {Req, 200, #{fiat_total => FiatTotal, holdings => AccountBalances}, State}.

post_req(_Response, _State) ->
    ok.
