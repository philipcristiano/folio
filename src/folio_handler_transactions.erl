-module(folio_handler_transactions).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

trails() ->
    Params = params(),
    Metadata = folio_http:make_get(getTransactions, Params, return_schema()),
    State = #{},
    [
        trails:trail(<<"/transactions">>, ?MODULE, State, Metadata)
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
        description => <<"Transactions">>,
        properties => #{
            <<"transactions">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of transactions">>
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
    ?LOG_INFO(#{message => params,
                params => Params}),
    C = fdb:checkout(),
    {ok, Transactions} = folio_integration:transactions(C, Params),
    fdb:checkin(C),
    TOut = lists:map(fun fmt_account_transaction/1, Transactions),

    {Req, 200, #{transactions => TOut}, State}.

post_req(_Response, _State) ->
    ok.

fmt_account_transaction(#{
    integration_id := IntegrationID,
    external_id := AccountID,
    source_id := SourceID,
    timestamp := DT,
    direction := Direction,
    symbol := Symbol,
    amount := Amount,
    provider_name := PN,
    type := Type,
    description := Description
}) ->
    #{
        integration_id => IntegrationID,
        external_id => AccountID,
        source_id => SourceID,
        timestamp => DT,
        direction => Direction,
        symbol => Symbol,
        amount => Amount,
        provider_name => PN,
        type => Type,
        description => Description
    }.
