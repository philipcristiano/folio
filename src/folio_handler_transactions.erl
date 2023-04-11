-module(folio_handler_transactions).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

trails() ->
    Metadata = folio_http:make_get(getTransactions, [], return_schema()),
    State = #{},
    [
        trails:trail(<<"/transactions">>, ?MODULE, State, Metadata)
    ].

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
    _Params,
    _Body,
    State
) ->
    C = fdb:checkout(),
    {ok, Transactions} = folio_integration:transactions(C),
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
