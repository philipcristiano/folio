-module(folio_handler_accounts).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

trails() ->
    GetMetadata = folio_http:make_get(getAccounts, [], return_schema()),
    PostMetadata = folio_http:make_json_post(syncAccounts, [], #{}),
    Metadata = maps:merge(GetMetadata, PostMetadata),
    State = #{},
    [
        trails:trail("/accounts", ?MODULE, State, Metadata)
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
    folio_http_session:init(Req),
    {specified_handler, Req, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    _Params,
    _Body,
    State
) ->
    ?LOG_INFO(#{message => getAccounts}),

    %{ok, Accounts} = folio_exchange_integration:integration_accounts(folio_coinbase_api),
    %ok = write_coinbase_accounts(Accounts),
    {ok, C} = fdb:connect(),
    {ok, ExchangeAccounts} = folio_accounts:account_balances(C),
    {ok, ChainAccounts} = folio_chain_accounts:account_balances(C),
    Accounts = ExchangeAccounts ++ ChainAccounts,

    {Req, 200, #{accounts => Accounts}, State};
handle_req(
    Req = #{method := <<"POST">>},
    _Params,
    _Body,
    State
) ->
    ?LOG_INFO(#{message => syncAccounts}),

    {Req, 202, #{status => ok}, State}.

post_req(_Response, _State) ->
    ok.
