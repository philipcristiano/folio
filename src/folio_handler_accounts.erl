-module(folio_handler_accounts).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

trails() ->
    GetMetadata = folio_http:make_get(teamsGet, [org_id], return_schema()),
    % PostMetadata = folio_http:make_json_post(teamCreate, [org_id], create_schema()),
    % Metadata = maps:merge(GetMetadata, PostMetadata),
    State = #{},
    [
        trails:trail("/accounts", ?MODULE, State, GetMetadata)
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
    {ok, Accounts} = fdb:select(C, coinbase_accounts, #{}),

    {Req, 200, #{accounts => Accounts}, State}.

post_req(_Response, _State) ->
    ok.

write_coinbase_accounts(Accounts) ->
    {ok, C} = fdb:connect(),
    lists:foreach(
        fun(#{id := ID, balance := B, symbol := S}) ->
            Data = #{id => ID, balance_amount => B, balance_currency => S},
            ?LOG_INFO(#{message => write_cb_account, data => Data}),
            {ok, _} = fdb:write(C, coinbase_accounts, Data)
        end,
        Accounts
    ),
    ok.
