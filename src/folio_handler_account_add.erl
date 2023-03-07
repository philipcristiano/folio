-module(folio_handler_account_add).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(LIST_PATH, <<"/accounts/add">>).

trails() ->
    Metadata = folio_http:make_get(listAddAbleAccounts, #{}),
    [
        trails:trail(?LIST_PATH, ?MODULE, #{}, Metadata)
        | trails_for_integrations()
    ].

trails_for_integrations() ->
    Integrations = folio_exchange_integration:integrations(),
    lists:map(
        fun(Integration = #{name := Name}) ->
            Spec = props_to_schema(folio_exchange_integration:integration_setup_properties(Name)),
            trails_for_integration_spec(Name, Integration, Spec)
        end,
        Integrations
    ).

trails_for_integration_spec(Name, State, Spec) ->
    GetMetadata = folio_http:make_get(addChainAccount, #{}),
    PostMetadata = folio_http:make_json_post(addChainAccount, [], Spec),
    Metadata = maps:merge(PostMetadata, GetMetadata),
    Path = <<?LIST_PATH/binary, <<"/">>/binary, Name/binary>>,
    trails:trail(Path, ?MODULE, State, Metadata).

props_to_schema(Props) ->
    Names = maps:keys(Props),
    SchemaProps = maps:map(fun(K, _V) -> folio_http:make_string_property(K) end, Props),
    #{
        required => Names,
        properties => SchemaProps
    }.

props_to_form_input(Props) ->
    maps:keys(Props).

init(Req, State) ->
    folio_http_session:init(Req),
    {specified_handler, Req, State}.

handle_req(
    Req = #{method := <<"GET">>, path := ?LIST_PATH},
    _Params,
    _Body,
    State
) ->
    Integrations = folio_exchange_integration:integrations(),
    Names = lists:map(fun(#{name := N}) -> N end, Integrations),
    {Req, 200, #{integrations => Names}, State};
handle_req(
    Req = #{method := <<"GET">>},
    _Params,
    _Body,
    State = #{mod := Mod, name := Name}
) ->
    ?LOG_INFO(#{message => getAccountAdd, mode => Mod, name => Name}),

    Props = folio_exchange_integration:integration_setup_properties(Name),

    {Req, 200, #{setup_properties => props_to_form_input(Props)}, State};
handle_req(
    Req = #{method := <<"GET">>, path := Path},
    _Params,
    _Body,
    State
) ->
    ?LOG_INFO(#{message => getAccountAdd, state => State, path => Path}),

    {Req, 200, #{setup_properties => []}, State};
handle_req(
    Req = #{method := <<"POST">>},
    _Params,
    Body,
    State = #{mod := _Mod, name := Name}
) ->
    ok = folio_exchange_integration:add_integration(Name, Body),
    {Req, 200, #{}, State};
handle_req(
    Req = #{method := <<"POST">>},
    _Params,
    _Body = #{
        address := Address
    },
    State
) ->
    ok = folio_chain_accounts:add(#{
        address => Address,
        chain => <<"bitcoin">>,
        type => <<"address">>
    }),

    {Req, 202, #{status => ok}, State}.

post_req(_Response, _State) ->
    ok.