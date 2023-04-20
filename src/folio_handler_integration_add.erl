-module(folio_handler_integration_add).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(LIST_PATH, <<"/api/integration/add">>).

trails() ->
    Metadata = folio_http:make_get(listAddAbleAccounts, #{}),
    [
        trails:trail(?LIST_PATH, ?MODULE, #{}, Metadata)
        | trails_for_integrations()
    ].

trails_for_integrations() ->
    Providers = folio_provider:providers_by_types([chain, exchange]),
    lists:map(
        fun(Provider = #{name := Name}) ->
            Spec = props_to_schema(folio_provider:provider_setup_properties(Name)),
            trails_for_integration_spec(Name, Provider, Spec)
        end,
        Providers
    ).

trails_for_integration_spec(Name, State, Spec) ->
    GetMetadata = folio_http:make_get(addChainAccount, #{}),
    PostMetadata = folio_http:make_json_post(addChainAccount, [], Spec),
    Metadata = maps:merge(PostMetadata, GetMetadata),
    Path = <<?LIST_PATH/binary, <<"/">>/binary, Name/binary>>,
    trails:trail(Path, ?MODULE, State, Metadata).

props_to_schema(PropMaps) when is_list(PropMaps) ->
    PropSchemas = lists:map(fun props_to_schema/1, PropMaps),
    #{oneOf => PropSchemas};
props_to_schema(_ProviderSetup = #{fields := Props}) when is_map(Props) ->
    Names = maps:keys(Props),
    SchemaProps = maps:map(fun(K, _V) -> folio_http:make_string_property(K) end, Props),
    #{
        required => Names,
        properties => SchemaProps
    }.

props_to_form_input(SetupPropertiesList) when is_list(SetupPropertiesList) ->
    lists:map(fun props_to_form_input/1, SetupPropertiesList);
props_to_form_input(#{fields := Fields}) when is_map(Fields) ->
    FormMap = maps:map(fun prop_to_form_input/2, Fields),
    maps:values(FormMap).

prop_to_form_input(Name, _Config = #{choices := Choices}) ->
    #{
        name => Name,
        type => choice,
        choices => maps:keys(Choices)
    };
prop_to_form_input(Name, _Config) ->
    #{
        name => Name,
        type => text
    }.

init(Req, State) ->
    folio_http_session:init(Req),
    {specified_handler, Req, State}.

handle_req(
    Req = #{method := <<"GET">>, path := ?LIST_PATH},
    _Params,
    _Body,
    State
) ->
    Integrations = folio_provider:providers_by_types([chain, exchange]),
    Names = lists:map(fun(#{name := N}) -> N end, Integrations),
    {Req, 200, #{integrations => Names}, State};
handle_req(
    Req = #{method := <<"GET">>},
    _Params,
    _Body,
    State = #{mod := Mod, name := Name}
) ->
    ?LOG_INFO(#{message => getAccountAdd, mode => Mod, name => Name}),

    Props = folio_provider:provider_setup_properties(Name),

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
    {ok, Int} = folio_integration:add_integration(Name, Body),
    ok = folio_fetcher:sync(Int),
    {Req, 200, #{}, State}.

post_req(_Response, _State) ->
    ok.
