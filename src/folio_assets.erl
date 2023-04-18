-module(folio_assets).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    write_assets/2,
    asset_for_symbol/2,
    get_assets/2,
    get_annotated_assets/2,
    set_provider_asset/4,
    try_to_set_unset_provider_assets/1
]).

-spec write_assets(epgsql:connection(), list(cryptowatch:asset())) -> ok.
write_assets(C, Assets) ->
    lists:foreach(
        fun(#{id := ID, symbol := Symbol, name := Name}) ->
            Data = #{
                external_id => ID, symbol => Symbol, name => Name, source => <<"cryptowatch">>
            },
            {ok, _} = fdb:write(C, assets, Data)
        end,
        Assets
    ),
    fdb:checkin(C),
    ok.

asset_for_symbol(C, SymBin) when is_binary(SymBin) ->
    Sym = string:lowercase(SymBin),
    {ok, Resp} = fdb:select(C, assets, #{symbol => Sym}),
    case Resp of
        [] ->
            ?LOG_INFO(#{
                message => "No assets found for symbol",
                symbol => Sym
            }),
            undefined;
        [A] ->
            A;
        Assets ->
            ?LOG_INFO(#{
                message => "Multiple assets found for symbol",
                symbol => Sym,
                assets => Assets
            }),
            undefined
    end.

% Entry point
try_to_set_unset_provider_assets(C) ->
    ?with_span(
        <<"try_to_set_unset_provider_assets">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            {ok, Balances} = fdb:select(C, integration_account_balances, #{}),
            lists:foreach(fun(B) -> try_to_set_unset_provider_assets(C, B) end, Balances)
        end
    ).

% For each balance, try to see if anything needs to be done
try_to_set_unset_provider_assets(_C, #{provider_asset_id := null}) ->
    ok;
try_to_set_unset_provider_assets(C, #{integration_id := IID, symbol := S, provider_asset_id := PAID}) ->
    {ok, [#{provider_name := PN}]} = fdb:select(C, integrations, #{id => IID}),

    Filter = #{
        provider_name => PN,
        provider_asset_id => PAID,
        asset_source => coingecko
    },

    case fdb:select(C, provider_asset_id_mapping, Filter) of
        % Mapping exists
        {ok, [_]} -> ok;
        {ok, []} -> set_provider_asset_for_symbol(C, PN, PAID, S)
    end.

set_provider_asset_for_symbol(C, PName, PAID, S) ->
    case asset_for_symbol(C, S) of
        undefined -> ok;
        Asset -> set_provider_asset(C, PName, PAID, Asset)
    end.

set_provider_asset(C, PName, PAID, #{source := AS, external_id := AEID}) ->
    D = #{
        provider_name => PName,
        provider_asset_id => PAID,
        asset_source => AS,
        asset_external_id => AEID
    },
    fdb:write(C, provider_asset_id_mapping, D).

get_assets(C, Filters) ->
    fdb:select(C, assets, Filters).

get_annotated_assets(C, Filters) ->
    fdb:select(C, v_assets, Filters, [{order_by, last_price, desc}]).
