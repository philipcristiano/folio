-module(folio_assets).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([write_assets/2, asset_for_symbol/1, asset_for_symbol/2, get_assets/2]).

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

asset_for_symbol(SymBin) when is_binary(SymBin) ->
    Sym = string:lowercase(SymBin),
    C = fdb:checkout(),
    {ok, Resp} = fdb:select(C, assets, #{symbol => Sym}),
    fdb:checkin(C),
    case Resp of
        [] -> undefined;
        [A] -> A;
        _else -> undefined
    end.
asset_for_symbol(C, SymBin) when is_binary(SymBin) ->
    Sym = string:lowercase(SymBin),
    {ok, Resp} = fdb:select(C, assets, #{symbol => Sym}),
    case Resp of
        [] -> undefined;
        [A] -> A;
        _else -> undefined
    end.

get_assets(C, Filters) ->
    fdb:select(C, v_assets, Filters, [{order_by, last_price, desc}]).
