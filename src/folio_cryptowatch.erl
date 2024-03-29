-module(folio_cryptowatch).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([folio_init/0]).
-export([name/0]).
-export([get_assets/1, get_asset_prices/1]).
-export([get_assets_init/0, get_asset_prices_init/0]).

-export_type([asset_id/0]).
-type asset_id() :: binary().

-export_type([asset/0]).
-type asset() :: #{
    id := asset_id(),
    symbol := binary(),
    name := binary()
}.

-export_type([fiat_value/0]).
-type fiat_value() :: #{
    currency := binary(),
    amount := decimal:decimal(),
    symbol := binary()
}.

folio_init() ->
    folio_throttle:setup(?MODULE, 1, 5000).

name() -> <<"cryptowatch">>.

get_assets_init() ->
    {ok, #{}}.

-spec get_assets(any()) -> {complete, list(asset()), any()}.
get_assets(State) ->
    {ok, #{<<"result">> := Assets}} = request(<<"/assets">>),
    AssetMaps = lists:map(
        fun(#{<<"id">> := IDInt, <<"symbol">> := Symbol, <<"name">> := Name}) ->
            ID = erlang:integer_to_binary(IDInt),
            #{id => ID, symbol => Symbol, name => Name}
        end,
        Assets
    ),
    {complete, AssetMaps, State}.

get_asset_prices_init() ->
    {ok, #{}}.
get_asset_prices(State) ->
    {ok, #{<<"result">> := RawPriceMap}} = request(<<"/markets/prices">>),
    PriceData = parse_market_prices(RawPriceMap),

    % Filter Coinbase USD prices
    FilteredPrices = lists:filter(
        fun({Type, Market, Pair, _Price}) ->
            lists:all(fun(Bool) -> Bool end, [
                Type == <<"market">>,
                Market == <<"coinbase">>,
                binary:longest_common_suffix([<<"usd">>, Pair]) == 3
            ])
        end,
        PriceData
    ),

    Prices = lists:map(
        fun({_, _, Pair, Price}) ->
            Symbol = binary:part(Pair, 0, size(Pair) - 3),
            #{
                currency => <<"usd">>,
                amount => Price,
                symbol => Symbol
            }
        end,
        FilteredPrices
    ),
    {complete, Prices, State}.

parse_market_prices(RawPriceMap) ->
    NewMap = maps:map(
        fun(K, Price) ->
            [Type, Source, Pair] = binary:split(K, <<":">>, [global]),
            {Type, Source, Pair, folio_math:to_decimal(Price)}
        end,
        RawPriceMap
    ),

    Parsed = maps:values(NewMap),
    Parsed.

request(PathQS) ->
    request(PathQS, #{attempts_remaining => 5}).

request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Cryptowatch request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://api.cryptowat.ch">>,
    URL = <<BasePath/binary, PathQS/binary>>,
    ?LOG_INFO(#{
        message => cryptowatch_request,
        url => URL
    }),
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"user-agent">>, <<"folio">>}
    ],
    rate_limit(),

    EF = fun() -> request(PathQS, Opts#{attempts_remaining => AR - 1}) end,
    case folio_http:request(get, URL, Headers, [], EF) of
        {ok, 429, _RespHeaders, _Body} ->
            rate_limit(),
            rate_limit(),
            ?LOG_INFO(#{
                message => rate_limited_by_provider,
                provider => cryptowatch,
                url => URL
            }),
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Data} ->
            log_api_info(PathQS, Data),
            {ok, Data}
    end.

rate_limit() ->
    folio_throttle:rate_limit(?MODULE, key).

log_api_info(Path, #{<<"allowance">> := A}) ->
    Msg = #{
        message => "cryptowatch_api_allowance",
        path => Path
    },
    Line = maps:merge(Msg, map_keys_to_atoms(A)),
    ?LOG_INFO(Line);
log_api_info(_Path, _) ->
    ok.

map_keys_to_atoms(M) ->
    maps:fold(
        fun(K, V, AccIn) ->
            Ka = erlang:binary_to_atom(K),
            AccIn#{Ka => V}
        end,
        #{},
        M
    ).
