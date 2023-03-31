-module(folio_cryptowatch).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([folio_init/0]).
-export([get_assets/0, get_asset_prices/0]).

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
    ok = throttle:setup(?MODULE, 1, 5000),
    ok.

-spec get_assets() -> {ok, list(asset())}.
get_assets() ->
    {ok, #{<<"result">> := Assets}} = request(<<"/assets">>),
    AssetMaps = lists:map(
        fun(#{<<"sid">> := ID, <<"symbol">> := Symbol, <<"name">> := Name}) ->
            #{id => ID, symbol => Symbol, name => Name}
        end,
        Assets
    ),
    {ok, AssetMaps}.

get_asset_prices() ->
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
    {ok, Prices}.

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

-spec request(binary()) -> {ok, map() | list()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 5}).

-spec request(binary(), map()) -> {ok, map() | list()} | {error, binary()}.
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
    case hackney:request(get, URL, Headers, [], [with_body]) of
        {error, closed} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, 429, _RespHeaders, _Body} ->
            rate_limit(),
            rate_limit(),
            ?LOG_INFO(#{
                message => rate_limited_by_provider,
                provider => cryptowatch,
                url => URL
            }),
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true ->
                    {ok, jsx:decode(Body, [return_maps])};
                false ->
                    {error, Body}
            end
    end.

rate_limit() ->
    case throttle:check(?MODULE, key) of
        {ok, _RemainingAttempts, _TimeToReset} ->
            ok;
        {limit_exceeded, _, TimeToReset} ->
            ChosenTime = time_to_reset(TimeToReset),
            ?LOG_DEBUG(#{
                message => "Rate limit would be exceeded",
                time_to_reset => TimeToReset,
                time_to_sleep => ChosenTime,
                pid => self()
            }),
            timer:sleep(ChosenTime),
            rate_limit()
    end.

time_to_reset(I) when I < 2 ->
    rand:uniform(30);
time_to_reset(N) ->
    N.
