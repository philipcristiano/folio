-module(folio_coingecko).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([folio_init/0]).
-export([get_assets/0, price_for_asset/1]).

-export_type([asset/0]).
-type asset() :: #{
    id := binary(),
    symbol := binary(),
    name := binary()
}.

folio_init() ->
    ok = throttle:setup(?MODULE, 1, 10000),
    ok.

-spec get_assets() -> {ok, list(asset())}.
get_assets() ->
    {ok, Assets} = request(<<"/api/v3/coins/list">>),
    AssetMaps = lists:map(
        fun(#{<<"id">> := ID, <<"symbol">> := Symbol, <<"name">> := Name}) ->
            #{id => ID, symbol => Symbol, name => Name}
        end,
        Assets
    ),
    {ok, AssetMaps}.

-spec price_for_asset(asset()) -> any().
price_for_asset(_Asset) ->
    request(<<"/api/v3/ping">>).

-spec request(binary()) -> {ok, map()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

-spec request(binary(), map()) -> {ok, map()} | {error, binary()}.
request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "CoinGecko request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://api.coingecko.com">>,
    URL = <<BasePath/binary, PathQS/binary>>,
    ?LOG_INFO(#{
        message => coingecko_request,
        url => URL
    }),
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"user-agent">>, <<"folio">>}
    ],
    rate_limit(),
    case hackney:request(get, URL, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, _RespCode, _RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, jsx:decode(Body, [return_maps])};
                false -> {error, Body}
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
