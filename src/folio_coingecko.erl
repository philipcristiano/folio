-module(folio_coingecko).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([folio_init/0]).
-export([get_assets/0, price_for_asset/1, price_for_assets/1]).

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
    datetime := calendar:datetime(),
    asset_id := asset_id()
}.

folio_init() ->
    ok = throttle:setup(?MODULE, 1, 15000),
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

-spec price_for_asset(asset()) -> {ok, fiat_value()}.
price_for_asset(A) when is_map(A) ->
    {ok, [Single]} = price_for_assets([A]),
    {ok, Single}.

price_for_assets(Assets) ->
    AssetIDs = lists:map(fun(#{id := ID}) -> ID end, Assets),
    IDsArgs = binary_join:join(<<",">>, AssetIDs),
    Path =
        <<<<"/api/v3/simple/price?vs_currencies=usd&precision=full&include_last_updated_at=true&ids=">>/binary,
            IDsArgs/binary>>,
    {ok, Data} = request(Path),
    DataMaps = price_data_to_maps(Data),
    {ok, DataMaps}.

price_data_to_maps(Data) ->
    AssetMap = maps:map(
        fun(Name, #{<<"usd">> := Amount, <<"last_updated_at">> := Timestamp}) ->
            DT = qdate:to_date(Timestamp),
            #{
                currency => <<"usd">>,
                amount => to_decimal(Amount),
                datetime => DT,
                asset_id => Name
            }
        end,
        Data
    ),
    maps:values(AssetMap).

-spec request(binary()) -> {ok, map() | list()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 5}).

-spec request(binary(), map()) -> {ok, map() | list()} | {error, binary()}.
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
        {error, closed} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1});
        {ok, 429, _RespHeaders, _Body} ->
            rate_limit(),
            rate_limit(),
            ?LOG_INFO(#{
                message => rate_limited_by_provider,
                provider => coingecko,
                url => URL
            }),
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

to_decimal(I) when is_number(I) ->
    decimal:to_decimal(I, #{precision => 100, rounding => round_floor});
to_decimal(F) when is_binary(F) ->
    L = size(F),
    decimal:to_decimal(F, #{precision => L, rounding => round_floor}).
