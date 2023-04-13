-module(folio_throttle).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("kernel/include/logger.hrl").

-export([setup/3, rate_limit/2, sleep/1]).

setup(Domain, Rate, Time) ->
    throttle:setup(Domain, Rate, Time).

rate_limit(Domain, Key) ->
    case throttle:check(Domain, key) of
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
            sleep(ChosenTime),
            rate_limit(Domain, Key)
    end.

sleep(Time) ->
    ?with_span(
        <<"sleep">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            timer:sleep(Time)
        end
    ).

time_to_reset(I) when I < 2 ->
    rand:uniform(30);
time_to_reset(N) ->
    N.
