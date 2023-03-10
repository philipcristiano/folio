-module(folio_blockstream).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-behavior(folio_integration).

-export([folio_init/0]).

-export([balance/1]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 10, per_second),
    ok.

setup_properties() ->
    #{
        address => #{}
    }.

add(IntegrationID, #{address := Addr}) ->
    Credentials = #{address => Addr},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    #{
        address => Addr,
        integration_id => IntegrationID
    }.

accounts(State = #{address := Addr}) ->
    {ok, #{
        <<"chain_stats">> := #{
            <<"funded_txo_sum">> := In,
            <<"spent_txo_sum">> := Out
        }
    }} = balance(Addr),

    % Blockstream returns in sats, divide to get whole BTC.
    SatBalance = In - Out,
    BTCBalance = SatBalance / 100000000,

    Accounts = [
        #{
            id => Addr,
            balances => [
                #{
                    balance => BTCBalance,
                    symbol => <<"BTC">>
                }
            ]
        }
    ],
    {complete, Accounts, State}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    #{address := Addr} = folio_credentials_store:get_credentials(IntegrationID),
    State = #{
        account_id => AccountID,
        address => Addr,
        integration_id => IntegrationID
    },
    State.

account_transactions(State = #{address := _Addr}) ->
    {complete, [], State}.

balance(Address) when is_binary(Address) ->
    Path = <<<<"/api/address/">>/binary, Address/binary>>,
    {ok, D} = request(Path),
    {ok, D}.

-spec request(binary()) -> {ok, map()} | {error, binary()}.
request(PathQS) ->
    request(PathQS, #{attempts_remaining => 3}).

-spec request(binary(), map()) -> {ok, map()} | {error, binary()}.
request(PathQS, #{attempts_remaining := AR}) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Blockstream request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining"};
request(PathQS, Opts = #{attempts_remaining := AR}) ->
    BasePath = <<"https://blockstream.info">>,
    Url = <<BasePath/binary, PathQS/binary>>,
    Headers = [],
    rate_limit(),
    case hackney:request(get, Url, Headers, [], [with_body]) of
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
