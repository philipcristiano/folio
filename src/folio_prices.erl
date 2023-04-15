%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @copyright 2023 philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(folio_prices).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    sync_assets/0,
    sync_asset_prices/0,
    price_for_asset_id/1, price_for_asset_id/2
]).
-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, _TRef} = start_timer(),

    sync_assets_if_none(),
    sync_asset_prices_if_not_recent(),
    {ok, #state{}}.

sync_assets() ->
    gen_server:cast(?MODULE, sync_assets).

sync_asset_prices() ->
    gen_server:cast(?MODULE, sync_asset_prices).

sync_assets_if_none() ->
    C = fdb:checkout(),
    {ok, Assets} = fdb:select(C, assets, #{}, [{limit, 1}]),
    fdb:checkin(C),
    case Assets of
        [] -> sync_assets();
        _ -> ?LOG_INFO(#{message => "Assets exist, not syncing on start"})
    end.

sync_asset_prices_if_not_recent() ->
    C = fdb:checkout(),
    Now = os:system_time(second),
    Recent = qdate:to_date(qdate:add_minutes(-15, Now)),
    {ok, Assets} = fdb:select(C, asset_prices, #{timestamp => {'>', Recent}}, [{limit, 1}]),
    fdb:checkin(C),
    case Assets of
        [] -> sync_asset_prices();
        _ -> ?LOG_INFO(#{message => "Recent asset prices found, not syncing on start"})
    end.

price_for_asset_id(<<"united-states-dollar">>) ->
    {ok, #{amount => <<"1.0">>}};
price_for_asset_id(AID) ->
    C = fdb:checkout(),
    Resp = price_for_asset_id(C, AID),
    fdb:checkin(C),
    Resp.
price_for_asset_id(_C, <<"united-states-dollar">>) ->
    {ok, #{amount => <<"1.0">>}};
price_for_asset_id(C, AID) ->
    ?LOG_DEBUG(#{
        message => <<"price for asset id">>,
        assetid => AID
    }),
    Resp = fdb:select(
        C, asset_prices, #{source => "cryptowatch", external_id => AID, fiat_symbol => <<"usd">>}, [
            {order_by, timestamp, desc}, {limit, 1}
        ]
    ),
    case Resp of
        {ok, [P]} ->
            {ok, P};
        {ok, []} ->
            ?LOG_INFO(#{
                message => "No price found",
                asset_id => AID
            }),
            undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(sync_assets, State) ->
    ?LOG_INFO(#{
        message => "Starting asset sync"
    }),
    ?with_span(
        <<"sync_assets">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            {ok, Assets} = folio_cryptowatch:get_assets(),
            C = fdb:checkout(),
            ok = folio_assets:write_assets(C, Assets),
            fdb:checkin(C),
            {noreply, State}
        end
    );
handle_cast(sync_asset_prices, State) ->
    ?with_span(
        <<"sync_asset_prices">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            C = fdb:checkout(),

            Now = os:system_time(second),
            DT = qdate:to_date(Now),
            {ok, Prices} = folio_cryptowatch:get_asset_prices(),

            ?LOG_DEBUG(#{
                message => asset_prices,
                prices => Prices,
                datetime => DT
            }),

            write_asset_prices(C, DT, Prices),

            fdb:checkin(C),
            {noreply, State}
        end
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_timer() ->
    timer:apply_interval(timer:minutes(51), ?MODULE, sync_asset_prices, []),
    timer:apply_interval(timer:minutes(600), ?MODULE, sync_assets, []).

write_asset_prices(C, DT, Prices) ->
    ?with_span(
        <<"write_asset_prices">>,
        #{attributes => #{}},
        fun(_Ctx) ->
            lists:foreach(
                fun(P = #{symbol := Symbol}) ->
                    Asset = folio_assets:asset_for_symbol(C, Symbol),
                    write_asset_price(C, DT, Asset, P)
                end,
                Prices
            )
        end
    ).

write_asset_price(_C, DT, undefined, P) ->
    ?LOG_ERROR(#{
        message => "unknown asset for symbol",
        dt => DT,
        p => P
    }),
    ok;
write_asset_price(C, DT, #{external_id := ID}, #{currency := Cu, amount := A, symbol := Symbol}) ->
    AssetPrice = #{
        source => <<"cryptowatch">>,
        symbol => Symbol,
        external_id => ID,
        amount => decimal:to_binary(A),
        timestamp => DT,
        fiat_symbol => Cu
    },
    fdb:write(C, asset_prices, AssetPrice),
    ok.
