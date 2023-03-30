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
    fetch_price_for_symbol/1,
    fetch_and_store_price/1,
    asset_for_symbol/1,
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
    sync_assets(),
    sync_asset_prices(),
    {ok, #state{}}.

sync_assets() ->
    gen_server:cast(?MODULE, sync_assets).

sync_asset_prices() ->
    gen_server:cast(?MODULE, sync_asset_prices).

asset_for_symbol(SymBin) when is_binary(SymBin) ->
    Sym = string:lowercase(SymBin),
    {ok, C} = fdb:connect(),
    {ok, Resp} = fdb:select(C, assets, #{symbol => Sym}),
    fdb:close(C),
    case Resp of
        [] -> undefined;
        [A] -> A;
        _else -> undefined
    end.

price_for_asset_id(AID) ->
    {ok, C} = fdb:connect(),
    Resp = price_for_asset_id(C, AID),
    fdb:close(C),
    Resp.
price_for_asset_id(C, AID) ->
    Resp = fdb:select(
        C, asset_prices, #{source => "coingecko", external_id => AID, fiat_symbol => <<"usd">>}, [
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

fetch_price_for_symbol(Symbol) ->
    {ok, C} = fdb:connect(),
    R = fetch_price_for_symbol(C, Symbol),
    fdb:close(C),
    R.
fetch_price_for_symbol(C, Symbol) ->
    {ok, [DBA]} = fdb:select(C, assets, #{symbol => Symbol}),
    Asset = db_asset_to_asset(DBA),
    {ok, Val} = folio_coingecko:price_for_asset(Asset),
    {ok, Val}.

fetch_and_store_price(ID) ->
    {ok, C} = fdb:connect(),
    {ok, [DBA]} = fdb:select(C, assets, #{external_id => ID}),
    Asset = db_asset_to_asset(DBA),
    {ok, Val} = folio_coingecko:price_for_asset(Asset),
    write_asset_price(C, Asset, Val),
    fdb:close(C).

db_asset_to_asset(#{external_id := ID, symbol := S, name := Name}) ->
    #{id => ID, symbol => S, name => Name}.

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
    {ok, Assets} = folio_coingecko:get_assets(),
    {ok, State1} = write_assets(Assets, State),

    {noreply, State1};
handle_cast(sync_asset_prices, State) ->
    {ok, C} = fdb:connect(),
    {ok, AllBalances} = fdb:select(C, integration_account_balances, #{}),
    Balances = lists:filter(
        fun(#{balance := BalanceBin}) ->
            Balance = to_decimal(BalanceBin),
            case decimal:is_zero(Balance) of
                true -> false;
                false -> true
            end
        end,
        AllBalances
    ),
    SymbolList = lists:map(fun(#{symbol := S}) -> string:lowercase(S) end, Balances),
    SymbolsToFetch = sets:to_list(sets:from_list(SymbolList)),

    AssetLists = lists:map(
        fun(S) ->
            {ok, Assets} = fdb:select(C, assets, #{symbol => S}),
            ?LOG_INFO(#{
                message => look_up_asset,
                symbol => S,
                assets => Assets
            }),
            Assets
        end,
        SymbolsToFetch
    ),

    DBAAssets = lists:flatten(AssetLists),
    Assets = lists:map(fun db_asset_to_asset/1, DBAAssets),

    ?LOG_INFO(#{
        message => assets_to_fetch_and_save,
        assets => Assets,
        asset_lists => AssetLists,
        symbol_list => SymbolList,
        symbols_to_fetch => SymbolsToFetch,
        dba_assets => DBAAssets
    }),

    fetch_and_write(C, Assets),

    fdb:close(C),
    {noreply, State}.

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
-spec write_assets(list(coingecko:asset()), any()) -> {ok, any()}.
write_assets(Assets, State) ->
    {ok, C} = fdb:connect(),
    lists:foreach(
        fun(#{id := ID, symbol := Symbol, name := Name}) ->
            Data = #{external_id => ID, symbol => Symbol, name => Name, source => <<"coingecko">>},
            {ok, _} = fdb:write(C, assets, Data)
        end,
        Assets
    ),
    fdb:close(C),
    {ok, State}.

fetch_and_write(_C, []) ->
    ok;
fetch_and_write(C, Assets) ->
    {AssetBatch, AssetsRest} = split_list(10, Assets),
    {ok, Vals} = folio_coingecko:price_for_assets(AssetBatch),
    Pairs = pair_up_assets_and_values(AssetBatch, Vals),

    lists:foreach(fun({A, V}) -> write_asset_price(C, A, V) end, Pairs),
    fetch_and_write(C, AssetsRest).

pair_up_assets_and_values(_Assets, []) ->
    [];
pair_up_assets_and_values(Assets, [#{asset_id := AssetID} = V | T]) ->
    {value, A} = lists:search(fun(#{id := ID}) -> ID == AssetID end, Assets),
    [{A, V} | pair_up_assets_and_values(Assets, T)].

-spec write_asset_price(epgsql:connection(), folio_coingeck:asset(), folio_coingecko:fiat_value()) ->
    ok.
write_asset_price(C, #{id := ID, symbol := S}, #{currency := Cu, amount := A, datetime := DT}) ->
    AssetPrice = #{
        source => <<"coingecko">>,
        symbol => S,
        external_id => ID,
        amount => decimal:to_binary(A),
        timestamp => DT,
        fiat_symbol => Cu
    },
    fdb:write(C, asset_prices, AssetPrice),
    ok.

to_decimal(I) when is_number(I) ->
    decimal:to_decimal(I, #{precision => 100, rounding => round_floor});
to_decimal(F) when is_binary(F) ->
    L = size(F),
    decimal:to_decimal(F, #{precision => L, rounding => round_floor}).

split_list(N, L) ->
    case length(L) >= N of
        true -> lists:split(N, L);
        false -> {L, []}
    end.

start_timer() ->
    timer:apply_interval(timer:minutes(51), ?MODULE, sync_asset_prices, []).
