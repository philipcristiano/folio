%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @copyright 2023 philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(folio_fetcher).

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

-export([sync/0]).
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
    {ok, #state{}}.

sync() ->
    gen_server:cast(?MODULE, sync).

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
handle_cast(sync, State) ->
    {ok, Accounts} = folio_exchange_integration:integration_accounts(folio_coinbase_api),
    ok = write_coinbase_accounts(Accounts),
    Parent = ?current_span_ctx,

    lists:foreach(
        fun(Acc) ->
            proc_lib:spawn_link(fun() ->
                %% a new process has a new context so the span created
                %% by the following `with_span` will have no parent
                Link = opentelemetry:link(Parent),
                ?with_span(
                    <<"coinbase_fetch">>,
                    #{links => [Link]},
                    fun(_Ctx) ->
                        Transactions = folio_exchange_integration:integration_account_transactions(
                            folio_coinbase_api, Acc
                        ),
                        ?LOG_INFO(#{
                            message => account_transactions,
                            account => Acc,
                            transactions => Transactions
                        })
                    end
                )
            end)
        end,
        Accounts
    ),

    {noreply, State};
handle_cast(_Msg, State) ->
    {ok, Accounts} = folio_exchange_integration:integration_accounts(folio_coinbase_api),
    ok = write_coinbase_accounts(Accounts),
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
write_coinbase_accounts(Accounts) ->
    {ok, C} = fdb:connect(),
    lists:foreach(
        fun(#{id := ID, balance := B, symbol := S}) ->
            AData = #{id => ID, symbol => S},
            BalData = #{account_id => ID, balance => B},
            ?LOG_INFO(#{
                message => write_cb_account,
                account_data => AData,
                balance_data => BalData
            }),
            {ok, _} = fdb:write(C, coinbase_accounts, AData),
            {ok, _} = fdb:write(C, coinbase_account_balances, BalData)
        end,
        Accounts
    ),
    ok.
