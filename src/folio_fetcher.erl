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

-export([sync/0, sync/1]).
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
    {ok, C} = fdb:connect(),
    {ok, Integrations} = folio_integration:integrations(C),

    lists:foreach(fun sync/1, Integrations),
    ok.

-spec sync(folio_integration:integration()) -> ok.
sync(Int = #{id := _ID, provider_name := _PN}) ->
    gen_server:cast(?MODULE, {sync, Int}),
    ok.

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
handle_cast({sync, Integration = #{id := _ID, provider_name := _PN}}, State) ->
    ?LOG_INFO(#{
        message => "Starting sync spawn",
        integration => Integration
    }),
    ctx_spawn(<<"fetch_integration_accounts">>, fun() ->
        ?LOG_INFO(#{
            message => "Starting sync",
            integration => Integration
        }),
        {ok, Accounts} = folio_integration:fetch_integration_accounts(Integration),
        ok = write_accounts(Integration, Accounts),

        lists:foreach(
            fun(Acc) ->
                ctx_spawn(<<"fetch_integration">>, fun() ->
                    Transactions = folio_integration:fetch_integration_account_transactions(
                        Integration, Acc
                    ),
                    ?LOG_INFO(#{
                        message => account_transactions,
                        account => Acc,
                        transactions => Transactions
                    })
                end)
            end,
            Accounts
        )
    end),

    {noreply, State}.

ctx_spawn(Name, Fun) ->
    Parent = ?current_span_ctx,

    proc_lib:spawn(fun() ->
        %% a new process has a new context so the span created
        %% by the following `with_span` will have no parent
        Link = opentelemetry:link(Parent),
        ?with_span(
            Name,
            #{links => [Link]},
            fun(_Ctx) ->
                Fun()
            end
        )
    end).

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
write_accounts(#{id := IntegrationID}, Accounts) ->
    {ok, C} = fdb:connect(),
    lists:foreach(
        fun(#{id := ID, balance := B, symbol := S}) ->
            AData = #{external_id => ID, integration_id => IntegrationID},
            BalData = #{
                integration_id => IntegrationID,
                external_id => ID,
                symbol => S,
                balance => B
            },
            ?LOG_INFO(#{
                message => write_cb_account,
                account_data => AData,
                balance_data => BalData
            }),
            {ok, _} = fdb:write(C, integration_accounts, AData),
            {ok, _} = fdb:write(C, integration_account_balances, BalData)
        end,
        Accounts
    ),
    fdb:close(C),
    ok.
