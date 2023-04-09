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

-type pid_info() ::
    {integration, folio_integration:integration()}
    | {transactions, folio_integration:integration(), folio_integration:account()}.

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
    erlang:process_flag(trap_exit, true),
    {ok, _TRef} = start_timer(),
    {ok, #{pid_info_map => #{}}}.

sync() ->
    C = fdb:checkout(),
    {ok, Integrations} = folio_integration:integrations(C),

    lists:foreach(fun sync/1, Integrations),
    ok.

-spec sync(folio_integration:integration()) -> ok.
sync(Int = #{id := _ID, provider_name := _PN}) ->
    gen_server:cast(?MODULE, {sync, Int}),
    ok.

-spec register_pid(pid(), pid_info()) -> ok.
register_pid(Pid, Info) ->
    gen_server:cast(?MODULE, {register_pid, Pid, Info}).

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
handle_cast({register_pid, Pid, Info}, State = #{pid_info_map := PidInfoMap}) ->
    erlang:link(Pid),
    NewPidInfoMap = PidInfoMap#{Pid => Info},
    {noreply, State#{pid_info_map => NewPidInfoMap}};
handle_cast({sync, Integration = #{id := _ID, provider_name := _PN}}, State) ->
    ?LOG_INFO(#{
        message => "Starting sync spawn",
        integration => Integration
    }),
    ok = folio_integration:set_integration_state(Integration, starting),
    ctx_spawn(<<"fetch_integration_accounts">>, {integration, Integration}, fun() ->
        ?LOG_INFO(#{
            message => "Starting sync",
            integration => Integration
        }),
        {ok, Accounts} = folio_integration:fetch_integration_accounts(Integration),
        ok = write_accounts(Integration, Accounts),

        lists:foreach(
            fun(Acc) ->
                ctx_spawn(<<"fetch_integration">>, {transactions, Integration, Acc}, fun() ->
                    WriteFun = fun(TXs) ->
                        ok = write_account_transactions(Integration, Acc, TXs),
                        ?LOG_DEBUG(#{
                            message => account_transactions,
                            account => Acc,
                            transactions => TXs
                        })
                    end,

                    ok = folio_integration:fetch_integration_account_transactions(
                        WriteFun, Integration, Acc
                    )
                end)
            end,
            Accounts
        )
    end),

    {noreply, State}.

ctx_spawn(Name, Info, Fun) ->
    Parent = ?current_span_ctx,
    Integration = integration_from_info(Info),
    Attrs = otel_attributes_for_integration(Integration),

    Pid = proc_lib:spawn(fun() ->
        %% a new process has a new context so the span created
        %% by the following `with_span` will have no parent
        Link = opentelemetry:link(Parent),
        ?with_span(
            Name,
            #{links => [Link], attributes => Attrs},
            fun(_Ctx) ->
                Fun()
            end
        )
    end),
    register_pid(Pid, Info),
    Pid.

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
handle_info({'EXIT', From, Reason}, State = #{pid_info_map := PidInfoMap}) ->
    {Info, NewPidInfoMap} = maps:take(From, PidInfoMap),
    Integration = integration_from_info(Info),
    StillRunning = pid_info_has_integration(Integration, NewPidInfoMap),
    ?LOG_DEBUG(#{
        info => Info,
        message => "exiting process info",
        reason => Reason,
        from => From,
        still_running => StillRunning,
        pid_info_map => PidInfoMap
    }),
    SyncState =
        case {StillRunning, Reason} of
            {false, normal} -> complete;
            {true, normal} -> running;
            {_, _} -> error
        end,
    folio_integration:set_integration_state(Integration, SyncState),
    {noreply, State#{pid_info_map => NewPidInfoMap}};
handle_info(Info, State) ->
    ?LOG_ERROR(#{
        message => "unhandled info",
        info => Info
    }),
    {noreply, State}.

pid_info_has_integration(Int, PidInfoMap) ->
    Infos = maps:values(PidInfoMap),
    lists:any(
        fun(Info) ->
            integration_from_info(Info) == Int
        end,
        Infos
    ).

integration_from_info({integration, Int}) -> Int;
integration_from_info({transactions, Int, _Account}) -> Int.
otel_attributes_for_integration(#{id := ID, provider_name := P}) ->
    #{integration_id => ID, provider_name => P}.
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
    C = fdb:checkout(),
    ?with_span(
        <<"write_accounts">>,
        #{attributes => #{integration_id => IntegrationID}},
        fun(_Ctx) ->
            lists:foreach(
                fun(#{id := ID, balances := Balances}) ->
                    AData = #{external_id => ID, integration_id => IntegrationID},
                    {ok, _} = fdb:write(C, integration_accounts, AData),
                    lists:foreach(
                        fun(#{balance := Balance, symbol := Symbol}) ->
                            BalData = #{
                                integration_id => IntegrationID,
                                external_id => ID,
                                symbol => Symbol,
                                balance => decimal:to_binary(Balance)
                            },
                            ?LOG_DEBUG(#{
                                message => write_cb_account,
                                account_data => AData,
                                balance_data => BalData
                            }),
                            {ok, _} = fdb:write(C, integration_account_balances, BalData)
                        end,
                        Balances
                    )
                end,
                Accounts
            )
        end
    ),
    fdb:checkin(C),
    ok.

% TODO: Move fdb:write to folio_integration
-spec write_account_transactions(
    folio_integration:integration(),
    folio_integration:account(),
    folio_integration:account_transactions()
) -> ok.
write_account_transactions(#{id := IntegrationID}, _Account = #{id := AccountID}, Transactions) ->
    C = fdb:checkout(),
    ?with_span(
        <<"write_account_transactions">>,
        #{attributes => #{integration_id => IntegrationID, account_id => AccountID}},
        fun(_Ctx) ->
            lists:foreach(
                fun(
                    _T = #{
                        source_id := SourceID,
                        line := Line,
                        datetime := DT,
                        direction := Direction,
                        symbol := Symbol,
                        amount := Amount,
                        type := Type,
                        description := Description
                    }
                ) ->
                    DBT = #{
                        integration_id => IntegrationID,
                        external_id => AccountID,
                        source_id => SourceID,
                        line => Line,
                        timestamp => DT,
                        direction => Direction,
                        symbol => Symbol,
                        amount => decimal:to_binary(Amount),
                        type => Type,
                        description => Description
                    },
                    {ok, _} = fdb:write(C, integration_account_transactions, DBT)
                end,
                Transactions
            )
        end
    ),
    fdb:checkin(C),
    ok.

start_timer() ->
    timer:apply_interval(timer:minutes(181), ?MODULE, sync, []).
