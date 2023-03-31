-module(folio_gemini_api).

-include_lib("kernel/include/logger.hrl").

% Notes:
% * Doesn't handle Gemini Staking
% * Fees not handled
% * transfers from exchange <-> Earn do not generate the matching TX on the other side

-behavior(folio_integration).

-define(TRANSFERS_THROTTLE_KEY, folio_gemini_api_transfers).

-export([folio_init/0]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 2, per_second),
    ok = throttle:setup(?TRANSFERS_THROTTLE_KEY, 1, 5000),
    ok.

setup_properties() ->
    [
        #{
            fields =>
                #{
                    key => #{},
                    secret => #{}
                }
        }
    ].

add(IntegrationID, #{key := K, secret := S}) ->
    Credentials = #{key => K, secret => S},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{
        integration_id => IntegrationID,
        get_account_list => true
    }.

accounts(State = #{get_account_list := true}) ->
    {ok, _Headers, AccountResp, State1} = request(<<"/v1/account/list">>, State),
    io:format("Resp ~p~n", [AccountResp]),
    ToSyncLists = lists:map(
        fun(#{<<"account">> := Acct}) -> [{exchange, Acct}, {earn, Acct}] end, AccountResp
    ),
    ToSync = lists:flatten(ToSyncLists),
    {incomplete, [], State1#{get_account_list => false, to_sync => ToSync}};
accounts(State = #{to_sync := []}) ->
    {complete, [], State};
accounts(State = #{to_sync := [{exchange, GeminiAcct} | T]}) ->
    RequestArgs = #{account => GeminiAcct},
    {ok, _Headers, BalanceResp, State1} = request(<<"/v1/balances">>, RequestArgs, State),
    io:format("Resp ~p~n", [BalanceResp]),

    FBalances = lists:map(fun to_folio_balance/1, BalanceResp),
    Acct = #{
        id => <<<<"exchange.">>/binary, GeminiAcct/binary>>,
        account => GeminiAcct,
        balances => FBalances
    },
    {incomplete, [Acct], State1#{to_sync => T}};
accounts(State = #{to_sync := [{earn, GeminiAcct} | T]}) ->
    RequestArgs = #{account => GeminiAcct},
    {ok, _Headers, BalanceResp, State1} = request(<<"/v1/balances/earn">>, RequestArgs, State),

    FBalances = lists:map(fun to_folio_balance/1, BalanceResp),
    Acct = #{
        id => <<<<"earn.">>/binary, GeminiAcct/binary>>,
        balances => FBalances
    },
    {incomplete, [Acct], State1#{to_sync => T}};
accounts(State = #{sync_exchange := false, sync_earn := false}) ->
    {complete, [], State}.

account_transactions_init(#{id := IntegrationID}, #{id := FolioID}) ->
    [AccountTypeBin, ID] = binary:split(FolioID, <<".">>),
    AccountType = erlang:binary_to_atom(AccountTypeBin),
    State =
        case AccountType of
            earn ->
                #{
                    earn_request_args => #{
                        account => ID,
                        until => os:system_time(millisecond),
                        sortAsc => false,
                        limit => 500
                    }
                };
            exchange ->
                #{
                    sync_trades => true,
                    sync_transfers => true,
                    trades_request_args => #{
                        account => ID,
                        timestamp => 0,
                        limit_trades => 500
                    },
                    transfers_request_args => #{
                        account => ID,
                        timestamp => 0,
                        limit_transfers => 50
                    }
                }
        end,
    State1 = State#{
        gemini_account_id => ID,
        account_type => AccountType,
        integration_id => IntegrationID
    },
    State1.

account_transactions(
    State = #{account_type := exchange, sync_trades := false, sync_transfers := false}
) ->
    {complete, [], State};
account_transactions(
    State = #{account_type := exchange, trades_request_args := RequestArgs, sync_trades := true}
) ->
    Path = <<"/v1/mytrades">>,
    {ok, _, Transactions, State1} = request(
        Path, RequestArgs, State
    ),
    ?LOG_INFO(#{
        message => "Gemini API Transactions",
        transactions => Transactions,
        request_args => RequestArgs
    }),

    TXLists = lists:map(fun(I) -> trades_to_folio_txs(I, State1) end, Transactions),
    TXs = lists:flatten(TXLists),

    {SyncTrades, NextTimestamp} =
        case TXs of
            [] ->
                {false, undefined};
            _ ->
                TXTSs = lists:map(fun(#{<<"timestamp">> := TS}) -> TS end, Transactions),
                MaxTS = lists:max(TXTSs),
                {true, MaxTS + 1}
        end,

    NextRequestArgs = RequestArgs#{timestamp => NextTimestamp},
    {incomplete, TXs, State1#{trades_request_args => NextRequestArgs, sync_trades => SyncTrades}};
account_transactions(
    State = #{
        account_type := exchange, transfers_request_args := RequestArgs, sync_transfers := true
    }
) ->
    Path = <<"/v1/transfers">>,
    {ok, _, Transactions, State1} = request(
        Path, RequestArgs, State
    ),
    ?LOG_INFO(#{
        message => "Gemini API Transfers",
        transactions => Transactions,
        request_args => RequestArgs
    }),

    TXLists = lists:map(fun(I) -> transfers_to_folio_txs(I, State1) end, Transactions),
    TXs = lists:flatten(TXLists),

    {SyncTransfers, NextTimestamp} =
        case TXs of
            [] ->
                {false, undefined};
            _ ->
                TXTSs = lists:map(fun(#{<<"timestampms">> := TS}) -> TS end, Transactions),
                MaxTS = lists:max(TXTSs),
                {true, MaxTS + 1}
        end,

    NextRequestArgs = RequestArgs#{timestamp => NextTimestamp},
    {incomplete, TXs, State1#{
        transfers_request_args => NextRequestArgs, sync_transfers => SyncTransfers
    }};
account_transactions(State = #{account_type := earn, earn_request_args := RequestArgs}) ->
    Path = <<"/v1/earn/history">>,
    {ok, _, Resp, State1} = request(Path, RequestArgs, State),

    RespTransactions =
        case Resp of
            [] -> [];
            [#{<<"transactions">> := Transactions}] -> Transactions
        end,

    TXLists = lists:map(fun(I) -> earn_to_folio_txs(I, State1) end, RespTransactions),
    TXs = lists:flatten(TXLists),

    {Complete, NextUntil} =
        case RespTransactions of
            [] ->
                {complete, undefined};
            _ ->
                TXDTs = lists:map(fun(#{<<"dateTime">> := DT}) -> DT end, RespTransactions),
                MinDTMinus1 = lists:min(TXDTs) - 1,
                {incomplete, MinDTMinus1}
        end,
    NextRequestArgs = RequestArgs#{until => NextUntil},
    {Complete, TXs, State1#{earn_request_args => NextRequestArgs}}.

to_folio_balance(#{
    <<"type">> := <<"exchange">>,
    <<"currency">> := Currency,
    <<"amount">> := BalanceS
}) ->
    Balance = folio_math:to_decimal(BalanceS),
    #{balance => Balance, symbol => Currency};
to_folio_balance(#{
    <<"type">> := <<"Earn">>,
    <<"currency">> := Currency,
    <<"balance">> := BalanceS
}) ->
    Balance = folio_math:to_decimal(BalanceS),
    #{balance => Balance, symbol => Currency}.

transfers_to_folio_txs(
    #{
        <<"amount">> := AmountBin,
        <<"currency">> := Currency,
        <<"eid">> := IDInt,
        <<"status">> := Status,
        <<"type">> := Type,
        <<"timestampms">> := TSMS
    },
    #{account_type := exchange}
) ->
    Direction =
        case Type of
            <<"Deposit">> -> in;
            <<"Withdrawal">> -> out;
            <<"Reward">> -> in
        end,
    Amount = folio_math:to_decimal(AmountBin),

    ID = erlang:integer_to_binary(IDInt),
    SourceID = <<<<"transfers.">>/binary, ID/binary>>,
    Description = <<Type/binary, <<" ">>/binary, Status/binary>>,

    TX = #{
        source_id => SourceID,
        datetime => qdate:to_date(trunc(TSMS / 1000)),
        symbol => Currency,
        type => undefined,
        direction => Direction,
        description => Description,
        amount => Amount
    },
    [TX].
trades_to_folio_txs(
    #{
        <<"amount">> := AmountBin,
        <<"symbol">> := TradePair,
        <<"tid">> := ID,
        <<"type">> := Type,
        <<"price">> := PriceBin,
        <<"timestamp">> := TS
    },
    #{account_type := exchange}
) ->
    {Left, Right} = tradepair_to_currency(TradePair),
    IDB = erlang:integer_to_binary(ID),
    LeftID = <<IDB/binary, <<".">>/binary, Left/binary>>,
    RightID = <<IDB/binary, <<".">>/binary, Right/binary>>,
    Amount = folio_math:to_decimal(AmountBin),
    Price = folio_math:to_decimal(PriceBin),

    RightAmount = decimal:mult(Amount, Price),

    {LeftDir, RightDir} =
        case Type of
            <<"Buy">> -> {in, out};
            <<"Sell">> -> {out, in}
        end,

    LeftTX = #{
        source_id => LeftID,
        datetime => qdate:to_date(TS),
        symbol => Left,
        type => undefined,
        direction => LeftDir,
        description => TradePair,
        amount => Amount
    },
    RightTX = #{
        source_id => RightID,
        datetime => qdate:to_date(TS),
        symbol => Right,
        direction => RightDir,
        type => undefined,
        description => TradePair,
        amount => RightAmount
    },

    [LeftTX, RightTX].

tradepair_to_currency(<<Left:3/binary, Right:3/binary>>) ->
    {Left, Right}.

earn_to_folio_txs(
    #{
        <<"amount">> := AmountFloat,
        <<"amountCurrency">> := Currency,
        <<"earnTransactionId">> := ID,
        <<"transactionType">> := Type,
        <<"dateTime">> := DT
    },
    #{account_type := earn}
) ->
    Default = #{
        source_id => ID,
        datetime => qdate:to_date(trunc(DT / 1000)),
        symbol => Currency,
        amount => folio_math:to_decimal(AmountFloat)
    },
    TypeProps =
        case Type of
            T = <<"Deposit">> ->
                #{
                    direction => in,
                    type => undefined,
                    description => T
                };
            T = <<"Redeem">> ->
                #{
                    direction => out,
                    type => undefined,
                    description => T
                };
            T = <<"AdminRedeem">> ->
                #{
                    direction => out,
                    type => undefined,
                    description => T
                };
            T = <<"AdminDebitAdjustment">> ->
                #{
                    direction => out,
                    type => undefined,
                    description => T
                };
            T = <<"AdminCreditAdjustment">> ->
                #{
                    direction => in,
                    type => undefined,
                    description => T
                };
            T = <<"Interest">> ->
                #{
                    direction => in,
                    type => undefined,
                    description => T
                }
        end,
    maps:merge(Default, TypeProps).

credentials(_State = #{integration_id := ID}) ->
    C =
        #{key := _Key, secret := _Secret} = folio_credentials_store:get_credentials(
            ID
        ),
    C.

-spec request(binary(), any()) -> {ok, list(), map() | list(), any()} | {error, binary(), any()}.
request(PathQS, State) ->
    request(PathQS, #{}, #{attempts_remaining => 3}, State).

-spec request(binary(), map(), any()) ->
    {ok, list(), map() | list(), any()} | {error, binary(), any()}.
request(PathQS, Args, State) ->
    request(PathQS, Args, #{attempts_remaining => 3}, State).

-spec request(binary(), map(), map(), any()) ->
    {ok, list(), map() | list(), any()} | {error, binary(), any()}.
request(PathQS, _Args, #{attempts_remaining := AR}, State) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Coinbase request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining", State};
request(PathQS, Args, Opts = #{attempts_remaining := AR}, State) ->
    BasePath = <<"https://api.gemini.com">>,
    Url = <<BasePath/binary, PathQS/binary>>,

    rate_limit(),
    Headers = sign(PathQS, Args, State),

    case hackney:request(post, Url, Headers, <<>>, [with_body]) of
        {error, timeout} ->
            request(PathQS, Args, Opts#{attempts_remaining => AR - 1}, State);
        {ok, _RespCode, RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, RespHeaders, jsx:decode(Body, [return_maps]), State};
                false -> {error, Body, State}
            end
    end.

sign(PathQS, ArgMap, State) ->
    #{key := Key, secret := Secret} = credentials(State),
    Now = os:system_time(second),
    PayloadDefaults = #{
        request => PathQS,
        nonce => Now
    },
    Payload = maps:merge(PayloadDefaults, ArgMap),

    PayloadJSON = jsx:encode(Payload),
    PayloadJSONB64 = base64:encode(PayloadJSON),
    Sig = binary:encode_hex(crypto:mac(hmac, sha384, Secret, PayloadJSONB64)),

    Headers = [
        {<<"X-GEMINI-APIKEY">>, Key},
        {<<"X-GEMINI-PAYLOAD">>, PayloadJSONB64},
        {<<"X-GEMINI-SIGNATURE">>, Sig},
        {<<"User-Agent">>, <<"folio">>},
        {<<"Cache-Control">>, <<"no-cache">>},
        {<<"Content-Type">>, <<"text/plain">>}
    ],
    Headers.

rate_limit() ->
    rate_limit(?MODULE, key).
rate_limit(Scope, Key) ->
    case throttle:check(Scope, Key) of
        {ok, _RemainingAttempts, _TimeToReset} ->
            ok;
        {limit_exceeded, _, TimeToReset} ->
            ChosenTime = time_to_reset(TimeToReset),
            ?LOG_DEBUG(#{
                message => "Rate limit would be exceeded",
                time_to_reset => TimeToReset,
                time_to_sleep => ChosenTime,
                scope => Scope,
                key => Key,
                pid => self()
            }),
            timer:sleep(ChosenTime),
            rate_limit(Scope, Key)
    end.

time_to_reset(I) when I < 2 ->
    rand:uniform(30);
time_to_reset(N) ->
    N.
