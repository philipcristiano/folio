-module(folio_gemini_api).

-include_lib("kernel/include/logger.hrl").

% Notes:
% * Doesn't handle Gemini Staking
% * Fees not handled
% * transfers from exchange <-> Earn do not generate the matching TX on the other side

-behavior(folio_account_provider).

-define(TRANSFERS_THROTTLE_KEY, folio_gemini_api_transfers).

-export([folio_init/0]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    ok = throttle:setup(?MODULE, 1, per_second),
    ok = throttle:setup(?TRANSFERS_THROTTLE_KEY, 1, 7500),
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
    {ok, _, _, AccountResp} = request(<<"/v1/account/list">>, State),
    ToSyncLists = lists:map(
        fun(#{<<"account">> := Acct}) -> [{exchange, Acct}, {earn, Acct}] end, AccountResp
    ),
    ToSync = lists:flatten(ToSyncLists),
    {incomplete, [], State#{get_account_list => false, to_sync => ToSync}};
accounts(State = #{to_sync := []}) ->
    {complete, [], State};
accounts(State = #{to_sync := [{exchange, GeminiAcct} | T]}) ->
    RequestArgs = #{account => GeminiAcct},
    {ok, _, _, BalanceResp} = request(<<"/v1/balances">>, RequestArgs, State),
    FBalances = lists:map(fun to_folio_balance/1, BalanceResp),
    Acct = #{
        id => <<<<"exchange.">>/binary, GeminiAcct/binary>>,
        account => GeminiAcct,
        balances => FBalances
    },
    {incomplete, [Acct], State#{to_sync => T}};
accounts(State = #{to_sync := [{earn, GeminiAcct} | T]}) ->
    RequestArgs = #{account => GeminiAcct},
    {ok, _, _, BalanceResp} = request(<<"/v1/balances/earn">>, RequestArgs, State),

    FBalances = lists:map(fun to_folio_balance/1, BalanceResp),
    Acct = #{
        id => <<<<"earn.">>/binary, GeminiAcct/binary>>,
        balances => FBalances
    },
    {incomplete, [Acct], State#{to_sync => T}};
accounts(State = #{sync_exchange := false, sync_earn := false}) ->
    {complete, [], State}.

account_transactions_init(#{id := IntegrationID}, #{id := FolioID}) ->
    [AccountTypeBin, ID] = binary:split(FolioID, <<".">>),
    AccountType = erlang:binary_to_atom(AccountTypeBin),
    EarnSync = #{
        type => earn,
        request_args => #{
            account => ID,
            until => os:system_time(millisecond),
            sortAsc => false,
            limit => 500
        }
    },
    TransfersSync = #{
        type => transfers,
        request_args => #{
            account => ID,
            timestamp => 0,
            limit_transfers => 50
        }
    },
    TradesSync = #{
        type => trades,
        request_args => #{
            account => ID,
            timestamp => 0,
            limit_trades => 500
        }
    },
    State =
        case AccountType of
            earn ->
                #{
                    to_sync => [EarnSync#{tx_filter => fun predicate_any/1}]
                };
            exchange ->
                #{
                    to_sync => [
                        TradesSync,
                        TransfersSync,
                        EarnSync#{tx_filter => fun predicate_earn_tx_for_eachange/1}
                    ]
                }
        end,
    State1 = State#{
        gemini_account_id => ID,
        account_type => AccountType,
        integration_id => IntegrationID
    },
    State1.

account_transactions(
    State = #{to_sync := []}
) ->
    {complete, [], State};
account_transactions(
    State = #{to_sync := [#{type := trades, request_args := RequestArgs} = H | RestToSync]}
) ->
    Path = <<"/v1/mytrades">>,
    {ok, _, _, Transactions} = request(
        Path, RequestArgs, State
    ),
    ?LOG_DEBUG(#{
        message => "Gemini API Transactions",
        transactions => Transactions,
        request_args => RequestArgs
    }),

    TXLists = lists:map(fun(I) -> trades_to_folio_txs(I, State) end, Transactions),
    TXs = lists:flatten(TXLists),

    ToSync =
        case TXs of
            [] ->
                RestToSync;
            _ ->
                TXTSs = lists:map(fun(#{<<"timestamp">> := TS}) -> TS end, Transactions),
                MaxTS = lists:max(TXTSs),
                NextRequestArgs = RequestArgs#{timestamp => MaxTS + 1},
                [H#{request_args => NextRequestArgs} | RestToSync]
        end,

    {incomplete, TXs, State#{to_sync => ToSync}};
account_transactions(
    State = #{to_sync := [#{type := transfers, request_args := RequestArgs} = H | RestToSync]}
) ->
    Path = <<"/v1/transfers">>,
    folio_throttle:rate_limit(?TRANSFERS_THROTTLE_KEY, key),
    {ok, _, _, Transactions} = request(
        Path, RequestArgs, State
    ),
    ?LOG_DEBUG(#{
        message => "Gemini API Transfers",
        transactions => Transactions,
        request_args => RequestArgs
    }),

    TXLists = lists:map(fun(I) -> transfers_to_folio_txs(I, State) end, Transactions),
    TXs = lists:flatten(TXLists),

    ToSync =
        case TXs of
            [] ->
                RestToSync;
            _ ->
                TXTSs = lists:map(fun(#{<<"timestampms">> := TS}) -> TS end, Transactions),
                MaxTS = lists:max(TXTSs),
                NextRequestArgs = RequestArgs#{timestamp => MaxTS + 1},
                [H#{request_args => NextRequestArgs} | RestToSync]
        end,

    {incomplete, TXs, State#{to_sync => ToSync}};
account_transactions(
    State = #{
        to_sync := [
            #{type := earn, request_args := RequestArgs, tx_filter := TXFilterFun} = H | RestToSync
        ]
    }
) ->
    Path = <<"/v1/earn/history">>,
    {ok, _, _, Resp} = request(Path, RequestArgs, State),

    RespTransactions =
        case Resp of
            [] -> [];
            [#{<<"transactions">> := Transactions}] -> Transactions
        end,

    FilteredTransactions = lists:filter(TXFilterFun, RespTransactions),
    TXLists = lists:map(fun(I) -> earn_to_folio_txs(I, State) end, FilteredTransactions),
    TXs = lists:flatten(TXLists),

    ToSync =
        case RespTransactions of
            [] ->
                RestToSync;
            _ ->
                TXDTs = lists:map(fun(#{<<"dateTime">> := DT}) -> DT end, RespTransactions),
                MinDTMinus1 = lists:min(TXDTs) - 1,
                NextRequestArgs = RequestArgs#{until => MinDTMinus1},
                [H#{request_args => NextRequestArgs} | RestToSync]
        end,
    {incomplete, TXs, State#{to_sync => ToSync}}.

to_folio_balance(#{
    <<"type">> := <<"exchange">>,
    <<"currency">> := Currency,
    <<"amount">> := BalanceS
}) ->
    Balance = folio_math:to_decimal(BalanceS),
    #{balance => Balance, asset => #{symbol => Currency}};
to_folio_balance(#{
    <<"type">> := <<"Earn">>,
    <<"currency">> := Currency,
    <<"balance">> := BalanceS
}) ->
    Balance = folio_math:to_decimal(BalanceS),
    #{balance => Balance, asset => #{symbol => Currency}}.

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
    Description = <<Type/binary, <<" ">>/binary, Status/binary>>,

    TX = #{
        source_id => ID,
        line => <<"">>,
        datetime => qdate:to_date(trunc(TSMS / 1000)),
        asset => #{symbol => Currency},
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
    Amount = folio_math:to_decimal(AmountBin),
    Price = folio_math:to_decimal(PriceBin),

    RightAmount = decimal:mult(Amount, Price),

    {LeftDir, RightDir} =
        case Type of
            <<"Buy">> -> {in, out};
            <<"Sell">> -> {out, in}
        end,

    LeftTX = #{
        source_id => IDB,
        line => Left,
        datetime => qdate:to_date(TS),
        asset => #{symbol => Left},
        type => undefined,
        direction => LeftDir,
        description => TradePair,
        amount => Amount
    },
    RightTX = #{
        source_id => IDB,
        line => Right,
        datetime => qdate:to_date(TS),
        asset => #{symbol => Right},
        direction => RightDir,
        type => undefined,
        description => TradePair,
        amount => RightAmount
    },

    [LeftTX, RightTX].

tradepair_to_currency(<<Left:3/binary, Right:3/binary>>) ->
    {Left, Right}.

earn_tx_direction(in, earn) -> in;
earn_tx_direction(out, earn) -> out;
earn_tx_direction(in, exchange) -> out;
earn_tx_direction(out, exchange) -> in.

earn_to_folio_txs(
    #{
        <<"amount">> := AmountFloat,
        <<"amountCurrency">> := Currency,
        <<"earnTransactionId">> := ID,
        <<"transactionType">> := Type,
        <<"dateTime">> := DT
    },
    #{account_type := AccountType}
) ->
    Default = #{
        source_id => ID,
        line => <<"">>,
        datetime => qdate:to_date(trunc(DT / 1000)),
        asset => #{symbol => Currency},
        amount => folio_math:to_decimal(AmountFloat)
    },

    TypeProps =
        case Type of
            <<"Deposit">> ->
                #{
                    direction => earn_tx_direction(in, AccountType),
                    type => undefined,
                    description => <<"Earn Deposit">>
                };
            <<"Redeem">> ->
                #{
                    direction => earn_tx_direction(out, AccountType),
                    type => undefined,
                    description => <<"Earn Redeem">>
                };
            T = <<"AdminRedeem">> ->
                #{
                    direction => earn_tx_direction(out, AccountType),
                    type => undefined,
                    description => T
                };
            T = <<"AdminDebitAdjustment">> ->
                #{
                    direction => earn_tx_direction(out, AccountType),
                    type => undefined,
                    description => T
                };
            T = <<"AdminCreditAdjustment">> ->
                #{
                    direction => earn_tx_direction(in, AccountType),
                    type => undefined,
                    description => T
                };
            T = <<"Interest">> ->
                #{
                    direction => earn_tx_direction(in, AccountType),
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

request(PathQS, State) ->
    request(PathQS, #{}, #{attempts_remaining => 3}, State).

request(PathQS, Args, State) ->
    request(PathQS, Args, #{attempts_remaining => 3}, State).

request(PathQS, _Args, #{attempts_remaining := AR}, State) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Gemini request failed",
        path => PathQS
    }),
    {error, "No more attempts remaining", State};
request(PathQS, Args, Opts = #{attempts_remaining := AR}, State) ->
    BasePath = <<"https://api.gemini.com">>,
    Url = <<BasePath/binary, PathQS/binary>>,

    ?LOG_DEBUG(#{
        message => "Gemini http request",
        url => Url
    }),
    rate_limit(),
    Headers = sign(PathQS, Args, State),

    EF = fun() -> request(PathQS, Args, Opts#{attempts_remaining => AR - 1}, State) end,
    folio_http:request(post, Url, Headers, [], EF).

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
    folio_throttle:rate_limit(?MODULE, key).

predicate_any(_) -> true.

predicate_earn_tx_for_eachange(#{
    <<"transactionType">> := Type
}) ->
    lists:member(Type, [<<"Deposit">>, <<"Redeem">>]).
