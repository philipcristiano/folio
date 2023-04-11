-module(folio_gemini_api_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_gemini_api).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

accounts_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},

    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], credentials()),

    State0 = ?MUT:accounts_init(Integration),

    URLAccounts = url_for_path(<<"/v1/account/list">>),
    AccountResp = json([
        #{
            <<"name">> => <<"Primary">>,
            <<"account">> => <<"primary">>,
            <<"type">> => <<"exchange">>,
            <<"counterparty_id">> => <<"EMONNYXH">>,
            <<"created">> => 1495127793000,
            <<"status">> => <<"open">>
        }
    ]),

    URLBalances = url_for_path(<<"/v1/balances">>),
    BalancesResp = json([
        #{
            <<"type">> => <<"exchange">>,
            <<"currency">> => <<"BTC">>,
            <<"amount">> => <<"1.23">>
        }
    ]),
    URLEarnBalances = url_for_path(<<"/v1/balances/earn">>),
    EarnBalancesResp = json([
        #{
            <<"type">> => <<"Earn">>,
            <<"currency">> => <<"BTC2">>,
            <<"balance">> => <<"1.234">>
        }
    ]),
    ok = meck:expect(
        hackney,
        request,
        [
            {[post, URLAccounts, '_', [], [with_body]], {ok, 200, [], AccountResp}},
            {[post, URLBalances, '_', [], [with_body]], {ok, 200, [], BalancesResp}},
            {[post, URLEarnBalances, '_', [], [with_body]], {ok, 200, [], EarnBalancesResp}}
        ]
    ),

    {incomplete, [], State1} = ?MUT:accounts(State0),
    {incomplete, [Acct1], State2} = ?MUT:accounts(State1),
    {incomplete, [Acct2], State3} = ?MUT:accounts(State2),
    {complete, [], _State4} = ?MUT:accounts(State3),

    ?assertMatch(
        #{
            balances := [#{balance := {123, -2}, symbol := <<"BTC">>}],
            id := <<"exchange.primary">>
        },
        Acct1
    ),
    ?assertMatch(
        #{
            balances := [#{balance := {1234, -3}, symbol := <<"BTC2">>}],
            id := <<"earn.primary">>
        },
        Acct2
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_exchange_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    AccountID = <<"exchange.primary">>,
    Account = #{id => AccountID},

    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], credentials()),

    io:format(<<"1">>),
    State0 = ?MUT:account_transactions_init(Integration, Account),
    TradesURL = url_for_path(<<"/v1/mytrades">>),
    TransferURL = url_for_path(<<"/v1/transfers">>),
    EarnURL = url_for_path(<<"/v1/earn/history">>),

    ok = meck:expect(
        hackney,
        request,
        fun(post, ReqURL, Headers, _, _) ->
            PayloadB64 = proplists:get_value(<<"X-GEMINI-PAYLOAD">>, Headers),
            PayloadJSON = base64:decode(PayloadB64),
            Payload = jsx:decode(PayloadJSON, [return_maps]),
            % io:format("Payload ~p~n", [Payload]),
            case {ReqURL, Payload} of
                {EarnURL, #{<<"sortAsc">> := false, <<"limit">> := 500, <<"until">> := 99000}} ->
                    {ok, 200, [], json([])};
                {EarnURL, #{<<"sortAsc">> := false, <<"limit">> := 500, <<"until">> := 100000}} ->
                    {ok, 200, [], json([#{transactions => [earn_interest(99001)]}])};
                {EarnURL, #{<<"sortAsc">> := false}} ->
                    {ok, 200, [], json([#{transactions => [earn_deposit(100001)]}])};
                {TradesURL, #{<<"timestamp">> := 21, <<"limit_trades">> := 500}} ->
                    {ok, 200, [], json([])};
                {TradesURL, #{<<"timestamp">> := 11, <<"limit_trades">> := 500}} ->
                    {ok, 200, [], json([trade_sell(20)])};
                {TradesURL, #{<<"timestamp">> := 0, <<"limit_trades">> := 500}} ->
                    {ok, 200, [], json([trade_buy(10)])};
                {TransferURL, #{<<"timestamp">> := 20002, <<"limit_transfers">> := 50}} ->
                    {ok, 200, [], json([])};
                {TransferURL, #{<<"timestamp">> := 10001, <<"limit_transfers">> := 50}} ->
                    {ok, 200, [], json([transfer_withdrawal(20000), transfer_reward(20001)])};
                {TransferURL, #{<<"timestamp">> := 0, <<"limit_transfers">> := 50}} ->
                    {ok, 200, [], json([transfer_deposit(10000)])}
            end
        end
    ),

    {incomplete, [TXBuyL, TXBuyR], State1} = ?MUT:account_transactions(State0),
    {incomplete, [TXSellL, TXSellR], State2} = ?MUT:account_transactions(State1),
    {incomplete, [], State3} = ?MUT:account_transactions(State2),
    {incomplete, [TXDeposit], State4} = ?MUT:account_transactions(State3),
    {incomplete, [TXWithdrawal, TXReward], State5} = ?MUT:account_transactions(State4),
    {incomplete, [], State6} = ?MUT:account_transactions(State5),
    {incomplete, [EarnDeposit], State7} = ?MUT:account_transactions(State6),
    {incomplete, [], State8} = ?MUT:account_transactions(State7),
    {incomplete, [], State9} = ?MUT:account_transactions(State8),
    {complete, [], _State10} = ?MUT:account_transactions(State9),

    ?assertMatch(
        #{
            source_id := <<"107317526">>,
            line := <<"BTC">>,
            datetime := {{1970, 1, 1}, {0, 0, 10}},
            direction := in,
            asset := #{symbol := <<"BTC">>},
            amount := {27343246, -10},
            type := undefined,
            description := <<"BTCUSD">>
        },
        TXBuyL
    ),
    ?assertMatch(
        #{
            source_id := <<"107317526">>,
            line := <<"USD">>,
            datetime := {{1970, 1, 1}, {0, 0, 10}},
            direction := out,
            asset := #{symbol := <<"USD">>},
            amount := {9975062230014, -12},
            type := undefined,
            description := <<"BTCUSD">>
        },
        TXBuyR
    ),
    ?assertMatch(
        #{
            source_id := <<"107317527">>,
            line := <<"BTC">>,
            datetime := {{1970, 1, 1}, {0, 0, 20}},
            direction := out,
            asset := #{symbol := <<"BTC">>},
            amount := {12343246, -10},
            type := undefined,
            description := <<"BTCUSD">>
        },
        TXSellL
    ),
    ?assertMatch(
        #{
            source_id := <<"107317527">>,
            line := <<"USD">>,
            datetime := {{1970, 1, 1}, {0, 0, 20}},
            direction := in,
            asset := #{symbol := <<"USD">>},
            amount := {4505272446754, -12},
            type := undefined,
            description := <<"BTCUSD">>
        },
        TXSellR
    ),
    ?assertMatch(
        #{
            source_id := <<"320033681">>,
            line := <<"">>,
            datetime := {{1970, 1, 1}, {0, 0, 10}},
            direction := in,
            asset := #{symbol := <<"USD">>},
            amount := {125, 0},
            type := undefined,
            description := <<"Deposit Advanced">>
        },
        TXDeposit
    ),
    ?assertMatch(
        #{
            source_id := <<"320033682">>,
            line := <<"">>,
            datetime := {{1970, 1, 1}, {0, 0, 20}},
            direction := out,
            asset := #{symbol := <<"USD">>},
            amount := {1, 2},
            type := undefined,
            description := <<"Withdrawal Complete">>
        },
        TXWithdrawal
    ),
    ?assertMatch(
        #{
            source_id := <<"320033683">>,
            line := <<"">>,
            datetime := {{1970, 1, 1}, {0, 0, 20}},
            direction := in,
            asset := #{symbol := <<"USD">>},
            amount := {25, 0},
            type := undefined,
            description := <<"Reward Advanced">>
        },
        TXReward
    ),

    ?assertMatch(
        #{
            source_id := <<"GHIJK34L5">>,
            line := <<"">>,
            datetime := {{1970, 1, 1}, {0, 1, 40}},
            direction := out,
            asset := #{symbol := <<"BTC">>},
            amount := {100000001, -8},
            type := undefined,
            description := <<"Earn Deposit">>
        },
        EarnDeposit
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_earn_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    AccountID = <<"earn.primary">>,
    Account = #{id => AccountID},

    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], credentials()),

    State0 = ?MUT:account_transactions_init(Integration, Account),
    URL = url_for_path(<<"/v1/earn/history">>),

    ok = meck:expect(
        hackney,
        request,
        fun(post, URL, Headers, _, _) ->
            PayloadB64 = proplists:get_value(<<"X-GEMINI-PAYLOAD">>, Headers),
            PayloadJSON = base64:decode(PayloadB64),
            Payload = jsx:decode(PayloadJSON, [return_maps]),
            io:format("Payload ~p~n", [Payload]),
            case Payload of
                #{<<"sortAsc">> := false, <<"limit">> := 500, <<"until">> := 99000} ->
                    {ok, 200, [], json([])};
                #{<<"sortAsc">> := false, <<"limit">> := 500, <<"until">> := 100000} ->
                    {ok, 200, [], json([#{transactions => [earn_interest(99001)]}])};
                #{<<"sortAsc">> := false} ->
                    {ok, 200, [], json([#{transactions => [earn_deposit(100001)]}])}
            end
        end
    ),

    {incomplete, [TXDeposit], State1} = ?MUT:account_transactions(State0),
    {incomplete, [TXInterest], State2} = ?MUT:account_transactions(State1),
    {incomplete, [], State3} = ?MUT:account_transactions(State2),
    {complete, [], _State4} = ?MUT:account_transactions(State3),

    ?assertMatch(
        #{
            source_id := <<"GHIJK34L5">>,
            datetime := {{1970, 1, 1}, {0, 1, 40}},
            direction := in,
            asset := #{symbol := <<"BTC">>},
            amount := {100000001, -8},
            type := undefined,
            description := <<"Earn Deposit">>
        },
        TXDeposit
    ),
    ?assertMatch(
        #{
            source_id := <<"GHIJK34L4">>,
            datetime := {{1970, 1, 1}, {0, 1, 39}},
            direction := in,
            asset := #{symbol := <<"BTC">>},
            amount := {200000002, -8},
            type := undefined,
            description := <<"Interest">>
        },
        TXInterest
    ),

    folio_meck:unload(?MOCK_MODS).

url_for_path(P) ->
    BasePath = <<"https://api.gemini.com">>,
    <<BasePath/binary, P/binary>>.

json(M) ->
    jsx:encode(M).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

credentials() ->
    [#{fields := Fields}] = ?MUT:setup_properties(),
    maps:map(
        fun(K, _V) ->
            BinK = erlang:atom_to_binary(K),
            <<<<"test_">>/binary, BinK/binary>>
        end,
        Fields
    ).

earn_deposit(DT) ->
    #{
        <<"earnTransactionId">> => <<"GHIJK34L5">>,
        <<"transactionType">> => <<"Deposit">>,
        <<"amountCurrency">> => <<"BTC">>,
        <<"amount">> => 1.00000001,
        <<"priceCurrency">> => <<"USD">>,
        <<"priceAmount">> => 64598.01,
        <<"dateTime">> => DT
    }.

earn_interest(DT) ->
    #{
        <<"earnTransactionId">> => <<"GHIJK34L4">>,
        <<"transactionType">> => <<"Interest">>,
        <<"amountCurrency">> => <<"BTC">>,
        <<"amount">> => 2.00000002,
        <<"priceCurrency">> => <<"USD">>,
        <<"priceAmount">> => 64598.01,
        <<"dateTime">> => DT
        %"dateTime"  => 1638809712388
    }.

trade_buy(TS) ->
    #{
        <<"price">> => <<"3648.09">>,
        <<"amount">> => <<"0.0027343246">>,
        <<"timestamp">> => TS,
        <<"timestampms">> => TS * 1000,
        <<"type">> => <<"Buy">>,
        <<"aggressor">> => true,
        <<"fee_currency">> => <<"USD">>,
        <<"fee_amount">> => <<"0.024937655575035">>,
        <<"tid">> => 107317526,
        <<"order_id">> => <<"107317524">>,
        <<"exchange">> => <<"gemini">>,
        <<"is_clearing_fill">> => false,
        <<"symbol">> => <<"BTCUSD">>
    }.
trade_sell(TS) ->
    #{
        <<"price">> => <<"3649.99">>,
        <<"amount">> => <<"0.0012343246">>,
        <<"timestamp">> => TS,
        <<"timestampms">> => TS * 1000,
        <<"type">> => <<"Sell">>,
        <<"aggressor">> => true,
        <<"fee_currency">> => <<"USD">>,
        <<"fee_amount">> => <<"0.012837655575035">>,
        <<"tid">> => 107317527,
        <<"order_id">> => <<"107317524">>,
        <<"exchange">> => <<"gemini">>,
        <<"is_clearing_fill">> => false,
        <<"symbol">> => <<"BTCUSD">>
    }.

transfer_deposit(TSMS) ->
    #{
        <<"type">> => <<"Deposit">>,
        <<"status">> => <<"Advanced">>,
        <<"timestampms">> => TSMS,
        <<"eid">> => 320033681,
        <<"currency">> => <<"USD">>,
        <<"amount">> => <<"125.00">>,
        <<"method">> => <<"CreditCard">>
    }.
transfer_withdrawal(TSMS) ->
    #{
        <<"type">> => <<"Withdrawal">>,
        <<"status">> => <<"Complete">>,
        <<"timestampms">> => TSMS,
        <<"eid">> => 320033682,
        <<"currency">> => <<"USD">>,
        <<"amount">> => <<"100.00">>,
        <<"method">> => <<"ACH">>
    }.

transfer_reward(TSMS) ->
    #{
        <<"type">> => <<"Reward">>,
        <<"status">> => <<"Advanced">>,
        <<"timestampms">> => TSMS,
        <<"eid">> => 320033683,
        <<"currency">> => <<"USD">>,
        <<"amount">> => <<"25.00">>,
        <<"method">> => <<"CreditCard">>
    }.
