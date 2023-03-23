-module(folio_coinbase_pro_api_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_coinbase_pro_api).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

accounts_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},

    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], credentials()),

    State0 = ?MUT:accounts_init(Integration),

    URL = url_for_path(<<"/accounts">>),
    BodyResp = json([
        #{
            <<"id">> => <<"test_account_id_1">>,
            <<"currency">> => <<"BTC">>,
            <<"balance">> => <<"1.23">>
        },
        #{
            <<"id">> => <<"test_account_id_2">>,
            <<"currency">> => <<"USD">>,
            <<"balance">> => <<"4.56">>
        }
    ]),
    ok = meck:expect(
        hackney,
        request,
        [get, URL, '_', [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {complete, [Acct1, Acct2], _State1} = ?MUT:accounts(State0),

    ?assertMatch(
        #{
            balances := [#{balance := {123, -2}, symbol := <<"BTC">>}],
            id := <<"test_account_id_1">>
        },
        Acct1
    ),
    ?assertMatch(
        #{
            balances := [#{balance := {456, -2}, symbol := <<"USD">>}],
            id := <<"test_account_id_2">>
        },
        Acct2
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    AccountID = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
    Account = #{id => AccountID},

    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], credentials()),

    State0 = ?MUT:account_transactions_init(Integration, Account),
    URLAccount = url_for_path(<<<<"/accounts/">>/binary, AccountID/binary>>),
    AccountResp = json(#{<<"currency">> => <<"BTC">>}),

    URL1 = url_for_path(<<<<"/accounts/">>/binary, AccountID/binary, <<"/ledger">>/binary>>),
    BodyResp1 = json([
        transfer_in(AccountID)
    ]),
    URL2 = url_for_path(
        <<<<"/accounts/">>/binary, AccountID/binary, <<"/ledger?before=BEFORE1">>/binary>>
    ),
    BodyResp2 = json([
        transfer_deposit(),
        transfer_withdraw(),
        match_sell(),
        match_buy(),
        fee(),
        conversion()
    ]),
    ok = meck:expect(
        hackney,
        request,
        [
            {[get, URLAccount, '_', '_', [with_body]], {ok, 200, [], AccountResp}},
            {
                [get, URL1, '_', [], [with_body]],
                {ok, 200, [{<<"CB-BEFORE">>, <<"BEFORE1">>}], BodyResp1}
            },
            {
                [get, URL2, '_', [], [with_body]],
                {ok, 200, [], BodyResp2}
            }
        ]
    ),

    % Get account currency
    {incomplete, [], State1} = ?MUT:account_transactions(State0),
    % Get a transaction, assuming a second query will be needed
    {incomplete, [TX1], State2} = ?MUT:account_transactions(State1),
    % Get the remaining examples
    {complete, [TXDeposit, TXWithdraw, TXSell, TXBuy, TXFee, TXConversion], _State3} = ?MUT:account_transactions(
        State2
    ),

    #{<<"id">> := TransferInID} = transfer_in(AccountID),
    ?assertMatch(
        #{
            source_id := TransferInID,
            datetime := {{2019, 6, 11}, {22, 11, 56}},
            direction := in,
            symbol := <<"BTC">>,
            amount := {322, -2},
            type := undefined,
            description := <<"sell">>
        },
        TX1
    ),
    ?assertMatch(
        #{
            amount := {34698, -4},
            datetime := {{2021, 11, 2}, {16, 17, 06}},
            description := <<"deposit">>,
            direction := in,
            source_id := <<"28879">>,
            symbol := <<"BTC">>,
            type := undefined
        },
        TXDeposit
    ),
    ?assertMatch(
        #{
            amount := {1064, -2},
            datetime := {{2022, 12, 2}, {15, 49, 26}},
            description := <<"withdraw">>,
            direction := out,
            source_id := <<"24436872562">>,
            symbol := <<"BTC">>,
            type := undefined
        },
        TXWithdraw
    ),
    ?assertMatch(
        #{
            amount := {11, -3},
            datetime := {{2021, 11, 20}, {15, 6, 35}},
            description := <<"BTC-USD">>,
            direction := out,
            source_id := <<"135010173">>,
            symbol := <<"BTC">>,
            type := undefined
        },
        TXSell
    ),
    ?assertMatch(
        #{
            amount := {12, -3},
            datetime := {{2021, 11, 20}, {15, 6, 36}},
            description := <<"BTC-USD">>,
            direction := in,
            source_id := <<"135010174">>,
            symbol := <<"BTC">>,
            type := undefined
        },
        TXBuy
    ),
    ?assertMatch(
        #{
            amount := {194964, -5},
            datetime := {{2022, 9, 21}, {9, 50, 19}},
            description := <<"fee BTC-USD">>,
            direction := out,
            source_id := <<"33513">>,
            symbol := <<"BTC">>,
            type := fee
        },
        TXFee
    ),
    ?assertMatch(
        #{
            amount := {1, 3},
            datetime := {{2023, 4, 15}, {13, 32, 54}},
            description := <<"stablecoin conversion">>,
            direction := out,
            source_id := <<"5832056521">>,
            symbol := <<"BTC">>,
            type := undefined
        },
        TXConversion
    ),

    folio_meck:unload(?MOCK_MODS).

url_for_path(P) ->
    BasePath = <<"https://api.exchange.coinbase.com">>,
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
    #{secret := Secret} =
        CredentialsProps = maps:map(
            fun(K, _V) ->
                BinK = erlang:atom_to_binary(K),
                <<<<"test_">>/binary, BinK/binary>>
            end,
            Fields
        ),
    B64Secret = base64:encode(Secret),
    CredentialsProps#{secret => B64Secret}.

transfer_withdraw() ->
    #{
        <<"amount">> =>
            <<"-10.6400000000000000">>,
        <<"balance">> =>
            <<"0.0006261531002000">>,
        <<"created_at">> =>
            <<"2022-12-02T15:49:26.662677Z">>,
        <<"details">> =>
            #{
                <<"transfer_id">> =>
                    <<"5f1daae2-e3bc-446e-9c46-08081d0c0d3d">>,
                <<"transfer_type">> =>
                    <<"withdraw">>
            },
        <<"id">> =>
            <<"24436872562">>,
        <<"type">> =>
            <<"transfer">>
    }.

transfer_in(AccountID) ->
    #{
        <<"created_at">> => <<"2019-06-11T22:11:56.382749Z">>,
        <<"id">> => <<"1444415179">>,
        <<"amount">> => <<"3.2200000000000000">>,
        <<"balance">> => <<"3.2200000000000000">>,
        <<"type">> => <<"transfer">>,
        <<"details">> => #{
            <<"to">> => AccountID,
            <<"from">> => <<"20640810-6219-4d3b-95f4-5e1741dd6ea4">>,
            <<"profile_transfer_id">> => <<"1f854356-4923-4b10-8db1-d82f7fae8eda">>
        }
    }.

transfer_deposit() ->
    #{
        <<"amount">> =>
            <<"3.4698000000000000">>,
        <<"balance">> =>
            <<"3.4698000000000000">>,
        <<"created_at">> =>
            <<"2021-11-02T16:17:06.838496Z">>,
        <<"details">> =>
            #{
                <<"transfer_id">> =>
                    <<"9ad86313-5acf-46a5-848b-f108cbf305e0">>,
                <<"transfer_type">> =>
                    <<"deposit">>
            },
        <<"id">> =>
            <<"28879">>,
        <<"type">> =>
            <<"transfer">>
    }.

match_sell() ->
    #{
        <<"amount">> =>
            <<"-0.0110000000000000">>,
        <<"balance">> =>
            <<"0.0000000000000000">>,
        <<"created_at">> =>
            <<"2021-11-20T15:06:35.647091Z">>,
        <<"details">> =>
            #{
                <<"order_id">> =>
                    <<"a3db205f-1c50-4036-b552-403bb49d78ba">>,
                <<"product_id">> =>
                    <<"BTC-USD">>,
                <<"trade_id">> =>
                    <<"1365">>
            },
        <<"id">> =>
            <<"135010173">>,
        <<"type">> =>
            <<"match">>
    }.
match_buy() ->
    #{
        <<"amount">> =>
            <<"0.0120000000000000">>,
        <<"balance">> =>
            <<"0.0000000000000000">>,
        <<"created_at">> =>
            <<"2021-11-20T15:06:36.647091Z">>,
        <<"details">> =>
            #{
                <<"order_id">> =>
                    <<"a3db205f-1c50-4036-b552-403bb49d78bb">>,
                <<"product_id">> =>
                    <<"BTC-USD">>,
                <<"trade_id">> =>
                    <<"1366">>
            },
        <<"id">> =>
            <<"135010174">>,
        <<"type">> =>
            <<"match">>
    }.

fee() ->
    #{
        <<"amount">> =>
            <<"-1.9496400000000000">>,
        <<"balance">> =>
            <<"10.6406261531002000">>,
        <<"created_at">> =>
            <<"2022-09-21T09:50:19.038872Z">>,
        <<"details">> =>
            #{
                <<"order_id">> =>
                    <<"de9ad3f5-215f-4046-88df-7466dbf4bcf0">>,
                <<"product_id">> =>
                    <<"BTC-USD">>,
                <<"trade_id">> =>
                    <<"33514">>
            },
        <<"id">> =>
            <<"33513">>,
        <<"type">> =>
            <<"fee">>
    }.

conversion() ->
    #{
        <<"amount">> =>
            <<"-1000.0000000000000000">>,
        <<"balance">> =>
            <<"0.0000000000000000">>,
        <<"created_at">> =>
            <<"2023-04-15T13:32:54.035877Z">>,
        <<"details">> =>
            #{
                <<"conversion_id">> =>
                    <<"05b716eb-ae6d-4c5f-8408-0431705978c6">>
            },
        <<"id">> =>
            <<"5832056521">>,
        <<"type">> =>
            <<"conversion">>
    }.
