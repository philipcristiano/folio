-module(folio_loopring_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_loopring).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

accounts_address_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},

    AccountID = <<"1212">>,
    APIKey = <<"test_key">>,
    ok = expect_credentials(APIKey, AccountID),
    Headers = headers(APIKey),

    State0 = ?MUT:accounts_init(Integration),

    TokenURL = url_for_path(<<"/api/v3/exchange/tokens">>),
    BalanceURL = url_for_path(<<<<"/api/v3/user/balances?accountId=">>/binary, AccountID/binary>>),

    ok = meck:expect(
        hackney,
        request,
        [
            {[get, TokenURL, [], [], [with_body]], {ok, 200, [], tokens_resp()}},
            {[get, BalanceURL, Headers, [], [with_body]], {ok, 200, [], balance_resp(AccountID)}}
        ]
    ),

    {incomplete, [], State1} = ?MUT:accounts(State0),
    {complete, [Acct], _State2} = ?MUT:accounts(State1),

    ?assertMatch(
        #{
            balances := [
                #{balance := {163599196, -9}, symbol := <<"ETH">>},
                #{balance := {729951761, -8}, symbol := <<"LRC">>}
            ],
            id := AccountID
        },
        Acct
    ),

    folio_meck:unload(?MOCK_MODS).

url_for_path(P) ->
    BasePath = <<"https://api3.loopring.io">>,
    <<BasePath/binary, P/binary>>.

json(M) ->
    jsx:encode(M).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

expect_credentials(Key, AccountID) ->
    ok = meck:expect(folio_credentials_store, get_credentials, ['_'], #{
        key => Key, account_id => AccountID
    }),
    ok.

tokens_resp() ->
    json([
        #{
            <<"address">> =>
                <<"0x0000000000000000000000000000000000000000">>,
            <<"decimals">> => 18,
            <<"enabled">> => true,
            <<"fastWithdrawLimit">> => <<"100000000000000000000">>,
            <<"gasAmounts">> =>
                #{
                    <<"deposit">> => <<"110000">>,
                    <<"distribution">> => <<"85000">>
                },
            <<"luckyTokenAmounts">> =>
                #{
                    <<"dust">> => <<"50000000000000">>,
                    <<"maximum">> => <<"1000000000000000000000">>,
                    <<"minimum">> => <<"50000000000000">>
                },
            <<"name">> => <<"Ethereum">>,
            <<"orderAmounts">> =>
                #{
                    <<"dust">> => <<"200000000000000">>,
                    <<"maximum">> => <<"1000000000000000000000">>,
                    <<"minimum">> => <<"1700000000000000">>
                },
            <<"precision">> => 7,
            <<"precisionForOrder">> => 3,
            <<"symbol">> => <<"ETH">>,
            <<"tokenId">> => 0,
            <<"type">> => <<"ETH">>
        },
        #{
            <<"address">> =>
                <<"0xbbbbca6a901c926f240b89eacb641d8aec7aeafd">>,
            <<"decimals">> => 18,
            <<"enabled">> => true,
            <<"fastWithdrawLimit">> => <<"750000000000000000000000">>,
            <<"gasAmounts">> =>
                #{
                    <<"deposit">> => <<"150000">>,
                    <<"distribution">> => <<"101827">>
                },
            <<"luckyTokenAmounts">> =>
                #{
                    <<"dust">> => <<"50000000000000000">>,
                    <<"maximum">> => <<"5000000000000000000000000">>,
                    <<"minimum">> => <<"50000000000000000">>
                },
            <<"name">> => <<"Loopring">>,
            <<"orderAmounts">> =>
                #{
                    <<"dust">> => <<"2000000000000000000">>,
                    <<"maximum">> => <<"5000000000000000000000000">>,
                    <<"minimum">> => <<"5000000000000000000">>
                },
            <<"precision">> => 3,
            <<"precisionForOrder">> => 3,
            <<"symbol">> => <<"LRC">>,
            <<"tokenId">> => 1,
            <<"type">> => <<"ERC20">>
        }
    ]).

balance_resp(AccountIDBin) when is_binary(AccountIDBin) ->
    AccountID = erlang:binary_to_integer(AccountIDBin),
    json([
        #{
            <<"accountId">> => AccountID,
            <<"locked">> => <<"0">>,
            <<"pending">> =>
                #{<<"deposit">> => <<"0">>, <<"withdraw">> => <<"0">>},
            <<"tokenId">> => 0,
            <<"total">> => <<"163599196000000000">>
        },
        #{
            <<"accountId">> => AccountID,
            <<"locked">> => <<"0">>,
            <<"pending">> =>
                #{<<"deposit">> => <<"0">>, <<"withdraw">> => <<"0">>},
            <<"tokenId">> => 1,
            <<"total">> => <<"7299517610000000000">>
        }
    ]).

headers(Key) ->
    [{<<"X-API-KEY">>, Key}].
