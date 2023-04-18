-module(folio_ethplorer_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_ethplorer).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

-define(TOKEN_ADDR_LRC, <<"test_token_addr_lrc">>).
-define(TOKEN_ADDR_ETH, <<"native">>).

accounts_address_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_eth_address">>,

    ok = expect_credentials(Addr),

    State0 = ?MUT:accounts_init(Integration),

    URL = url_for_path(
        <<<<"/getAddressInfo/">>/binary, Addr/binary, <<"?apiKey=freekey">>/binary>>
    ),
    io:format("URL ~p~n", [URL]),
    BodyResp = addr_resp(),
    ok = meck:expect(
        hackney,
        request,
        [get, URL, [], [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {complete, [Acct], _State1} = ?MUT:accounts(State0),

    ?assertMatch(
        #{
            balances := [
                #{
                    balance := {334792427825, -11},
                    asset := #{symbol := <<"ETH">>, id := ?TOKEN_ADDR_ETH}
                },
                #{
                    balance := {2210668711958, -8},
                    asset := #{symbol := <<"LRC">>, id := ?TOKEN_ADDR_LRC}
                }
            ],
            id := Addr
        },
        Acct
    ),

    folio_meck:unload(?MOCK_MODS).

empty_accounts_address_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_eth_address">>,

    ok = expect_credentials(Addr),

    State0 = ?MUT:accounts_init(Integration),

    URL = url_for_path(
        <<<<"/getAddressInfo/">>/binary, Addr/binary, <<"?apiKey=freekey">>/binary>>
    ),
    io:format("URL ~p~n", [URL]),
    BodyResp = empty_addr_resp(),
    ok = meck:expect(
        hackney,
        request,
        [get, URL, [], [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {complete, [Acct], _State1} = ?MUT:accounts(State0),

    ?assertMatch(
        #{
            balances := [
                #{balance := {0, 0}, asset := #{symbol := <<"ETH">>, id := ?TOKEN_ADDR_ETH}}
            ],
            id := Addr
        },
        Acct
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_ethereum_address">>,
    OtherAddress = <<"other_ethereum_address">>,
    AccountID = Addr,

    Account = #{id => AccountID},

    ok = expect_credentials(Addr),

    State0 = ?MUT:account_transactions_init(Integration, Account),
    AddressHistoryURL0 = url_for_path(
        <<<<"/getAddressHistory/">>/binary, Addr/binary, <<"?apiKey=freekey&limit=1000">>/binary>>
    ),
    AddressHistoryResp0 = json(#{
        operations => [
            transfer(
                100, <<"tx1">>, token_info(lrc), OtherAddress, Addr, <<"2000000000000000000">>
            ),
            transfer(200, <<"tx2">>, token_info(lrc), Addr, OtherAddress, <<"1000000000000000000">>)
        ]
    }),

    AddressTXURL0 = url_for_path(
        <<<<"/getAddressTransactions/">>/binary, Addr/binary,
            <<"?apiKey=freekey&limit=1000">>/binary>>
    ),
    AddressTXResp0 = json(
        [
            address_tx(
                100, <<"ethtx1">>, OtherAddress, Addr, 1.1478242782499998
            ),
            address_tx(
                200, <<"ethtx2">>, Addr, OtherAddress, 0.14782
            )
        ]
    ),

    ok = meck:expect(
        hackney,
        request,
        [
            {[get, AddressHistoryURL0, [], [], [with_body]], {ok, 200, [], AddressHistoryResp0}},
            {[get, AddressTXURL0, [], [], [with_body]], {ok, 200, [], AddressTXResp0}}
        ]
    ),

    {incomplete, [TX1, TX2], State1} = ?MUT:account_transactions(State0),
    {incomplete, [ETHTX1, ETHTX2], State2} = ?MUT:account_transactions(State1),
    {complete, [], _State3} = ?MUT:account_transactions(State2),

    ?assertMatch(
        #{
            amount := {2, 0},
            datetime := {{1970, 1, 1}, {0, 1, 40}},
            description := <<"transfer">>,
            direction := in,
            source_id :=
                <<"tx1">>,
            asset := #{symbol := <<"LRC">>, id := ?TOKEN_ADDR_LRC},
            type := undefined
        },
        TX1
    ),
    ?assertMatch(
        #{
            amount := {1, 0},
            datetime := {{1970, 1, 1}, {0, 3, 20}},
            description := <<"transfer">>,
            direction := out,
            source_id :=
                <<"tx2">>,
            asset := #{symbol := <<"LRC">>, id := ?TOKEN_ADDR_LRC},
            type := undefined
        },
        TX2
    ),
    ?assertMatch(
        #{
            amount := {11478242782499999, -16},
            datetime := {{1970, 1, 1}, {0, 1, 40}},
            description := <<"">>,
            direction := in,
            source_id :=
                <<"ethtx1">>,
            asset := #{symbol := <<"ETH">>, id := ?TOKEN_ADDR_ETH},
            type := undefined
        },
        ETHTX1
    ),
    ?assertMatch(
        #{
            amount := {14782, -5},
            datetime := {{1970, 1, 1}, {0, 3, 20}},
            description := <<"">>,
            direction := out,
            source_id :=
                <<"ethtx2">>,
            asset := #{symbol := <<"ETH">>, id := ?TOKEN_ADDR_ETH},
            type := undefined
        },
        ETHTX2
    ),

    folio_meck:unload(?MOCK_MODS).

url_for_path(P) ->
    BasePath = <<"https://api.ethplorer.io">>,
    <<BasePath/binary, P/binary>>.

json(M) ->
    jsx:encode(M).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

empty_addr_resp() ->
    json(#{
        % ETH specific information,
        <<"ETH">> => #{
            % balance in wei, as a string,
            <<"rawBalance">> => <<"0">>
        }
    }).

addr_resp() ->
    json(#{
        % ETH specific information,
        <<"ETH">> => #{
            % balance in wei, as a string,
            <<"rawBalance">> => <<"3347924278250000000">>
        },
        % exists if the specified address has any token balances
        <<"tokens">> => [
            #{
                <<"tokenInfo">> => token_info(lrc),
                <<"rawBalance">> => <<"22106687119580000000000">>
            }
        ]
    }).

expect_credentials(M) when is_map(M) ->
    ok = meck:expect(folio_credentials_store, get_credentials, ['_'], M),
    ok;
expect_credentials(Addr) when is_binary(Addr) ->
    ok = meck:expect(folio_credentials_store, get_credentials, ['_'], #{address => Addr}),
    ok.

transfer(Timestamp, TXHash, TokenInfo, From, To, Value) ->
    #{
        <<"timestamp">> => Timestamp,
        <<"transactionHash">> => TXHash,
        <<"tokenInfo">> => TokenInfo,
        <<"type">> => <<"transfer">>,
        <<"value">> => Value,
        <<"from">> => string:lowercase(From),
        <<"to">> => string:lowercase(To)
    }.

address_tx(Timestamp, TXHash, From, To, Value) ->
    #{
        <<"timestamp">> => Timestamp,
        <<"from">> => From,
        <<"to">> => To,
        <<"hash">> => TXHash,
        <<"value">> => Value,
        <<"usdPrice">> => 1538.33999999,
        <<"usdValue">> => 5150.24583420,
        <<"success">> => true
    }.

token_info(lrc) ->
    #{<<"address">> => ?TOKEN_ADDR_LRC, <<"symbol">> => <<"LRC">>, <<"decimals">> => <<"18">>}.
