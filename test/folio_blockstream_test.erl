-module(folio_blockstream_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_blockstream).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle, btcau]).

accounts_address_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_bitcoin_address">>,

    ok = expect_credentials(Addr),

    State0 = ?MUT:accounts_init(Integration),

    URL = url_for_path(<<<<"/api/address/">>/binary, Addr/binary>>),
    Funded = 10,
    Spent = 3,
    BodyResp = addr_resp(Funded, Spent),
    ok = meck:expect(
        hackney,
        request,
        [get, URL, [], [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {complete, [Acct], _State1} = ?MUT:accounts(State0),

    ?assertMatch(
        #{
            balances := [#{balance := {7, -8}, asset := #{symbol := <<"BTC">>}}],
            id := <<"test_bitcoin_address">>
        },
        Acct
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_xyzpub_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_bitcoin_address">>,

    Pub = <<"test_xyz_pub">>,

    ok = expect_credentials(#{xyzpub => Pub, format => <<"p2sh">>}),

    State0 = ?MUT:accounts_init(Integration),

    ok = meck:expect(btcau, pub_to_p2wpkh_in_p2sh, fun(PubIn, Derv) ->
        PubIn = Pub,
        case binary:split(Derv, <<"/">>, [global]) of
            [<<"M">>, A, B] -> <<A/binary, <<".">>/binary, B/binary>>
        end
    end),

    ok = meck:expect(
        hackney,
        request,
        fun(
            get,
            <<"https://blockstream.info/api/address/", RequestAdddr/binary>>,
            [],
            [],
            [with_body]
        ) ->
            Resp =
                case RequestAdddr of
                    <<"0.2">> -> addr_resp(1000, 100);
                    <<"1.3">> -> addr_resp(500, 50);
                    _ -> addr_resp(0, 0)
                end,
            {ok, 200, [], Resp}
        end
    ),

    AccountReturns = run_accounts(State0, 50),
    match_accounts(
        [
            {incomplete, [acct(<<"0.0">>, {0, 0})]},
            {incomplete, [acct(<<"0.1">>, {0, 0})]},
            {incomplete, [acct(<<"0.2">>, {9, -6})]},
            {incomplete, [acct(<<"0.3">>, {0, 0})]},
            {incomplete, [acct(<<"0.4">>, {0, 0})]},
            {incomplete, [acct(<<"0.5">>, {0, 0})]},
            {incomplete, [acct(<<"0.6">>, {0, 0})]},
            {incomplete, [acct(<<"0.7">>, {0, 0})]},
            {incomplete, [acct(<<"0.8">>, {0, 0})]},
            {incomplete, [acct(<<"0.9">>, {0, 0})]},
            {incomplete, [acct(<<"0.10">>, {0, 0})]},
            {incomplete, [acct(<<"0.11">>, {0, 0})]},
            {incomplete, [acct(<<"0.12">>, {0, 0})]},
            {incomplete, [acct(<<"0.13">>, {0, 0})]},
            {incomplete, [acct(<<"0.14">>, {0, 0})]},
            {incomplete, [acct(<<"0.15">>, {0, 0})]},
            {incomplete, [acct(<<"0.16">>, {0, 0})]},
            {incomplete, [acct(<<"0.17">>, {0, 0})]},
            {incomplete, [acct(<<"0.18">>, {0, 0})]},
            {incomplete, [acct(<<"0.19">>, {0, 0})]},
            {incomplete, [acct(<<"0.20">>, {0, 0})]},
            {incomplete, [acct(<<"0.21">>, {0, 0})]},
            {incomplete, [acct(<<"0.22">>, {0, 0})]},
            {incomplete, [acct(<<"0.23">>, {0, 0})]},
            {incomplete, [acct(<<"1.0">>, {0, 0})]},
            {incomplete, [acct(<<"1.1">>, {0, 0})]},
            {incomplete, [acct(<<"1.2">>, {0, 0})]},
            {incomplete, [acct(<<"1.3">>, {45, -7})]},
            {incomplete, [acct(<<"1.4">>, {0, 0})]},
            {incomplete, [acct(<<"1.5">>, {0, 0})]},
            {incomplete, [acct(<<"1.6">>, {0, 0})]},
            {incomplete, [acct(<<"1.7">>, {0, 0})]},
            {incomplete, [acct(<<"1.8">>, {0, 0})]},
            {incomplete, [acct(<<"1.9">>, {0, 0})]},
            {incomplete, [acct(<<"1.10">>, {0, 0})]},
            {incomplete, [acct(<<"1.11">>, {0, 0})]},
            {incomplete, [acct(<<"1.12">>, {0, 0})]},
            {incomplete, [acct(<<"1.13">>, {0, 0})]},
            {incomplete, [acct(<<"1.14">>, {0, 0})]},
            {incomplete, [acct(<<"1.15">>, {0, 0})]},
            {incomplete, [acct(<<"1.16">>, {0, 0})]},
            {incomplete, [acct(<<"1.17">>, {0, 0})]},
            {incomplete, [acct(<<"1.18">>, {0, 0})]},
            {incomplete, [acct(<<"1.19">>, {0, 0})]},
            {incomplete, [acct(<<"1.20">>, {0, 0})]},
            {incomplete, [acct(<<"1.21">>, {0, 0})]},
            {incomplete, [acct(<<"1.22">>, {0, 0})]},
            {incomplete, [acct(<<"1.23">>, {0, 0})]},
            {incomplete, [acct(<<"1.24">>, {0, 0})]},
            {complete, []}
        ],
        AccountReturns
    ),
    folio_meck:unload(?MOCK_MODS).

accounts_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_bitcoin_address">>,
    AccountID = Addr,

    Account = #{id => AccountID},

    ok = expect_credentials(Addr),

    BodyResp = json(tx_p2sh_out(Addr)),
    State0 = ?MUT:account_transactions_init(Integration, Account),
    URL = url_for_path(<<<<"/api/address/">>/binary, Addr/binary, <<"/txs/chain/0">>/binary>>),

    ok = meck:expect(
        hackney,
        request,
        [get, URL, [], [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {incomplete, [P2SHOut], _State1} = ?MUT:account_transactions(State0),

    ?assertMatch(
        #{
            amount := {10383, -7},
            datetime := {{1970, 1, 1}, {4, 28, 5}},
            description := <<>>,
            direction := out,
            source_id :=
                <<"8529be643ed0200777afa7b389debec28b8ece52a0318d98575b6a107c19e529">>,
            line := <<"">>,
            asset := #{symbol := <<"BTC">>},
            type := undefined
        },
        P2SHOut
    ),

    folio_meck:unload(?MOCK_MODS).

url_for_path(P) ->
    BasePath = <<"https://blockstream.info">>,
    <<BasePath/binary, P/binary>>.

json(M) ->
    jsx:encode(M).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

addr_resp(Funded, Spent) ->
    json(#{
        <<"chain_stats">> => #{
            <<"funded_txo_sum">> => Funded,
            <<"spent_txo_sum">> => Spent
        }
    }).
expect_credentials(M) when is_map(M) ->
    ok = meck:expect(folio_credentials_store, get_credentials, ['_'], M),
    ok;
expect_credentials(Addr) when is_binary(Addr) ->
    ok = meck:expect(folio_credentials_store, get_credentials, ['_'], #{address => Addr}),
    ok.

tx_p2sh_out(Addr) ->
    [
        #{
            <<"txid">> => <<"8529be643ed0200777afa7b389debec28b8ece52a0318d98575b6a107c19e529">>,
            <<"vout">> => [],
            <<"vin">> => [
                #{
                    <<"txid">> =>
                        <<"7decd2f4fdf2faecd1c2ddgg8a27d4b98405064a76d32403a91896331eecfc2b">>,
                    <<"prevout">> => #{
                        <<"scriptpubkey">> => <<"a914b3948e3cd55956c5e9da00112fba89397c94ca7387">>,
                        <<"scriptpubkey_asm">> =>
                            <<"OP_HASH160 OP_PUSHBYTES_20 b9384ecd353569c5e9de00112fba389979c4ca73 OP_EQUAL">>,
                        <<"scriptpubkey_type">> => <<"p2sh">>,
                        <<"scriptpubkey_address">> => Addr,
                        <<"value">> => 103830
                    }
                }
            ],
            <<"weight">> => 162,
            <<"fee">> => 50096,
            <<"status">> => #{
                <<"confirmed">> => true,
                <<"block_height">> => 66,
                <<"block_hash">> => <<"0000000000000ba00a1ca1d63c704802c1508c92ec5860be7d352e433">>,
                <<"block_time">> => 16085
            }
        }
    ].

acct(ID, Bal) ->
    #{
        balances => [#{balance => Bal, asset => #{symbol => <<"BTC">>}}],
        id => ID
    }.

run_accounts(State, Iterations) when Iterations > 0 ->
    {Complete, Accounts, NextState} = ?MUT:accounts(State),
    [{Complete, Accounts} | run_accounts(NextState, Iterations - 1)];
run_accounts(_State, _Iterations) ->
    [].

match_accounts([], []) ->
    ok;
match_accounts([A1 | TA], [A2 | TB]) ->
    ?assertEqual(A1, A2),
    match_accounts(TA, TB).
