-module(folio_blockstream_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_blockstream).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

accounts_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_bitcoin_address">>,

    ok = expect_credentials(Addr),
    ok = meck:expect(folio_credentials_store, get_credentials, [IntegrationID], #{address => Addr}),

    State0 = ?MUT:accounts_init(Integration),

    URL = url_for_path(<<<<"/api/address/">>/binary, Addr/binary>>),
    Funded = 10,
    Spent = 3,
    BodyResp = json(#{
        <<"chain_stats">> => #{
            <<"funded_txo_sum">> => Funded,
            <<"spent_txo_sum">> => Spent
        }
    }),
    ok = meck:expect(
        hackney,
        request,
        [get, URL, [], [], [with_body]],
        {ok, 200, [], BodyResp}
    ),

    {complete, [Acct], _State1} = ?MUT:accounts(State0),

    ?assertMatch(
        #{
            balances := [#{balance := {7, -8}, symbol := <<"BTC">>}],
            id := <<"test_bitcoin_address">>
        },
        Acct
    ),

    folio_meck:unload(?MOCK_MODS).

accounts_transactions_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    AccountID = make_ref(),

    Account = #{id => AccountID},

    Addr = <<"test_bitcoin_address">>,
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
            symbol := <<"BTC">>,
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

expect_credentials(Addr) ->
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
