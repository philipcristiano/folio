-module(folio_blockstream_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_blockstream).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

accounts_test() ->
    load(),

    IntegrationID = make_ref(),
    Integration = #{id => IntegrationID},
    Addr = <<"test_bitcoin_address">>,

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

url_for_path(P) ->
    BasePath = <<"https://blockstream.info">>,
    <<BasePath/binary, P/binary>>.

json(M) when is_map(M) ->
    jsx:encode(M).

load() ->
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.
