-module(folio_ethplorer_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_ethplorer).
-define(MOCK_MODS, [folio_credentials_store, hackney, throttle]).

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
                #{balance := {334792427825, -11}, symbol := <<"ETH">>},
                #{balance := {2210668711958, -8}, symbol := <<"LRC">>}
            ],
            id := Addr
        },
        Acct
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
                <<"tokenInfo">> => #{<<"symbol">> => <<"LRC">>, <<"decimals">> => <<"18">>},
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
