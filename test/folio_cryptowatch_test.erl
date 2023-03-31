-module(folio_cryptowatch_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_cryptowatch).
-define(MOCK_MODS, [hackney, throttle]).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

get_asset_prices_test() ->
    load(),

    URL = url(<<"/markets/prices">>),
    io:format("URL ~p~n", [URL]),
    Resp = to_json(#{
        result => #{
            <<"market:binance:btcusd">> => 1,
            <<"market:coinbase:btcusd">> => 2,
            <<"market:coinbase:ethusd">> => 3,
            <<"market:bar:ethusd">> => 4
        }
    }),
    ok = meck:expect(
        hackney,
        request,
        [
            {[get, URL, '_', [], [with_body]], {ok, 200, [], Resp}}
        ]
    ),

    {ok, [Val1, Val2]} = ?MUT:get_asset_prices(),

    ?assertMatch(
        #{
            amount := {2, 0},
            symbol := <<"btc">>,
            currency := <<"usd">>
        },

        Val1
    ),
    ?assertMatch(
        #{
            amount := {3, 0},
            symbol := <<"eth">>,
            currency := <<"usd">>
        },

        Val2
    ),

    folio_meck:unload(?MOCK_MODS).

to_json(Obj) -> jsx:encode(Obj).

url(Path) ->
    <<<<"https://api.cryptowat.ch">>/binary, Path/binary>>.