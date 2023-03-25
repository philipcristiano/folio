-module(folio_coingecko_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, folio_coingecko).
-define(MOCK_MODS, [hackney, throttle]).

load() ->
    application:ensure_all_started(qdate),
    folio_meck:load(?MOCK_MODS),
    ok = meck:expect(throttle, check, ['_', '_'], {ok, 1, 1}),
    ok.

price_for_asset_test() ->
    load(),

    AssetID = <<"bitcoin">>,
    Asset = #{id => AssetID},

    URL = price_url_for_asset_id(AssetID),
    io:format("URL ~p~n", [URL]),
    Resp = to_json(#{
        AssetID => #{
            <<"last_updated_at">> => 1679762765,
            <<"usd">> => 27868.57
        }
    }),
    ok = meck:expect(
        hackney,
        request,
        [
            {[get, URL, '_', [], [with_body]], {ok, 200, [], Resp}}
        ]
    ),

    {ok, Val} = ?MUT:price_for_asset(Asset),

    ?assertMatch(
        #{
            amount := {2786857, -2},
            asset_id := AssetID,
            currency := <<"usd">>
        },
        Val
    ),

    folio_meck:unload(?MOCK_MODS).

price_url_for_asset_id(ID) ->
    <<<<"https://api.coingecko.com/api/v3/simple/price?vs_currencies=usd&precision=full&include_last_updated_at=true&ids=">>/binary,
        ID/binary>>.

to_json(Obj) -> jsx:encode(Obj).
