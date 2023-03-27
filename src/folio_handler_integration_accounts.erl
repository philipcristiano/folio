-module(folio_handler_integration_accounts).

-include_lib("kernel/include/logger.hrl").

-export([init/2, trails/0, handle_req/4, post_req/2]).

-define(PATH, <<"/integrations/:integration_id/accounts">>).

trails() ->
    Metadata = folio_http:make_get(getAccounts, [integration_id], return_schema()),
    State = #{},
    [
        trails:trail(?PATH, ?MODULE, State, Metadata)
    ].

return_schema() ->
    #{
        type => object,
        description => <<"Accounts">>,
        properties => #{
            <<"accounts">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of accounts">>
            }
        }
    }.

init(Req, Opts) ->
    MatchReq = Req#{path => ?PATH},
    folio_http_session:init(Req),
    {specified_handler, MatchReq, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    _Params = #{integration_id := IntegrationID},
    _Body,
    State
) ->
    ?LOG_DEBUG(#{message => getAccounts}),

    {ok, C} = fdb:connect(),
    {ok, Accounts} = folio_integration:integration_accounts(C, IntegrationID),
    AccountsWithFiat = lists:map(
        fun(Act = #{symbol := Symbol, balance := Bal}) ->
            case folio_prices:asset_for_symbol(Symbol) of
                undefined ->
                    Act;
                #{external_id := AssetID} ->
                    {ok, #{amount := Price}} = folio_prices:price_for_asset_id(C, AssetID),
                    FiatValue = multiply_float_to_float(Price, Bal),
                    NewAct = Act#{fiat_value => FiatValue},
                    io:format("account ~p~n", [{Act, Symbol, AssetID, Price, FiatValue, NewAct}]),
                    NewAct
            end
        end,
        Accounts
    ),

    FiatValues = lists:filtermap(
        fun(A) ->
            case A of
                #{fiat_value := FV} -> {true, FV};
                _ -> false
            end
        end,
        AccountsWithFiat
    ),

    FiatTotal = sum_floats(FiatValues),

    fdb:close(C),

    {Req, 200, #{fiat_total => FiatTotal, accounts => AccountsWithFiat}, State}.

post_req(_Response, _State) ->
    ok.

multiply_float_to_float(ABin, BBin) ->
    A = to_decimal(ABin),
    B = to_decimal(BBin),
    C = decimal:mult(A, B),
    CBin = decimal:to_binary(C),
    CBin.

to_decimal(F) when is_binary(F) ->
    L = size(F),
    decimal:to_decimal(F, #{precision => L, rounding => round_floor}).

sum_floats(FloatValues) when is_list(FloatValues) ->
    DecimalTotal = lists:foldl(
        fun(FVal, Total) ->
            Val = to_decimal(FVal),
            decimal:add(Val, Total)
        end,
        {0, 0},
        FloatValues
    ),
    decimal:to_binary(DecimalTotal).
