-module(folio_accounts).

-export([
    add_fiat_value_for_accounts/2,
    fiat_value_of_accounts/1
]).

add_fiat_value_for_accounts(C, Accounts) ->
    lists:map(
        fun(Act = #{symbol := Symbol, balance := Bal}) ->
            case folio_prices:asset_for_symbol(Symbol) of
                undefined ->
                    Act;
                #{external_id := AssetID} ->
                    update_account_for_asset_id(C, Act, Bal, AssetID)
            end
        end,
        Accounts
    ).

fiat_value_of_accounts(Accounts) ->
    FiatValues = lists:filtermap(
        fun(A) ->
            case A of
                #{fiat_value := FV} -> {true, FV};
                _ -> false
            end
        end,
        Accounts
    ),

    FiatTotalDecimal = folio_math:sum(FiatValues),
    FiatTotal = folio_math:decimal_to_presentable_value(FiatTotalDecimal),
    FiatTotal.

update_account_for_asset_id(C, Act, Bal, AssetID) ->
    PriceResp = folio_prices:price_for_asset_id(C, AssetID),
    update_account_with_asset_price(Act, Bal, PriceResp).
update_account_with_asset_price(Act, _Bal, undefined) ->
    Act;
update_account_with_asset_price(Act, Bal, {ok, #{amount := Price}}) ->
    FiatValue = folio_math:multiply(Price, Bal),
    NewAct = Act#{
        fiat_value => folio_math:decimal_to_presentable_value(folio_math:to_decimal(FiatValue))
    },
    NewAct.
