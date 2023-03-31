-module(folio_math).

-export([to_decimal/1, multiply/2, sum/1]).

-export([decimal_to_presentable_value/1]).

to_decimal(I) when is_number(I) ->
    decimal:to_decimal(I, #{precision => 100, rounding => round_floor});
to_decimal(F) when is_binary(F) ->
    L = size(F),
    decimal:to_decimal(F, #{precision => L, rounding => round_floor}).

multiply(ABin, BBin) when is_binary(ABin) and is_binary(BBin) ->
    A = to_decimal(ABin),
    B = to_decimal(BBin),
    C = decimal:mult(A, B),
    CBin = decimal:to_binary(C),
    CBin.

sum(FloatValues) when is_list(FloatValues) ->
    DecimalTotal = lists:foldl(
        fun(FVal, Total) ->
            Val = to_decimal(FVal),
            decimal:add(Val, Total)
        end,
        {0, 0},
        FloatValues
    ),
    DecimalTotal.

decimal_to_presentable_value(D) ->
    F = decimal:to_binary(D, #{pretty => false}),
    {DotPos, _} = binary:match(F, <<".">>),
    Length = lists:min([DotPos + 3, size(F)]),
    P = binary:part(F, {0, Length}),
    P.
