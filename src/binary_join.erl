-module(binary_join).

-export([join/2]).

-spec join(Separator :: binary(), List :: [binary()]) -> binary().
join(_Separator, []) ->
    <<>>;
join(Separator, [H | T]) ->
    lists:foldl(fun(Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).
