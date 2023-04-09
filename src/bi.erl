-module(bi).

-export([gen_server_cast/2, timer_apply_interval/4]).

gen_server_cast(A, B) ->
    gen_server:cast(A, B).

timer_apply_interval(A, B, C, D) ->
    timer:apply_interval(A, B, C, D).
