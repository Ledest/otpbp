-module(otpbp_ets).

-ifndef(HAVE_ets__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_ets__take_2).
take(Tab, Key) ->
    Objects = ets:lookup(Tab, Key),
    Objects =:= [] orelse ets:delete(Tab, Key),
    Objects.
-endif.
