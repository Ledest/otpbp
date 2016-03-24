-module(otpbp_ets).

-ifndef(HAVE_ets__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_ets__take_2).
take(Tab, Key) ->
    R = ets:lookup(Tab, Key),
    ets:delete(Tab, Key),
    R.
-endif.
