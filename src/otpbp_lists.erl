-module(otpbp_lists).

-ifndef(HAVE_lists__droplast_1).
-export([droplast/1]).
-endif.

-ifndef(HAVE_lists__droplast_1).
droplast(L) -> lists:reverse(tl(lists:reverse(L))).
-endif.
