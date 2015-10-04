-module(otpbp_lists).

-ifndef(HAVE_lists__droplast_1).
-export([droplast/1]).
-endif.

-ifndef(HAVE_lists__droplast_1).
droplast([_])  -> [];
droplast([H|T]) -> [H|droplast(T)].
-endif.
