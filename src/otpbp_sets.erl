-module(otpbp_sets).

-ifndef(HAVE_sets__is_empty_1).
-export([is_empty/1]).
-endif.

-ifndef(HAVE_sets__is_empty_1).
is_empty(S) -> sets:size(S) =:= 0.
-endif.
