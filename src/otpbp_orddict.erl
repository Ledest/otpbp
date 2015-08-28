-module(otpbp_orddict).

-ifndef(HAVE_orddict__is_empty_1).
-export([is_empty/1]).
-endif.

-ifndef(HAVE_orddict__is_empty_1).
is_empty(Dict) -> orddict:size(Dict) =:= 0.
-endif.
