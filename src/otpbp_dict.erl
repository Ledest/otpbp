-module(otpbp_dict).

-ifndef(HAVE_dict__is_empty_1).
-export([is_empty/1]).
-endif.

-ifndef(HAVE_dict__is_empty_1).
is_empty(Dict) -> dict:size(dict) =:= 0.
-endif.
