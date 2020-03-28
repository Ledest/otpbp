-module(otpbp_ordsets).

-ifndef(HAVE_ordsets__is_empty_1).
% OTP 21.0
-export([is_empty/1]).
-endif.

-ifndef(HAVE_ordsets__is_empty_1).
is_empty(S) -> S =:= [].
-endif.
