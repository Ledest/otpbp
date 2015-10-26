-module(otpbp_math).

-ifndef(HAVE_math__log2_1).
-export([log2/1]).
-endif.

-ifndef(HAVE_math__log2_1).
log2(X) -> math:log(X) / math:log(2).
-endif.
