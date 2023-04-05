-module(otpbp_c).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_c__lm_0).
% OTP 20.0
-export([lm/0]).
-endif.

-ifndef(HAVE_c__lm_0).
lm() -> lists:map(fun c:l/1, c:mm()).
-endif.
