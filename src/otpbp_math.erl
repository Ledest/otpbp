-module(otpbp_math).

-ifndef(HAVE_math__tau_0).
% OTP 26.0
-export([tau/0]).
-endif.

-ifndef(HAVE_math__tau_0).
tau() -> math:pi() * 2.0.
-endif.
