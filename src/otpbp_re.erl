-module(otpbp_re).

-ifndef(HAVE_re__version_0).
% OTP >= 20.0
-export([version/0]).
-endif.

-ifndef(HAVE_re__version_0).
version() -> <<"8.33 2013-05-28">>.
-endif.
