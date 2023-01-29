-module(otpbp_timer).

-ifndef(HAVE_timer__tc_4).
% OTP 25.3
-export([tc/4]).
-endif.

-ifndef(HAVE_timer__tc_4).
tc(M, F, A, TU) ->
    T = erlang:monotonic_time(),
    V = apply(M, F, A),
    {erlang:convert_time_unit(erlang:monotonic_time() - T, native, TU), V}.
-endif.
