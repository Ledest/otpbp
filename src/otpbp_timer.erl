-module(otpbp_timer).

-ifndef(HAVE_timer__tc_4).
% OTP 26.0
-export([tc/4]).
-endif.
-ifndef(HAVE_timer__apply_after_2).
% OTP 27.0
-export([apply_after/2]).
-endif.
-ifndef(HAVE_timer__apply_after_3).
% OTP 27.0
-export([apply_after/3]).
-endif.
-ifndef(HAVE_timer__apply_interval_2).
% OTP 27.0
-export([apply_interval/2]).
-endif.
-ifndef(HAVE_timer__apply_interval_3).
% OTP 27.0
-export([apply_interval/3]).
-endif.

-ifndef(HAVE_timer__tc_4).
tc(M, F, A, TU) ->
    T = erlang:monotonic_time(),
    V = apply(M, F, A),
    {erlang:convert_time_unit(erlang:monotonic_time() - T, native, TU), V}.
-endif.

-ifndef(HAVE_timer__apply_after_2).
apply_after(Time, F) when is_function(F, 0) -> timer:apply_after(Time, erlang, apply, [F, []]);
apply_after(_Time, _F) -> {error, badarg}.
-endif.

-ifndef(HAVE_timer__apply_after_3).
apply_after(Time, F, A) when is_function(F, length(A)) -> timer:apply_after(Time, erlang, apply, [F, A]);
apply_after(_Time, _F, _A) -> {error, badarg}.
-endif.

-ifndef(HAVE_timer__apply_interval_2).
apply_interval(Time, F) when is_function(F, 0) -> timer:apply_interval(Time, erlang, apply, [F, []]);
apply_interval(_Time, _F) -> {error, badarg}.
-endif.

-ifndef(HAVE_timer__apply_interval_3).
apply_interval(Time, F, A) when is_function(F, length(A)) -> timer:apply_interval(Time, erlang, apply, [F, A]);
apply_interval(_Time, _F, _A) -> {error, badarg}.
-endif.
