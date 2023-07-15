-module(otpbp_supervisor).

-ifndef(HAVE_supervisor__check_childspecs_2).
% OTP 24.0
-export([check_childspecs/2]).
-endif.

-ifndef(HAVE_supervisor__check_childspecs_2).
check_childspecs(ChildSpecs, undefined) -> supervisor:check_childspecs(ChildSpecs);
check_childspecs(_ChildSpecs, AutoShutdown) -> {error, {badarg, AutoShutdown}}.
-endif.
