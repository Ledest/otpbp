-module(otpbp_supervisor).

-ifndef(HAVE_supervisor__check_childspecs_2).
% OTP 24.0
-export([check_childspecs/2]).
-endif.
-ifndef(HAVE_supervisor__which_child_2).
% OTP 28.0
-export([which_child/2]).
-endif.

-ifndef(HAVE_supervisor__check_childspecs_2).
check_childspecs(ChildSpecs, undefined) -> supervisor:check_childspecs(ChildSpecs);
check_childspecs(_ChildSpecs, AutoShutdown) -> {error, {badarg, AutoShutdown}}.
-endif.

-ifndef(HAVE_supervisor__which_child_2).
which_child(SupRef, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(SupRef)) of
        {_Id, R} -> {ok, R};
        _false -> {error, not_found}
    end.
-endif.
