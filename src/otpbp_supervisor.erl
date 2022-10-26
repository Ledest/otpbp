-module(otpbp_supervisor).

-ifndef(HAVE_supervisor__format_status_2).
% OTP 19.0
-export([format_status/2]).
-endif.
-ifndef(HAVE_supervisor__check_childspecs_2).
% OTP 24.0
-export([check_childspecs/2]).
-endif.

-ifndef(HAVE_supervisor__format_status_2).
-record(state, {name,
                strategy,
                children = [],
                dynamics,
                intensity :: non_neg_integer(),
                period :: pos_integer(),
                restarts = [],
                module,
                args}).

format_status(terminate, [_PDict, State]) -> State;
format_status(_, [_PDict, #state{module = Module} = State]) ->
    [{data, [{"State", State}]}, {supervisor, [{"Callback", Module}]}].
-endif.

-ifndef(HAVE_supervisor__check_childspecs_2).
check_childspecs(ChildSpecs, undefined) -> supervisor:check_childspecs(ChildSpecs);
check_childspecs(_ChildSpecs, AutoShutdown) -> {error, {badarg, AutoShutdown}}.
-endif.
