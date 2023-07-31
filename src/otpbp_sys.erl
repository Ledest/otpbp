-module(otpbp_sys).

-ifndef(HAVE_sys__get_log_1).
% OTP >= 22.0
-export([get_log/1]).
-endif.

-ifndef(HAVE_sys__get_log_1).
get_log(Debug) -> [Event || {Event, _State, _FormFunc} <- nlog_get(sys:get_debug(log, Debug, nlog_new()))].

nlog_new() -> nlog_new(10).

nlog_new([_|_] = NLog) -> nlog_new(10, NLog);
nlog_new(N) -> [N]. % Empty log size N >= 1

nlog_new(N, NLog) -> lists:foldl(fun nlog_put/2, nlog_new(N), nlog_get(NLog)).

nlog_put(Item, [R|[_|F]]) when is_list(R) -> [[Item|R]|F];
nlog_put(Item, [R|[]]) when is_list(R) ->
    [_|F] = lists:reverse(R, [Item]),
    [[]|F];
nlog_put(Item, [1|R]) -> [[Item|R]];
nlog_put(Item, [J|R]) -> [J - 1,Item|R].

nlog_get([[]|F]) -> F;
nlog_get([[_|_] = R|F]) -> F ++ lists:reverse(R);
nlog_get([_J|R]) -> lists:reverse(R).
-endif.
