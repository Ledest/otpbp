-module(erlang_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
-compile({nowarn_deprecated_function, {erlang, get_stacktrace, 0}}).
-endif.
-endif.

stacktrace_test() ->
    ?assertMatch({error, test, S1, S1}, stacktrace(error, test)),
    ?assertMatch({throw, test, S1, S1}, stacktrace(throw, test)),
    ?assertMatch({exit, test, S1, S1}, stacktrace(exit, test)).

stacktrace(C, R) ->
    try
        erlang:C(R)
    catch
        C:R:S -> {C, R, S, erlang:get_stacktrace()}
    end.
