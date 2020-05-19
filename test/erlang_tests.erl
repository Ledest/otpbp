-module(erlang_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
-if(?OTP_RELEASE >= 23).
-define(OTP_RELEASE_23, true).
-else.
-compile({nowarn_deprecated_function, {erlang, get_stacktrace, 0}}).
-endif.
-endif.
-endif.

-ifndef(OTP_RELEASE_23).
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
-endif.
