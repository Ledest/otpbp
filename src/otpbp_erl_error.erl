-module(otpbp_erl_error).

-ifndef(HAVE_erl_error__format_exception_7).
-compile([{parse_transform, otpbp_pt}]).
-endif.

-ifndef(HAVE_erl_error__format_exception_3).
% OTP 24.0
-export([format_exception/3]).
-endif.
-ifndef(HAVE_erl_error__format_exception_4).
% OTP 24.0
-export([format_exception/4]).
-endif.

-ifndef(HAVE_erl_error__format_exception_3).
-ifdef(HAVE_erl_error__format_exception_4).
format_exception(Class, Reason, StackTrace) -> erl_error:format_exception(Class, Reason, StackTrace, #{}).
-else.
format_exception(Class, Reason, StackTrace) ->
    erl_error:format_exception(1, Class, Reason, StackTrace,
                               fun(_, _, _) -> false end,
                               fun(Term, I) -> io_lib:print(Term, I, 80, 30) end,
                               unicode).
-endif.
-endif.

-ifndef(HAVE_erl_error__format_exception_4).
format_exception(Class, Reason, StackTrace, Options) ->
    erl_error:format_exception(maps:get(column, Options, 1),
                               Class, Reason, StackTrace,
                               maps:get(stack_trim_fun, Options, fun(_, _, _) -> false end),
                               maps:get(format_fun, Options, fun(Term, I) -> io_lib:print(Term, I, 80, 30) end),
                               unicode).
-endif.
