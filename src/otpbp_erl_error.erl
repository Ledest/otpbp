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
-import(erl_error, [format_exception/4]).
-endif.
-endif.


-ifndef(HAVE_erl_error__format_exception_3).
format_exception(Class, Reason, StackTrace) -> format_exception(Class, Reason, StackTrace, #{}).
-endif.

-ifndef(HAVE_erl_error__format_exception_4).
format_exception(Class, Reason, StackTrace, Options) ->
    erl_error:format_exception(maps:get(column, Options, 1),
                               Class, Reason, StackTrace,
                               maps:get(stack_trim_fun, Options, fun default_stack_trim/3),
                               maps:get(format_fun, Options, fun default_format/2),
                               unicode).

default_stack_trim(_, _, _) -> false.

default_format(Term, I) -> io_lib:print(Term, I, 80, 30).
-endif.
