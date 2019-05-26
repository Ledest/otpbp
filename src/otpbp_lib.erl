-module(otpbp_lib).

-ifndef(HAVE_lib__nonl_1).
-export([nonl/1]).
-endif.
-ifndef(HAVE_lib__send_2).
-export([send/2]).
-endif.
-ifndef(HAVE_lib__sendw_2).
-export([sendw/2]).
-endif.
-ifndef(HAVE_lib__flush_receive_0).
-export([flush_receive/0]).
-endif.
-ifndef(HAVE_lib__error_message_2).
-export([error_message/2]).
-endif.
-ifndef(HAVE_lib__progname_0).
-export([progname/0]).
-endif.
-ifndef(HAVE_lib__format_call_5).
-export([format_call/5]).
-endif.
-ifndef(HAVE_lib__format_exception_7).
-export([format_exception/7]).
-endif.
-ifndef(HAVE_lib__format_stacktrace_5).
-export([format_stacktrace/5]).
-endif.

-ifndef(HAVE_lib__nonl_1).
nonl([$\n|T]) -> nonl(T);
nonl([H|T]) -> [H|nonl(T)];
nonl([]) -> [].
-endif.

-ifndef(HAVE_lib__send_2).
send(To, Msg) -> To ! Msg.
-endif.

-ifndef(HAVE_lib__sendw_2).
sendw(To, Msg) ->
    To ! {self(), Msg},
    receive
        Reply -> Reply
    end.
-endif.

-ifndef(HAVE_lib__flush_receive_0).
flush_receive() ->
    receive
        _ -> flush_receive()
    after 0 -> ok
    end.
-endif.

-ifndef(HAVE_lib__error_message_2).
error_message(Format, Args) -> io:format("** ~ts **\n", [io_lib:format(Format, Args)]).
-endif.

-ifndef(HAVE_lib__progname_0).
progname() ->
    case init:get_argument(progname) of
        {ok, [[Prog]]} -> list_to_atom(Prog);
        _ -> no_prog_name
    end.
-endif.

-ifndef(HAVE_lib__format_call_5).
-ifdef(HAVE_erl_error__format_call_5).
format_call(I, ForMForFun, As, FormatFun, Encoding) -> erl_error:format_call(I, ForMForFun, As, FormatFun, Encoding).
-else.
format_call(I, ForMForFun, As, FormatFun, latin1) -> lib:format_call(I, ForMForFun, As, FormatFun).
-endif.
-endif.

-ifndef(HAVE_lib__format_exception_7).
-ifdef(HAVE_erl_error__format_exception_7).
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, Encoding) ->
    erl_error:format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, Encoding).
-else.
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, latin1) ->
    lib:format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun).
-endif.
-endif.

-ifndef(HAVE_lib__format_stacktrace_5).
-ifdef(HAVE_erl_error__format_stacktrace_5).
format_stacktrace(I, StackTrace, StackFun, FormatFun, Encoding) ->
    erl_error:format_stacktrace(I, StackTrace, StackFun, FormatFun, Encoding).
-else.
format_stacktrace(I, StackTrace, StackFun, FormatFun, latin1) ->
    lib:format_stacktrace(I, StackTrace, StackFun, FormatFun).
-endif.
-endif.
