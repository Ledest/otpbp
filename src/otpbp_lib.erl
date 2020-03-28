-module(otpbp_lib).

-ifndef(HAVE_lib__nonl_1).
% OTP < 21.0
-export([nonl/1]).
-endif.
-ifndef(HAVE_lib__send_2).
% OTP < 21.0
-export([send/2]).
-endif.
-ifndef(HAVE_lib__sendw_2).
% OTP < 21.0
-export([sendw/2]).
-endif.
-ifndef(HAVE_lib__flush_receive_0).
% OTP < 21.0
-export([flush_receive/0]).
-endif.
-ifndef(HAVE_lib__error_message_2).
% OTP < 21.0
-export([error_message/2]).
-endif.
-ifndef(HAVE_lib__progname_0).
% OTP < 21.0
-export([progname/0]).
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
