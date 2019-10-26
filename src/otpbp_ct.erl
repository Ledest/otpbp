-module(otpbp_ct).

-ifndef(HAVE_ct__get_event_mgr_ref_0).
% OTP 17.5
-export([get_event_mgr_ref/0]).
-endif.

-ifndef(HAVE_ct__get_progname_0).
-export([get_progname/0]).
-endif.

-ifndef(HAVE_ct__get_event_mgr_ref_0).
get_event_mgr_ref() -> ct_event.
-endif.

-ifndef(HAVE_ct__get_progname_0).
get_progname() ->
    case init:get_argument(progname) of
        {ok, [[Prog]]} -> Prog;
        _ -> "no_prog_name"
    end.
-endif.
