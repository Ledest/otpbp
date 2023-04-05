-module(otpbp_gen_event).

-ifndef(HAVE_gen_event__start_2).
% OTP 20.0
-export([start/2]).
-endif.
-ifndef(HAVE_gen_event__start_link_2).
% OTP 20.0
-export([start_link/2]).
-endif.

-define(NO_CALLBACK, 'no callback module').

-ifndef(HAVE_gen_event__start_2).
start(Name, Options) -> gen:start(gen_event, nolink, Name, ?NO_CALLBACK, [], Options).
-endif.

-ifndef(HAVE_gen_event__start_link_2).
start_link(Name, Options) -> gen:start(gen_event, link, Name, ?NO_CALLBACK, [], Options).
-endif.

