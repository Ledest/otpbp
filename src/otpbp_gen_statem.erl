-module(otpbp_gen_statem).

-ifndef(HAVE_gen_statem__enter_loop_4).
% OTP 19.1
-export([enter_loop/4]).
-endif.

-ifndef(HAVE_gen_statem__enter_loop_4).
enter_loop(Module, Opts, State, Data) -> gen_statem:enter_loop(Module, Opts, State, Data, self()).
-endif.
