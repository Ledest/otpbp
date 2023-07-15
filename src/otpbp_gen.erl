-module(otpbp_gen).

-ifndef(HAVE_gen__debug_options_1).
% OTP < 19.0
-export([debug_options/1]).
-endif.

-ifndef(HAVE_gen__debug_options_1).
debug_options(Opts) ->
    case lists:keyfind(debug, 1, Opts) of
        {_, Options} -> sys:debug_options(Options);
        false -> []
    end.
-endif.
