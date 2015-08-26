-module(otpbp_os).

-ifndef(HAVE_os__system_time_1).
-export([system_time/1]).
-endif.
-ifndef(HAVE_os__getenv_2).
-export([getenv/2]).
-endif.

-ifndef(HAVE_os__system_time_1).
system_time(seconds) ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds;
system_time(micro_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000000 + Seconds * 1000000 + MicroSeconds;
system_time(milli_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000 + Seconds * 1000 + round(MicroSeconds / 1000);
system_time(nano_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000000 * 1000 + Seconds * 1000000 * 1000 + MicroSeconds * 1000.
-endif.

-ifndef(HAVE_os__getenv_2).
getenv(VarName, DefaultValue) ->
    case os:getenv(VarName) of
        false -> DefaultValue;
        Value -> Value
    end.
-endif.
