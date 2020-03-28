-module(otpbp_calendar).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
% OTP 21.0
-export([system_time_to_universal_time/2]).
-endif.
-ifndef(HAVE_calendar__system_time_to_local_time_2).
% OTP 21.0
-export([system_time_to_local_time/2]).
-ifdef(HAVE_calendar__system_time_to_universal_time_2).
-import(calendar, [system_time_to_universal_time/2]).
-endif.
-endif.

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970 * ?SECONDS_PER_DAY)).
system_time_to_universal_time(Time, Unit) ->
    calendar:gregorian_seconds_to_datetime(erlang:convert_time_unit(Time, Unit, seconds) + ?SECONDS_FROM_0_TO_1970).
-endif.

-ifndef(HAVE_calendar__system_time_to_local_time_2).
system_time_to_local_time(Time, Unit) -> erlang:universaltime_to_localtime(system_time_to_universal_time(Time, Unit)).
-endif.
