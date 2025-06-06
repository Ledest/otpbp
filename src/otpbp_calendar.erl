-module(otpbp_calendar).

-ifndef(HAVE_calendar__local_time_to_system_time_1).
% OTP 28.0
-export([local_time_to_system_time/1]).
-endif.
-ifndef(HAVE_calendar__local_time_to_system_time_2).
% OTP 28.0
-export([local_time_to_system_time/2]).
-endif.
-ifndef(HAVE_calendar__universal_time_to_system_time_1).
% OTP 28.0
-export([universal_time_to_system_time/1]).
-endif.
-ifndef(HAVE_calendar__universal_time_to_system_time_2).
% OTP 28.0
-export([universal_time_to_system_time/2]).
-endif.

-ifndef(HAVE_calendar__local_time_to_system_time_1).
-ifdef(HAVE_calendar__universal_time_to_system_time_1).
-import(calendar, [universal_time_to_system_time/1]).
-endif.
-endif.
-ifndef(HAVE_calendar__local_time_to_system_time_2).
-ifdef(HAVE_calendar__universal_time_to_system_time_2).
-import(calendar, [universal_time_to_system_time/2]).
-endif.
-endif.
-ifndef(HAVE_calendar__universal_time_to_system_time_2).
-ifdef(HAVE_calendar__universal_time_to_system_time_1).
-import(calendar, [universal_time_to_system_time/1]).
-endif.
-endif.

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970 * ?SECONDS_PER_DAY)).
-define(DAYS_FROM_0_TO_10000, 2932897).
-define(SECONDS_FROM_0_TO_10000, (?DAYS_FROM_0_TO_10000 * ?SECONDS_PER_DAY)).

-ifndef(HAVE_calendar__local_time_to_system_time_1).
local_time_to_system_time(LocalTime) ->
    case calendar:local_time_to_universal_time_dst(LocalTime) of
        [UniversalTime] -> universal_time_to_system_time(UniversalTime);
        [] -> error({non_existing_local_time, LocalTime});
        [_, _] -> error({ambiguous_local_time, LocalTime})
    end.
-endif.

-ifndef(HAVE_calendar__local_time_to_system_time_2).
local_time_to_system_time(LocalTime, Options) ->
    case calendar:local_time_to_universal_time_dst(LocalTime) of
        [UniversalTime] -> universal_time_to_system_time(UniversalTime, Options);
        [] -> error({non_existing_local_time, LocalTime});
        [_, _] -> error({ambiguous_local_time, LocalTime})
    end.
-endif.

-ifndef(HAVE_calendar__universal_time_to_system_time_1).
universal_time_to_system_time(DateTime) -> calendar:datetime_to_gregorian_seconds(DateTime) - ?SECONDS_FROM_0_TO_1970.
-endif.

-ifndef(HAVE_calendar__universal_time_to_system_time_2).
universal_time_to_system_time(DateTime, Options) ->
    universal_time_to_system_time(DateTime) *
        erlang:convert_time_unit(1, second, proplists:get_value(unit, Options, second)).
-endif.
