-module(otpbp_calendar).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
% OTP 21.0
-export([system_time_to_universal_time/2]).
-endif.
-ifndef(HAVE_calendar__system_time_to_local_time_2).
% OTP 21.0
-export([system_time_to_local_time/2]).
-endif.
-ifndef(HAVE_calendar__system_time_to_rfc3339_1).
% OTP 21.0
-export([system_time_to_rfc3339/1]).
-endif.
-ifndef(HAVE_calendar__system_time_to_rfc3339_2).
% OTP 21.0
-export([system_time_to_rfc3339/2]).
-endif.
-ifndef(HAVE_calendar__rfc3339_to_system_time_1).
% OTP 21.0
-export([rfc3339_to_system_time/1]).
-endif.
-ifndef(HAVE_calendar__rfc3339_to_system_time_2).
% OTP 21.0
-export([rfc3339_to_system_time/2]).
-endif.

-ifndef(HAVE_calendar__system_time_to_local_time_2).
-ifdef(HAVE_calendar__system_time_to_universal_time_2).
-import(calendar, [system_time_to_universal_time/2]).
-endif.
-endif.

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970 * ?SECONDS_PER_DAY)).
-define(DAYS_FROM_0_TO_10000, 2932897).
-define(SECONDS_FROM_0_TO_10000, (?DAYS_FROM_0_TO_10000 * ?SECONDS_PER_DAY)).

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
system_time_to_universal_time(Time, Unit) ->
    calendar:gregorian_seconds_to_datetime(erlang:convert_time_unit(Time, Unit, second) + ?SECONDS_FROM_0_TO_1970).
-endif.

-ifndef(HAVE_calendar__system_time_to_local_time_2).
system_time_to_local_time(Time, Unit) -> erlang:universaltime_to_localtime(system_time_to_universal_time(Time, Unit)).
-endif.

-ifndef(HAVE_calendar__system_time_to_rfc3339_1).
-ifndef(HAVE_calendar__system_time_to_rfc3339_2).
system_time_to_rfc3339(Time) -> calendar:system_time_to_rfc3339(Time, []).
-else.
system_time_to_rfc3339(Time) ->
    AdjustmentSecs = local_offset(Time, second),
    case Time + AdjustmentSecs of
        Secs when Secs >= -?SECONDS_FROM_0_TO_1970, Secs < ?SECONDS_FROM_0_TO_10000 ->
            {{Year, Month, Day}, {Hour, Min, Sec}} = system_time_to_datetime(Secs),
            lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s",
                                        [Year, Month, Day, Hour, Min, Sec, offset(AdjustmentSecs)]));
        _ -> error({badarg, [Time]})
    end.

-ifndef(NEED_local_offset_2).
-define(NEED_local_offset_2, true).
-endif.
-ifndef(NEED_system_time_to_datetime_1).
-define(NEED_system_time_to_datetime_1, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_calendar__system_time_to_rfc3339_2).
system_time_to_rfc3339(Time, Options) ->
    case proplists:get_value(unit, Options, second) of
        native ->
            system_time_to_rfc3339(erlang:convert_time_unit(Time, native, millisecond), Options, millisecond,
                                   case proplists:get_value(offset, Options, "") of
                                       OffsetOpt when is_integer(OffsetOpt) ->
                                           erlang:convert_time_unit(OffsetOpt, native, millisecond);
                                       OffsetOpt -> OffsetOpt
                                   end);
        Unit -> system_time_to_rfc3339(Time, Options, Unit, proplists:get_value(offset, Options, ""))
    end.

system_time_to_rfc3339(Time, Options, Unit, OffsetOption) ->
    AdjustmentSecs = offset_adjustment(Time, Unit, OffsetOption),
    AdjustedTime = Time + erlang:convert_time_unit(AdjustmentSecs, second, Unit),
    Factor = factor(Unit),
    case AdjustedTime div Factor of
        Secs when Secs >= -?SECONDS_FROM_0_TO_1970, Secs < ?SECONDS_FROM_0_TO_10000 ->
            {{Year, Month, Day}, {Hour, Min, Sec}} = system_time_to_datetime(Secs),
            L = io_lib:format("~4..0B-~2..0B-~2..0B~c~2..0B:~2..0B:~2..0B~s~s",
                              [Year, Month, Day, proplists:get_value(time_designator, Options, $T),
                               Hour, Min, Sec, fraction_str(Factor, AdjustedTime),
                               offset(OffsetOption, AdjustmentSecs)]),
            case proplists:get_value(return, Options, string) of
                string -> lists:flatten(L);
                binary -> list_to_binary(L)
            end;
        _ -> error({badarg, [Time, Options]})
    end.

-ifndef(NEED_factor_1).
-define(NEED_factor_1, true).
-endif.
-ifndef(NEED_system_time_to_datetime_1).
-define(NEED_system_time_to_datetime_1, true).
-endif.

offset_adjustment(Time, Unit, "") -> local_offset(Time, Unit);
offset_adjustment(_Time, _Unit, OffsetString) when is_list(OffsetString) ->
    offset_string_adjustment(OffsetString);
offset_adjustment(_Time, Unit, Offset) when is_integer(Offset) -> erlang:convert_time_unit(Offset, Unit, second).

-ifndef(NEED_local_offset_2).
-define(NEED_local_offset_2, true).
-endif.

offset(OffsetOption, Secs) when OffsetOption =:= ""; is_integer(OffsetOption) -> offset(Secs);
offset(OffsetOption, _Secs) -> OffsetOption.

offset(Secs) when Secs < 0 -> offset_(-Secs, $-);
offset(Secs) -> offset_(Secs, $+).

offset_(Secs, Sign) -> io_lib:format("~c~2..0B:~2..0B", [Sign, Secs div 3600, (Secs rem 3600) div 60]).

-ifndef(NEED_offset_string_adjustment_1).
-define(NEED_offset_string_adjustment_1, true).
-endif.

fraction_str(1, _Time) -> "";
fraction_str(Factor, Time) -> io_lib:format(".~*..0B", [byte_size(integer_to_binary(Factor)) - 1, abs(Time rem Factor)]).
-endif.

-ifndef(HAVE_calendar__rfc3339_to_system_time_1).
-ifdef(HAVE_calendar__rfc3339_to_system_time_2).
rfc3339_to_system_time(DateTimeString) -> calendar:rfc3339_to_system_time(DateTimeString, []).
-else.
rfc3339_to_system_time(DateTimeString) ->
    {Time, TimeStr} = datetime_str(DateTimeString),
    case Time - offset_string_adjustment(lists:dropwhile(fun is_fraction_char/1, TimeStr)) of
        Secs when Secs >= -?SECONDS_FROM_0_TO_1970, Secs < ?SECONDS_FROM_0_TO_10000 -> Secs;
        _ -> error({badarg, [Time]})
    end.

-ifndef(NEED_datetime_str_1).
-define(NEED_datetime_str_1, true).
-endif.
-ifndef(NEED_offset_string_adjustment_1).
-define(NEED_offset_string_adjustment_1, true).
-endif.
-ifndef(NEED_is_fraction_char_1).
-define(NEED_is_fraction_char_1, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_calendar__rfc3339_to_system_time_2).
rfc3339_to_system_time(DateTimeString, Options) ->
    {Time, TimeStr} = datetime_str(DateTimeString),
    {FractionStr, UtcOffset} = lists:splitwith(fun is_fraction_char/1, TimeStr),
    case Time - offset_string_adjustment(UtcOffset) of
        Secs when Secs >= -?SECONDS_FROM_0_TO_1970, Secs < ?SECONDS_FROM_0_TO_10000 ->
            Unit = proplists:get_value(unit, Options, second),
            ScaledEpoch = erlang:convert_time_unit(Secs, second, Unit),
            ScaledEpoch + copy_sign(fraction(Unit, FractionStr), ScaledEpoch);
        _ -> error({badarg, [DateTimeString, Options]})
    end.

-ifndef(NEED_datetime_str_1).
-define(NEED_datetime_str_1, true).
-endif.
-ifndef(NEED_offset_string_adjustment_1).
-define(NEED_offset_string_adjustment_1, true).
-endif.
-ifndef(NEED_is_fraction_char_1).
-define(NEED_is_fraction_char_1, true).
-endif.

copy_sign(N1, N2) when N2 < 0 -> -N1;
copy_sign(N1, _N2) -> N1.

fraction(Unit, FractionStr) when Unit =:= second; FractionStr =:= "" -> 0;
fraction(Unit, FractionStr) -> round(factor(Unit) * list_to_float([$0|FractionStr])).

-ifndef(NEED_factor_1).
-define(NEED_factor_1, true).
-endif.
-endif.

-ifdef(NEED_local_offset_2).
local_offset(SystemTime, Unit) ->
    UniversalTime = calendar:system_time_to_universal_time(SystemTime, Unit),
    calendar:datetime_to_gregorian_seconds(erlang:universaltime_to_localtime(UniversalTime)) -
        calendar:datetime_to_gregorian_seconds(UniversalTime).
-endif.
-ifdef(NEED_factor_1).
factor(Unit) -> erlang:convert_time_unit(1, second, Unit).
-endif.
-ifdef(NEED_offset_string_adjustment_1).
offset_string_adjustment(TZ) when TZ =:= "Z"; TZ =:= "z" -> 0;
offset_string_adjustment([Sign, H1, H2]) -> offset_string_adjustment(Sign, H1, H2, 0);
offset_string_adjustment([Sign, H1, H2, $:, M1, M2]) ->
    offset_string_adjustment(Sign, H1, H2, list_to_integer([M1, M2]) * 60);
offset_string_adjustment(TZ) -> error({badmatch, TZ}).

offset_string_adjustment(Sign, H1, H2, Min) -> offset_string_adjustment(Sign, 3600 * list_to_integer([H1, H2]) + Min).

offset_string_adjustment($-, Adjustment) -> -Adjustment;
offset_string_adjustment($+, Adjustment) -> Adjustment.
-endif.
-ifdef(NEED_datetime_str_1).
datetime_str([Y1, Y2, Y3, Y4, $-, Mon1, Mon2, $-, D1, D2, _T, H1, H2, $:, Min1, Min2, $:, S1, S2|TimeStr]) ->
    {calendar:datetime_to_gregorian_seconds({{list_to_integer([Y1, Y2, Y3, Y4]),
                                              list_to_integer([Mon1, Mon2]),
                                              list_to_integer([D1, D2])},
                                             {list_to_integer([H1, H2]),
                                              list_to_integer([Min1, Min2]),
                                              list_to_integer([S1, S2])}}) - ?SECONDS_FROM_0_TO_1970,
     TimeStr}.
-endif.
-ifdef(NEED_is_fraction_char_1).
is_fraction_char(C) -> C =:= $. orelse C >= $0 andalso C =< $9.
-endif.
-ifdef(NEED_system_time_to_datetime_1).
system_time_to_datetime(Seconds) -> calendar:gregorian_seconds_to_datetime(Seconds + ?SECONDS_FROM_0_TO_1970).
-endif.
