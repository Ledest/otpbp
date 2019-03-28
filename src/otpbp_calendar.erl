-module(otpbp_calendar).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
-export([system_time_to_universal_time/2]).
-endif.
-ifndef(HAVE_calendar__system_time_to_local_time_2).
-export([system_time_to_local_time/2]).
-ifdef(HAVE_calendar__system_time_to_universal_time_2).
-import(calendar, [system_time_to_universal_time/2]).
-endif.
-endif.

-export([rfc3339_to_system_time/1, rfc3339_to_system_time/2]).
-export([system_time_to_rfc3339/1, system_time_to_rfc3339/2]).

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(DAYS_FROM_0_TO_10000, 2932897).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970 * ?SECONDS_PER_DAY)).
-define(SECONDS_FROM_0_TO_10000, (?DAYS_FROM_0_TO_10000 * ?SECONDS_PER_DAY)).

-ifndef(HAVE_calendar__system_time_to_universal_time_2).
system_time_to_universal_time(Time, Unit) ->
    calendar:gregorian_seconds_to_datetime(erlang:convert_time_unit(Time, Unit, seconds) + ?SECONDS_FROM_0_TO_1970).
-endif.

-ifndef(HAVE_calendar__system_time_to_local_time_2).
system_time_to_local_time(Time, Unit) -> erlang:universaltime_to_localtime(system_time_to_universal_time(Time, Unit)).
-endif.

-type rfc3339_string() :: [byte(), ...].
-type rfc3339_time_unit() :: microsecond | millisecond | nanosecond | second.


-spec rfc3339_to_system_time(DateTimeString::rfc3339_string()) -> integer().
rfc3339_to_system_time(DateTimeString) -> rfc3339_to_system_time(DateTimeString, []).

-spec rfc3339_to_system_time(DateTimeString::rfc3339_string(), Options::[{unit, rfc3339_time_unit()}]) -> integer().
%% _T is the character separating the date and the time:
rfc3339_to_system_time([Y1, Y2, Y3, Y4, $-, Mon1, Mon2, $-, D1, D2, _T, H1, H2, $:,
                        Min1, Min2, $:, S1, S2|TimeStr] = DateTimeString,
                       Options) ->
    {FractionStr, UtcOffset} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 orelse C =:= $. end, TimeStr),
    Time = calendar:datetime_to_gregorian_seconds({{list_to_integer([Y1, Y2, Y3, Y4]),
                                                    list_to_integer([Mon1, Mon2]),
                                                    list_to_integer([D1, D2])},
                                                   {list_to_integer([H1, H2]),
                                                    list_to_integer([Min1, Min2]),
                                                    list_to_integer([S1, S2])}}) - ?SECONDS_FROM_0_TO_1970,
    Secs = Time - offset_adjustment(Time, second, UtcOffset),
    check(DateTimeString, Options, Secs),
    case lists:keyfind(unit, 1, Options) of
        U when not U; U =:= {unit, second} -> Secs;
        {_, Unit} ->
            case erlang:convert_time_unit(Secs, second, Unit) of
                ScaledEpoch when FractionStr =:= "" -> ScaledEpoch;
                ScaledEpoch ->
                    Fraction = round(factor(Unit) * list_to_float([$0|FractionStr])),
                    if
                        ScaledEpoch < 0 -> ScaledEpoch - Fraction;
                        true -> ScaledEpoch + Fraction
                    end
            end
    end.

offset_adjustment(Time, Unit, "") ->
    %% Not optimized for special cases.
    UniversalTime = system_time_to_universal_time(Time, Unit),
    calendar:datetime_to_gregorian_seconds(erlang:universaltime_to_localtime(UniversalTime)) -
        calendar:datetime_to_gregorian_seconds(UniversalTime);
offset_adjustment(_Time, _Unit, [Z]) when Z =:= $Z; Z =:= $z -> 0;
offset_adjustment(_Time, _Unit, [Sign, H1, H2, $:, M1, M2]) ->
    Adjustment = 3600 * list_to_integer([H1, H2]) + 60 * list_to_integer([M1, M2]),
    if
        Sign =:= $- -> -Adjustment;
        Sign =:= $+ -> Adjustment
    end;
offset_adjustment(_Time, Unit, Offset) when is_integer(Offset) -> erlang:convert_time_unit(Offset, Unit, second).

check(_Arg, _Options, Secs) when Secs >= -?SECONDS_FROM_0_TO_1970, Secs < ?SECONDS_FROM_0_TO_10000 -> ok;
check(Arg, Options, _Secs) -> error({badarg, [Arg, Options]}).

factor(second) -> 1;
factor(millisecond) -> 1000;
factor(microsecond) -> 1000000;
factor(nanosecond) -> 1000000000.

-spec system_time_to_rfc3339(Time::integer()) -> rfc3339_string().
system_time_to_rfc3339(Time) -> system_time_to_rfc3339(Time, []).

-type offset() :: [byte()] | integer().
-spec system_time_to_rfc3339(Time::integer(),
                             Options::[{offset, offset()}|{time_designator, byte()}|{unit, rfc3339_time_unit()}]) ->
          rfc3339_string().
system_time_to_rfc3339(Time, Options) ->
    Unit = proplists:get_value(unit, Options, second),
    OffsetOption = proplists:get_value(offset, Options, ""),
    AdjustmentSecs = offset_adjustment(Time, Unit, OffsetOption),
    AdjustedTime = Time + erlang:convert_time_unit(AdjustmentSecs, second, Unit),
    Factor = factor(Unit),
    Secs = AdjustedTime div Factor,
    check(Time, Options, Secs),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Secs + ?SECONDS_FROM_0_TO_1970),
    lists:flatten([io_lib:fwrite("~4..0B-~2..0B-~2..0B~c~2..0B:~2..0B:~2..0B",
                                 [Year, Month, Day, proplists:get_value(time_designator, Options, $T), Hour, Min, Sec]),
                   if
                       Unit =:= second -> "";
                       true -> io_lib:fwrite(".~*..0B", [log10(Unit), abs(Time rem Factor)])
                   end,
                   offset(OffsetOption, AdjustmentSecs)]).

offset(OffsetOption, Secs0) when OffsetOption =:= ""; is_integer(OffsetOption) ->
    {Secs, Sign} = if
                       Secs0 < 0 -> {-Secs0, $-};
                       true -> {Secs0, $+}
                   end,
    io_lib:fwrite("~c~2..0B:~2..0B", [Sign, Secs div 3600, (Secs rem 3600) div 60]);
offset(OffsetOption, _Secs) -> OffsetOption.

log10(millisecond) -> 3;
log10(microsecond) -> 6;
log10(nanosecond) -> 9.
