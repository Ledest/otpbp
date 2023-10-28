-module(otpbp_scheduler).

-ifndef(HAVE_scheduler__sample_0).
% OTP 21.0
-export([sample/0]).
-endif.
-ifndef(HAVE_scheduler__sample_all_0).
% OTP 21.0
-export([sample_all/0]).
-endif.
-ifndef(HAVE_scheduler__utilization_1).
% OTP 21.0
-export([utilization/1]).
-endif.
-ifndef(HAVE_scheduler__utilization_2).
% OTP 21.0
-export([utilization/2]).
-endif.
-ifndef(HAVE_scheduler__get_sample_0).
% OTP 24.3
-export([get_sample/0]).
-endif.
-ifndef(HAVE_scheduler__get_sample_all_0).
% OTP 24.3
-export([get_sample_all/0]).
-endif.

-ifndef(HAVE_scheduler__utilization_1).
-ifdef(HAVE_scheduler__utilization_2).
-import(scheduler, [utilization/2]).
-endif.
-endif.

-ifndef(HAVE_scheduler__sample_0).
sample() -> sample(scheduler_wall_time).

-ifndef(NEED_sample_1).
-define(NEED_sample_1, true).
-endif.
-endif.

-ifndef(HAVE_scheduler__sample_all_0).
sample_all() -> sample(scheduler_wall_time_all).

-ifndef(NEED_sample_1).
-define(NEED_sample_1, true).
-endif.
-endif.

-ifndef(HAVE_scheduler__utilization_1).
utilization(Seconds) when is_integer(Seconds), Seconds > 0 ->
    erlang:system_flag(scheduler_wall_time, true),
    T0 = sample(),
    receive
    after Seconds * 1000 -> ok
    end,
    T1 = sample(),
    erlang:system_flag(scheduler_wall_time, false),
    utilization(T0, T1);
utilization({Stats, _} = T0) when Stats =:= scheduler_wall_time; Stats =:= scheduler_wall_time_all ->
    utilization(T0, sample(Stats)).

-ifndef(NEED_sample_1).
-define(NEED_sample_1, true).
-endif.
-endif.

-ifndef(HAVE_scheduler__utilization_2).
utilization({Stats, Ts0}, {Stats, Ts1}) ->
    {Lst0, {A, T, N}} = lists:foldl(fun({Tag, I, Adiff, Tdiff}, {Lst, Acc}) ->
                                        R = safe_div(Adiff, Tdiff),
                                        {[{Tag, I, R, percent(R)}|Lst], acc(Tag, Adiff, Tdiff, Acc)}
                                    end,
                                    {[], {0, 0, 0}},
                                    lists:map(fun({{Tag, I, A0, T0}, {Tag, I, A1, T1}}) ->
                                                  {Tag, I, (A1 - A0), (T1 - T0)}
                                              end,
                                              lists:zip(Ts0,Ts1))),

    Total = safe_div(A, T),
    Lst1 = lists:reverse(Lst0),
    [{total, Total, percent(Total)}|case erlang:system_info(logical_processors_available) of
                                        unknown -> Lst1;
                                        LPA ->
                                            Weighted = Total * (N / LPA),
                                            [{weighted, Weighted, percent(Weighted)}|Lst1]
                                    end];
utilization({scheduler_wall_time, _} = T0, {scheduler_wall_time_all, Ts1}) -> utilization(T0, remove_io(Ts1));
utilization({scheduler_wall_time_all, Ts0}, {scheduler_wall_time, _} = T1) -> utilization(remove_io(Ts0), T1).

safe_div(_, B) when B == 0.0 -> 0.0;
safe_div(A, B) -> A / B.

percent(F) -> float_to_list(F * 100, [{decimals, 1}]) ++ [$%].

acc(io, _, _, Acc) -> Acc;
acc(Tag, Adiff, Tdiff, {Asum, Tsum, N}) when Tag =:= normal; Tag =:= cpu -> {Adiff + Asum, Tdiff + Tsum, N + 1}.

remove_io(Ts) ->
    {scheduler_wall_time,
     lists:filter(fun({io, _, _, _}) -> false;
                     (_) -> true
                  end,
                  Ts)}.
-endif.

-ifndef(HAVE_scheduler__get_sample_0).
get_sample() -> get_sample(scheduler_wall_time).

-ifndef(NEED_get_sample_1).
-define(NEED_get_sample_1, true).
-endif.
-endif.

-ifndef(HAVE_scheduler__get_sample_all_0).
get_sample_all() -> get_sample(scheduler_wall_time_all).

-ifndef(NEED_get_sample_1).
-define(NEED_get_sample_1, true).
-endif.
-endif.

-ifdef(NEED_sample_1).
sample(Stats) ->
    case statistics(Stats) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            sample(Stats);
        List -> create_sample(Stats, List)
    end.

-ifndef(NEED_create_sample_2).
-define(NEED_create_sample_2, true).
-endif.
-endif.

-ifdef(NEED_get_sample_1).
get_sample(Stats) ->
    case statistics(Stats) of
        undefined -> undefined;
        List -> create_sample(Stats, List)
    end.

-ifndef(NEED_create_sample_2).
-define(NEED_create_sample_2, true).
-endif.
-endif.

-ifdef(NEED_create_sample_2).
create_sample(Stats, List) ->
    Normal = erlang:system_info(schedulers),
    Cpu = erlang:system_info(dirty_cpu_schedulers) + Normal,
    {Stats,
     lists:map(fun({I, A, T}) ->
                   {if
                        Normal >= I -> normal;
                        Cpu >= I -> cpu;
                        true -> io
                    end,
                    I, A, T}
               end,
               lists:sort(List))}.
-endif.
