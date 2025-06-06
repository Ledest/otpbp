-module(otpbp_scheduler).

-ifndef(HAVE_scheduler__get_sample_0).
% OTP 24.3
-export([get_sample/0]).
-endif.
-ifndef(HAVE_scheduler__get_sample_all_0).
% OTP 24.3
-export([get_sample_all/0]).
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

-ifdef(NEED_get_sample_1).
get_sample(Stats) ->
    case statistics(Stats) of
        undefined -> undefined;
        List -> create_sample(Stats, List)
    end.

-compile({inline, create_sample/2}).
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
