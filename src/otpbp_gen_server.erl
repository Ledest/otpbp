-module(otpbp_gen_server).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_gen_server__start_monitor_3).
% OTP 23.0
-export([start_monitor/3]).
-endif.
-ifndef(HAVE_gen_server__start_monitor_4).
% OTP 23.0
-export([start_monitor/4]).
-endif.

-ifndef(HAVE_gen_server__start_monitor_4).
start_monitor(Name, Module, Args, Options) ->
    case where(Name) of
        undefined ->
            monitor_return(proc_lib:start_monitor(gen, init_it,
                                                  [gen_server, self(), self(), Name, Module, Args, Options],
                                                  timeout(Options), spawn_opts(Options)));
        Pid -> {error, {already_started, Pid}}
    end.

-compile({inline, where/1}).
where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).

-ifndef(NEED_monitor_return_1).
-define(NEED_monitor_return_1, true).
-endif.
-ifndef(NEED_timeout_1).
-define(NEED_timeout_1, true).
-endif.
-ifndef(NEED_spawn_opts_1).
-define(NEED_spawn_opts_1, true).
-endif.
-ifndef(NEED_timeout_1).
-define(NEED_timeout_1, true).
-endif.
-endif.

-ifndef(HAVE_gen_server__start_monitor_3).
start_monitor(Module, Args, Options) ->
    monitor_return(proc_lib:start_monitor(gen, init_it, [gen_server, self(), self(), Module, Args, Options],
                                          timeout(Options), spawn_opts(Options))).

-ifndef(NEED_monitor_return_1).
-define(NEED_monitor_return_1, true).
-endif.
-ifndef(NEED_timeout_1).
-define(NEED_timeout_1, true).
-endif.
-ifndef(NEED_spawn_opts_1).
-define(NEED_spawn_opts_1, true).
-endif.
-ifndef(NEED_timeout_1).
-define(NEED_timeout_1, true).
-endif.
-endif.

-ifdef(NEED_monitor_return_1).
monitor_return({{ok, Pid}, Mon}) when is_pid(Pid), is_reference(Mon) -> {ok, {Pid, Mon}};
monitor_return({Error, Mon}) when is_reference(Mon) ->
    receive
        {'DOWN', Mon, process, _Pid, _Reason} -> ok
    end,
    Error.
-endif.

-ifdef(NEED_timeout_1).
timeout(Options) ->
    case lists:keyfind(timeout, 1, Options) of
        {_, Time} -> Time;
        false -> infinity
    end.
-endif.

-ifdef(NEED_spawn_opts_1).
spawn_opts(Options) ->
    case lists:keyfind(spawn_opt, 1, Options) of
        {_, Opts} -> Opts;
        false -> []
    end.
-endif.
