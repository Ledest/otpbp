-module(otpbp_gen_event).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_gen_event__start_2).
% OTP 20.0
-export([start/2]).
-endif.
-ifndef(HAVE_gen_event__start_link_2).
% OTP 20.0
-export([start_link/2]).
-endif.
-ifndef(HAVE_gen_event__start_monitor_0).
% OTP 23.0
-export([start_monitor/0]).
-endif.
-ifndef(HAVE_gen_event__start_monitor_1).
% OTP 23.0
-export([start_monitor/1]).
-endif.
-ifndef(HAVE_gen_event__start_monitor_2).
% OTP 23.0
-export([start_monitor/2]).
-endif.


-ifndef(HAVE_gen_event__start_monitor_0).
-ifdef(HAVE_gen_event__start_monitor_1).
-import(gen_event, [start_monitor/1]).
-endif.
-endif.
-ifndef(HAVE_gen_event__start_monitor_1).
-ifdef(HAVE_gen_event__start_monitor_2).
-import(gen_event, [start_monitor/2]).
-endif.
-endif.

-define(NO_CALLBACK, 'no callback module').

-ifndef(HAVE_gen_event__start_2).
start(Name, Options) -> gen:start(gen_event, nolink, Name, ?NO_CALLBACK, [], Options).
-endif.

-ifndef(HAVE_gen_event__start_link_2).
start_link(Name, Options) -> gen:start(gen_event, link, Name, ?NO_CALLBACK, [], Options).
-endif.

-ifndef(HAVE_gen_event__start_monitor_2).
start_monitor(Name, Options) ->
    case where(Name) of
        undefined ->
            monitor_return(proc_lib:start_monitor(gen, init_it,
                                                  [gen_event, self(), self(), Name, ?NO_CALLBACK, [], Options],
                                                  timeout(Options), spawn_opts(Options)));
        Pid -> {error, {already_started, Pid}}
    end.

-compile({inline, [where/1]}).
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

-ifndef(HAVE_gen_event__start_monitor_1).
start_monitor(Name) when is_tuple(Name) -> start_monitor(Name, []);
start_monitor(Options) when is_list(Options) ->
    monitor_return(proc_lib:start_monitor(gen, init_it, [gen_event, self(), self(), ?NO_CALLBACK, [], Options],
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

-ifndef(HAVE_gen_event__start_monitor_0).
start_monitor() -> start_monitor([]).
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
