-module(otpbp_proc_lib).

-ifndef(HAVE_proc_lib__start_monitor_3).
% OTP 23.0
-export([start_monitor/3]).
-endif.
-ifndef(HAVE_proc_lib__start_monitor_4).
% OTP 23.0
-export([start_monitor/4]).
-endif.
-ifndef(HAVE_proc_lib__start_monitor_5).
% OTP 23.0
-export([start_monitor/5]).
-endif.
-ifndef(HAVE_proc_lib__init_fail_2).
% OTP 26.0
-export([init_fail/2]).
-endif.
-ifndef(HAVE_proc_lib__init_fail_3).
% OTP 26.0
-export([init_fail/3]).
-endif.
-ifndef(HAVE_proc_lib__get_label_1).
% OTP 27.0
-export([get_label/1]).
-endif.
-ifndef(HAVE_proc_lib__set_label_1).
% OTP 27.0
-export([set_label/1]).
-endif.

-ifndef(HAVE_proc_lib__init_fail_2).
-ifdef(HAVE_proc_lib__init_fail_3).
-import(proc_lib, [init_fail/3]).
-endif.
-endif.

-ifndef(HAVE_proc_lib__start_monitor_3).
start_monitor(M, F, A) when is_atom(M), is_atom(F), is_list(A) -> start_monitor(M, F, A, infinity).
-endif.

-ifndef(HAVE_proc_lib__start_monitor_4).
start_monitor(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    sync_start_monitor(spawn_mon(M, F, A), Timeout).

-compile({inline, spawn_mon/3}).
spawn_mon(M, F, A) -> spawn_monitor(proc_lib, init_p, [get_my_name(), get_ancestors(), M, F, A]).

-compile({inline, get_my_name/0}).
get_my_name() ->
    case proc_info(self(), registered_name) of
        {registered_name, Name} -> Name;
        _ -> self()
    end.

-compile({inline, get_ancestors/0}).
get_ancestors() ->
    case get('$ancestors') of
        A when is_list(A) -> A;
        _ -> []
    end.

-ifndef(NEED_sync_start_monitor_2).
-define(NEED_sync_start_monitor_2, true).
-endif.
-ifndef(NEED_proc_info_2).
-define(NEED_proc_info_2, true).
-endif.
-endif.

-ifndef(HAVE_proc_lib__start_monitor_5).
start_monitor(M, F, A, Timeout, SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    lists:member(monitor, SpawnOpts) andalso error(badarg, [M, F, A, Timeout, SpawnOpts]),
    sync_start_monitor(proc_lib:spawn_opt(M, F, A, [monitor|SpawnOpts]), Timeout).

-ifndef(NEED_sync_start_monitor_2).
-define(NEED_sync_start_monitor_2, true).
-endif.
-endif.

-ifdef(NEED_sync_start_monitor_2).
sync_start_monitor({Pid, Ref}, Timeout) ->
    receive
        {ack, Pid, Return} -> {Return, Ref};
        {'DOWN', Ref, process, Pid, Reason} = Down ->
            self() ! Down,
            {{error, Reason}, Ref}
    after Timeout ->
            kill_flush(Pid),
            {{error, timeout}, Ref}
    end.

-compile({inline, [kill_flush/1]}).
kill_flush(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'EXIT', Pid, _} -> ok
    after 0 -> ok
    end.
-endif.

-ifndef(HAVE_proc_lib__init_fail_2).
init_fail(Return, Exception) ->
    [Parent|_] = get('$ancestors'),
    init_fail(Parent, Return, Exception).
-endif.

-ifndef(HAVE_proc_lib__init_fail_3).
init_fail(Parent, Return, Exception) ->
    Parent ! {nack, self(), Return},
    case Exception of
        {Class, Reason} when Class =:= error; Class =:= exit; Class =:= throw -> erlang:Class(Reason);
        {Class, Reason, Stacktrace} -> error(erlang:raise(Class, Reason, Stacktrace), [Parent, Return, Exception])
    end.
-endif.

-ifndef(HAVE_proc_lib__get_label_1).
get_label(Pid) when Pid =:= self() -> get('$process_label');
get_label(Pid) ->
    try get_process_info(Pid, {dictionary, '$process_label'}) of
        {process_label, Id} -> Id;
        _ -> undefined
    catch
        _:_ -> %% Old Node
            undefined
    end.

get_process_info(Pid, Tag) -> translate_process_info(Tag, catch proc_info(Pid, Tag)).

-compile({inline, translate_process_info/2}).
translate_process_info(registered_name, []) -> {registered_name, []};
translate_process_info(_ , {'EXIT', _}) -> undefined;
translate_process_info(_, Result) -> Result.

-ifndef(NEED_proc_info_2).
-define(NEED_proc_info_2, true).
-endif.
-endif.

-ifndef(HAVE_proc_lib__set_label_1).
set_label(Label) ->
    put('$process_label', Label),
    ok.
-endif.

-ifdef(NEED_proc_info_2).
proc_info(Pid, Item) when node(Pid) =:= node() -> process_info(Pid,Item);
proc_info(Pid, Item) ->
    case lists:member(node(Pid), nodes()) of
        true ->
            case rpc:call(node(Pid), erlang, process_info, [Pid, Item]) of
                {badrpc, nodedown} -> undefined;
                {badrpc, Error} -> Error;
                Res -> Res
            end;
        _ -> hidden
    end.
-endif.
