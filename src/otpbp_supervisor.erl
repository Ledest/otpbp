-module(otpbp_supervisor).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_supervisor__get_childspec_2).
-export([get_childspec/2]).
-endif.
-ifndef(HAVE_supervisor__try_again_restart_2).
-export([try_again_restart/2]).
-endif.
-ifndef(HAVE_supervisor__get_callback_module_1).
-ifndef(NEED_supervisor__state_record).
-define(NEED_supervisor__state_record, true).
-endif.
-export([get_callback_module/1]).
-endif.
-ifndef(HAVE_supervisor__format_status_2).
-ifndef(NEED_supervisor__state_record).
-define(NEED_supervisor__state_record, true).
-endif.
-export([format_status/2]).
-endif.

-ifndef(HAVE_supervisor__get_childspec_2).
get_childspec(SupRef, Id)
  when is_atom(SupRef) orelse is_pid(SupRef) orelse
       tuple_size(SupRef) =:= 2 andalso is_atom(element(1, SupRef)) andalso is_atom(element(2, SupRef)),
       not is_pid(Id) ->
    try sys:get_state(SupRef) of
        State when element(1, State) =:= state, tuple_size(State) >= 10, is_list(element(4, State)) ->
            try lists:foreach(fun({child, _PID, N, Start, Restart, Shutdown, Type, Modules}) when N =:= Id ->
                                  throw(maps:from_list([{id, Id}, {start, Start}, {restart, Restart},
                                                        {shutdown, Shutdown}, {type, Type}, {modules, Modules}]));
                                 (_) -> ok
                              end, element(4, State)) of
                ok -> {error, no_found}
            catch
                throw:CS -> {ok, CS}
            end
    catch
        exit:{noproc, {sys, get_state, _}} ->
            exit({noproc, {gen_server, call, [SupRef, {get_childspec, Id}, infinity]}});
        C:R -> erlang:C(R, [SupRef, Id])
    end.
-endif.

-ifndef(HAVE_supervisor__try_again_restart_2).
try_again_restart(Supervisor, Child) -> gen_server:cast(Supervisor, {try_again_restart, Child}).
-endif.

-ifdef(NEED_supervisor__state_record).
-record(state, {name,
                strategy,
                children = [],
                dynamics,
                intensity :: non_neg_integer(),
                period :: pos_integer(),
                restarts = [],
                module,
                args}).
-endif.

-ifndef(HAVE_supervisor__get_callback_module_1).
get_callback_module(Pid) ->
    {status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(Pid),
    case lists:keyfind(supervisor, 1, Misc) of
        {_, [{"Callback", Mod}]} -> Mod;
        _ ->
            [_Header, _Data, {data, [{"State", #state{module = Module}}]}|_] = Misc,
            Module
    end.
-endif.

-ifndef(HAVE_supervisor__format_status_2).
format_status(terminate, [_PDict, State]) -> State;
format_status(_, [_PDict, #state{module = Module} = State]) ->
    [{data, [{"State", State}]}, {supervisor, [{"Callback", Module}]}].
-endif.
