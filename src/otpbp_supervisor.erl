-module(otpbp_supervisor).

-ifndef(HAVE_supervisor__get_childspec_2).
-export([get_childspec/2]).
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
        C:R -> C(R)
    end.
-endif.
