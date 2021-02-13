-module(otpbp_gen).

-ifndef(HAVE_gen__name_1).
% OTP 19.0
-export([name/1]).
-endif.
-ifndef(HAVE_gen__unregister_name_1).
% OTP 19.0
-export([unregister_name/1]).
-endif.
-ifndef(HAVE_gen__get_proc_name_1).
% OTP 19.0
-export([get_proc_name/1]).
-endif.
-ifndef(HAVE_gen__get_parent_0).
% OTP 19.0
-export([get_parent/0]).
-endif.
-ifndef(HAVE_gen__debug_options_2).
% OTP 19.0
-export([debug_options/2]).
-endif.
-ifndef(HAVE_gen__debug_options_1).
% OTP < 19.0
-export([debug_options/1]).
-endif.

-ifndef(HAVE_gen__name_1).
name({T, Name}) when T =:= local; T =:= global -> Name;
name({via, _, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.
-endif.

-ifndef(HAVE_gen__unregister_name_1).
unregister_name({local, Name}) ->
    catch unregister(Name),
    ok;
unregister_name({global, Name}) ->
    global:unregister_name(Name),
    ok;
unregister_name({via, Mod, Name}) ->
    Mod:unregister_name(Name),
    ok;
unregister_name(Pid) when is_pid(Pid) -> ok.
-endif.

-ifndef(HAVE_gen__get_proc_name_1).
get_proc_name(Pid) when is_pid(Pid) -> Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
        {registered_name, Name} -> Name;
        {registered_name, _Name} -> exit(process_not_registered);
        [] -> exit(process_not_registered)
    end;
get_proc_name({global, Name}) ->
    global:whereis_name(Name) =:= self() orelse exit(process_not_registered_globally),
    Name;
get_proc_name({via, Mod, Name}) ->
    Mod:whereis_name(Name) =:= self() orelse exit({process_not_registered_via, Mod}),
    Name.
-endif.

-ifndef(HAVE_gen__get_parent_0).
get_parent() ->
    case get('$ancestors') of
        [Parent|_] when is_pid(Parent) -> Parent;
        [Parent|_] when is_atom(Parent) -> case whereis(Parent) of
                                               undefined -> case global:whereis_name(Parent) of
                                                                undefined -> exit(could_not_find_registered_name);
                                                                Pid -> Pid
                                                            end;
                                               Pid -> Pid
                                           end;
        _ -> exit(process_was_not_started_by_proc_lib)
    end.
-endif.

-ifndef(HAVE_gen__debug_options_2).
debug_options(Name, Opts) ->
    case lists:keyfind(debug, 1, Opts) of
        {_, Options} -> try
                            sys:debug_options(Options)
                        catch
                            _:_ ->
                                error_logger:format("~p: ignoring erroneous debug options - ~p~n", [Name, Options]),
                                []
                        end;
        false -> []
    end.
-endif.

-ifndef(HAVE_gen__debug_options_1).
debug_options(Opts) ->
    case lists:keyfind(debug, 1, Opts) of
        {_, Options} -> sys:debug_options(Options);
        false -> []
    end.
-endif.
