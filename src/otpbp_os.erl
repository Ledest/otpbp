-module(otpbp_os).

-ifndef(HAVE_os__system_time_1).
-export([system_time/1]).
-endif.
-ifndef(HAVE_os__getenv_2).
-export([getenv/2]).
-endif.
-ifndef(HAVE_os__cmd_2).
-export([cmd/2]).
-endif.

-ifndef(HAVE_os__system_time_1).
system_time(seconds) ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds;
system_time(micro_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000000 + Seconds * 1000000 + MicroSeconds;
system_time(milli_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000 + Seconds * 1000 + round(MicroSeconds / 1000);
system_time(nano_seconds) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 * 1000000 * 1000 + Seconds * 1000000 * 1000 + MicroSeconds * 1000.
-endif.

-ifndef(HAVE_os__getenv_2).
getenv(VarName, DefaultValue) ->
    case os:getenv(VarName) of
        false -> DefaultValue;
        Value -> Value
    end.
-endif.

-ifndef(HAVE_os__cmd_2).
cmd(Cmd0, Opts) ->
    Cmd = if
             is_atom(Cmd0) -> atom_to_list(Cmd0);
             true ->
                 validate(Cmd0),
                 Cmd0
          end,
    {SpawnCmd, SpawnOpts, SpawnInput, Eot} = mk_cmd(Cmd),
    Port = open_port({spawn, SpawnCmd}, [binary, stderr_to_stdout, stream, in, hide|SpawnOpts]),
    MonRef = erlang:monitor(port, Port),
    true = port_command(Port, SpawnInput),
    Bytes = get_data(Port, MonRef, Eot, [], 0, maps:get(max_size, Opts, infinity)),
    demonitor(MonRef, [flush]),
    %% Convert to unicode list if possible otherwise return bytes
    case unicode:characters_to_list(Bytes) of
        String when is_list(String) -> String;
        _ -> binary_to_list(Bytes)
    end.

mk_cmd(Cmd) ->
    %% Have to send command in like this in order to make sh commands like
    %% cd and ulimit available
    {"/bin/sh -s unix:cmd", [out],
     %% We insert a new line after the command, in case the command
     %% contains a comment character.
     %%
     %% The </dev/null closes stdin, which means that programs
     %% that use a closed stdin as an termination indicator works.
     %% An example of such a program is 'more'.
     %%
     %% The "echo ^D" is used to indicate that the program has executed
     %% and we should return any output we have gotten. We cannot use
     %% termination of the child or closing of stdin/stdout as then
     %% starting background jobs from os:cmd will block os:cmd.
     %%
     %% I tried changing this to be "better", but got bombarded with
     %% backwards incompatibility bug reports, so leave this as it is.
     ["(", unicode:characters_to_binary(Cmd), "\n) </dev/null; echo \"\^D\"\n"],
     <<$\^D>>}.

validate([C|Rest]) when is_integer(C) -> validate(Rest);
validate([List|Rest]) when is_list(List) ->
    validate(List),
    validate(Rest);
validate([]) -> ok.

get_data(Port, MonRef, Eot, Sofar, Size, Max) ->
    receive
        {Port, {data, Bytes}} -> if
                                     Eot =:= <<>> ->
                                         get_data(Port, MonRef, Eot, [Sofar, Bytes], Size + byte_size(Bytes), Max);
                                     true ->
                                         catch port_close(Port),
                                         flush_until_down(Port, MonRef),
                                         iolist_to_binary([Sofar, eot(Bytes, Eot, Size, Max)])
                                 end;
        {'DOWN', MonRef, _, _, _} ->
            flush_exit(Port),
            iolist_to_binary(Sofar)
    end.

eot(Bs, Eot, Size, Max) ->
    MS = Max - Size,
    case binary:match(Bs, Eot) of
        nomatch when byte_size(Bs) < MS -> more;
        {Pos, _} when Pos < MS -> binary:part(Bs, {0, Pos});
        _ -> binary:part(Bs, {0, MS})
    end.

flush_exit(Port) ->
    receive
        {'EXIT',  Port,  _} -> ok
    after 0 -> ok
    end.

flush_until_down(Port, MonRef) ->
    receive
        {Port, {data, _Bytes}} -> flush_until_down(Port, MonRef);
        {'DOWN', MonRef, _, _, _} -> flush_exit(Port)
    end.

-compile({inline, [mk_cmd/1, eot/4]}).
-endif.
