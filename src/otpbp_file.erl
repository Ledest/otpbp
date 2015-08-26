-module(otpbp_file).

-ifndef(HAVE_file__list_dir_all_1).
-export([list_dir_all/1]).
-endif.

-ifndef(HAVE_file__read_link_all_1).
-export([read_link_all/1]).
-endif.

-ifndef(HAVE_file__read_link_all_1).
read_link_all(Name) -> check_and_call(read_link_all, [file_name(Name)]).
-define(NEED_check_and_call_2, true).
-define(NEED_file_name_1, true).
-endif.

-ifndef(HAVE_file__list_dir_all_1).
list_dir_all(Name) -> check_and_call(list_dir_all, [file_name(Name)]).
-ifndef(NEED_check_and_call_2).
-define(NEED_check_and_call_2, true).
-endif.
-ifndef(NEED_file_name_1).
-define(NEED_file_name_1, true).
-endif.
-endif.

-ifdef(NEED_check_and_call_2).
-define(FILE_SERVER, file_server_2).   % Registered name
call(Command, Args) when is_list(Args) ->
    X = erlang:dt_spread_tag(true),
    Y = gen_server:call(?FILE_SERVER, list_to_tuple([Command|Args]), infinity),
    erlang:dt_restore_tag(X),
    Y.

check_and_call(Command, Args) when is_list(Args) ->
    case check_args(Args) of
        ok -> call(Command, Args);
        Error -> Error
    end.

check_args([{error, _} = Error|_]) -> Error;
check_args([_|Rest]) -> check_args(Rest);
check_args([]) -> ok.
-endif.

-ifdef(NEED_file_name_1).
file_name(N) when is_binary(N) -> N;
file_name(N) ->
    try file_name_1(N, file:native_name_encoding())
    catch Reason -> {error, Reason}
    end.

file_name_1([C|T], latin1) when is_integer(C), C < 256 -> [C|file_name_1(T, latin1)];
file_name_1([C|T], utf8) when is_integer(C) -> [C|file_name_1(T, utf8)];
file_name_1([H|T], E) -> file_name_1(H, E) ++ file_name_1(T, E);
file_name_1([], _) -> [];
file_name_1(N, _) when is_atom(N) -> atom_to_list(N);
file_name_1(_, _) -> throw(badarg).
-endif.
