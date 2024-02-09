-module(otpbp_file).

-ifndef(HAVE_file__del_dir_r_1).
% OTP 23.0
-export([del_dir_r/1]).
-endif.
-ifndef(HAVE_file__delete_2).
% OTP 24.0
-export([delete/2]).
-endif.
-ifndef(HAVE_file__read_file_2).
% OTP 27.0
-export([read_file/2]).
-endif.
-ifndef(HAVE_file__pid2name_1).
% OTP < 27.0
-export([pid2name/1]).
-endif.

-ifndef(HAVE_file__del_dir_r_1).
-include_lib("kernel/include/file.hrl").

del_dir_r(File) -> % rm -rf File
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            case file:list_dir_all(File) of
                {ok, Names} ->
                    lists:foreach(fun(Name) -> del_dir_r(filename:join(File, Name)) end, Names),
                    file:del_dir(File);
                {error, _Reason} -> file:del_dir(File)
            end;
        {ok, _FileInfo} -> file:delete(File);
        {error, _Reason} = Error -> Error
    end.
-endif.

-ifndef(HAVE_file__pid2name_1).
pid2name(Pid) when is_pid(Pid) -> undefined.
-endif.

-ifndef(HAVE_file__delete_2).
delete(Name, Opts) when is_list(Opts) ->
    case check_args(Opts, ok) of
        ok -> file:delete(Name);
        raw -> prim_file:delete(file_name(Name));
        Error -> Error
    end.

-ifndef(NEED_check_args_2).
-define(NEED_check_args_2, true).
-endif.
-ifndef(NEED_file_name_1).
-define(NEED_file_name_1, true).
-endif.
-endif.

-ifndef(HAVE_file__read_file_2).
read_file(Name, Opts) when is_list(Opts) ->
    case check_args(Opts, ok) of
        ok -> file:read_file(Name);
        raw -> prim_file:read_file(file_name(Name));
        Error -> Error
    end.

-ifndef(NEED_check_args_2).
-define(NEED_check_args_2, true).
-endif.
-ifndef(NEED_file_name_1).
-define(NEED_file_name_1, true).
-endif.
-endif.

-ifdef(NEED_check_args_2).
check_args([{error, _} = Error|_Rest], _) -> Error;
check_args([raw|Rest], _) -> check_args(Rest, raw);
check_args([_Name|Rest], R) -> check_args(Rest, R);
check_args([], R) -> R.
-endif.

-ifdef(NEED_file_name_1).
file_name(N) when is_binary(N) -> N;
file_name(N) ->
    try
        file_name(N, file:native_name_encoding())
    catch
        Reason -> {error, Reason}
    end.

file_name([C|T], E) when is_integer(C), E =:= utf8 orelse E =:= latin1 andalso C < 256 -> [C|file_name(T, E)];
file_name([H|T], E) -> file_name(H, E) ++ file_name(T, E);
file_name([], _) -> [];
file_name(N, _) when is_atom(N) -> atom_to_list(N);
file_name(_, _) -> throw(badarg).
-endif.
