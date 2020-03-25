-module(otpbp_file).

-ifndef(HAVE_file__del_dir_r_1).
-export([del_dir_r/1]).
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
