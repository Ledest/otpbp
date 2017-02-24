-module(otpbp_filename).

-ifndef(HAVE_filename__safe_relative_path_1).
-export([safe_relative_path/1]).
-endif.

-ifndef(HAVE_filename__safe_relative_path_1).
safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative -> safe_relative_path(filename:split(Path), []);
        _ -> unsafe
    end.

safe_relative_path([$.|T], Acc) -> safe_relative_path(T, Acc);
safe_relative_path([<<$.>>|T], Acc) -> safe_relative_path(T, Acc);
safe_relative_path([".."|_], []) -> unsafe;
safe_relative_path([<<"..">>|_], []) -> unsafe;
safe_relative_path([".."|T], [_|Acc]) -> safe_relative_path(T, Acc);
safe_relative_path([<<"..">>|T], [_|Acc]) -> safe_relative_path(T, Acc);
safe_relative_path([H|T], Acc) -> safe_relative_path(T, [H|Acc]);
safe_relative_path([], []) -> [];
safe_relative_path([], Acc) -> filename:join(lists:reverse(Acc)).
-endif.
