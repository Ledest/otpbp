-module(otpbp_filelib).

-ifndef(HAVE_filelib__safe_relative_path_2).
-export([safe_relative_path/2]).
-endif.

-ifndef(HAVE_filelib__safe_relative_path_2).
safe_relative_path(Path, Cwd) ->
    case filename:pathtype(Path) of
        relative -> safe_relative_path(filename:split(Path), Cwd, [], "");
        _ -> unsafe
    end.

safe_relative_path([], _Cwd, _PrevLinks, Acc) -> Acc;
safe_relative_path([Segment|Segments], Cwd, PrevLinks, Acc) ->
    case safe_relative_path(join(Acc, Segment)) of
        unsafe -> unsafe;
        SafeAccSegment ->
            case file:read_link(join(Cwd, SafeAccSegment)) of
                {ok, LinkPath} ->
                    case lists:member(LinkPath, PrevLinks) of
                        true -> unsafe;
                        false ->
                            case safe_relative_path(filename:split(LinkPath), Cwd, [LinkPath|PrevLinks], Acc) of
                                unsafe -> unsafe;
                                NewAcc -> safe_relative_path(Segments, Cwd, [], NewAcc)
                            end
                    end;
                {error, _} -> safe_relative_path(Segments, Cwd, PrevLinks, SafeAccSegment)
            end
    end.

join([], Path) -> Path;
join(Left, Right) -> filename:join(Left, Right).

safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative -> safe_relative_path_1(filename:split(Path), []);
        _ -> unsafe
    end.

safe_relative_path_1([H|T], Acc) when H =:= ".";  H =:= <<".">> -> safe_relative_path_1(T, Acc);
safe_relative_path_1([H|T], Acc) when H =:= "..";  H =:= <<"..">> -> climb(T, Acc);
safe_relative_path_1([H|T], Acc) -> safe_relative_path_1(T, [H|Acc]);
safe_relative_path_1([], []) -> [];
safe_relative_path_1([], Acc) -> filename:join(lists:reverse(Acc)).

climb(_, []) -> unsafe;
climb(T, [_|Acc]) -> safe_relative_path_1(T, Acc).

-compile({inline, [climb/2, safe_relative_path/1]}).
-endif.
