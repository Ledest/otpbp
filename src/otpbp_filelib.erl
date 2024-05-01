-module(otpbp_filelib).

-ifndef(HAVE_filelib__safe_relative_path_2).
% OTP 23.0
-export([safe_relative_path/2]).
-endif.
-ifndef(HAVE_filelib__ensure_path_1).
% OTP 25.0
-export([ensure_path/1]).
-endif.

-ifndef(HAVE_filelib__safe_relative_path_2).
safe_relative_path(Path, "") -> safe_relative_path(Path, ".");
safe_relative_path(Path, Cwd) -> srp_path(filename:split(Path), Cwd, sets:new(), []).

srp_path([], _Cwd, _Seen, []) -> "";
srp_path([], _Cwd, _Seen, Acc) -> filename:join(Acc);
srp_path([H|Segs], Cwd, Seen, Acc) when H =:= "."; H =:= <<".">> -> srp_path(Segs, Cwd, Seen, Acc);
srp_path([H|_Segs], _Cwd, _Seen, []) when H =:= ".."; H =:= <<"..">> -> unsafe;
srp_path([H|Segs], Cwd, Seen, [_|_] = Acc) when H =:= ".."; H =:= <<"..">> ->
    srp_path(Segs, Cwd, Seen, lists:droplast(Acc));
srp_path([clear|Segs], Cwd, _Seen, Acc) -> srp_path(Segs, Cwd, sets:new(), Acc);
srp_path([Seg|_] = Segs, Cwd, Seen, Acc) ->
    case filename:pathtype(Seg) of
        relative -> srp_segment(Segs, Cwd, Seen, Acc);
        _ -> unsafe
    end.

srp_segment([Seg|Segs], Cwd, Seen, Acc) ->
    Path = filename:join([Cwd|Acc]),
    case file:read_link(filename:join(Path, Seg)) of
        {ok, LinkPath} -> srp_link(Path, LinkPath, Segs, Cwd, Seen, Acc);
        {error, _} -> srp_path(Segs, Cwd, Seen, Acc ++ [Seg])
    end.

srp_link(Path, LinkPath, Segs, Cwd, Seen, Acc) ->
    FullLinkPath = filename:join(Path, LinkPath),
    case sets:is_element(FullLinkPath, Seen) of
        true -> unsafe;
        false -> srp_path(filename:split(LinkPath) ++ [clear|Segs], Cwd, sets:add_element(FullLinkPath, Seen), Acc)
    end.

-compile({inline, [srp_segment/4, srp_link/6]}).
-endif.

-ifndef(HAVE_filelib__ensure_path_1).
ensure_path("/") -> ok;
ensure_path(Path) ->
    case filelib:is_dir(Path) of
        true -> ok;
        false ->
            case filename:dirname(Path) of
                Path -> {error, einval};
                Parent ->
                    _ = ensure_path(Parent),
                    case file:make_dir(Path) of
                        {error, eexist} ->
                            case filelib:is_dir(Path) of
                                true -> ok;
                                false -> {error, eexist}
                            end;
                        Other -> Other
                    end
            end
    end.
-endif.
