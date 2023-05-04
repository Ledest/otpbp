-module(otpbp_filelib).

-ifndef(HAVE_filelib__safe_relative_path_2).
% OTP 23.0
-export([safe_relative_path/2]).
-endif.
-ifndef(HAVE_filelib__ensure_path_1).
% OTP 25.0
-export([ensure_path/1]).
-endif.
-ifndef(HAVE_filelib__find_file_2).
% OTP 20.0
-export([find_file/2]).
-endif.
-ifndef(HAVE_filelib__find_file_3).
% OTP 20.0
-export([find_file/3]).
-endif.
-ifndef(HAVE_filelib__find_source_1).
% OTP 20.0
-export([find_source/1]).
-endif.
-ifndef(HAVE_filelib__find_source_2).
% OTP 20.0
-export([find_source/2]).
-endif.
-ifndef(HAVE_filelib__find_source_3).
% OTP 20.0
-export([find_source/3]).
-endif.

-ifndef(HAVE_filelib__find_file_2).
-ifdef(HAVE_filelib__find_file_3).
-import(filelib, [find_file/3]).
-endif.
-endif.
-ifndef(HAVE_filelib__find_source_1).
-ifdef(HAVE_filelib__find_source_2).
-import(filelib, [find_source/2]).
-endif.
-endif.
-ifndef(HAVE_filelib__find_source_2).
-ifdef(HAVE_filelib__find_source_3).
-import(filelib, [find_source/3]).
-endif.
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

-ifndef(HAVE_filelib__find_file_2).
find_file(Filename, Dir) -> find_file(Filename, Dir, []).
-endif.

-ifndef(HAVE_filelib__find_file_3).
find_file(Filename, Dir, []) -> find_file(Filename, Dir, get_search_rules());
find_file(Filename, Dir, Rules) -> try_dir_rules(keep_dir_search_rules(Rules), Filename, Dir).

try_dir_rules([{From, To}|Rest], Filename, Dir) when is_list(From), is_list(To) ->
    case try_dir_rule(Dir, Filename, From, To) of
        {ok, _File} = R -> R;
        error -> try_dir_rules(Rest, Filename, Dir)
    end;
try_dir_rules([], _Filename, _Dir) -> {error, not_found}.

try_dir_rule(Dir, Filename, From, To) ->
    case lists:suffix(From, Dir) of
        true ->
            Src = filename:join(lists:sublist(Dir, 1, length(Dir) - length(From)) ++ To, Filename),
            case filelib:is_regular(Src) of
                true -> {ok, Src};
                false -> find_regular_file(filelib:wildcard(Src))
            end;
        false -> error
    end.

find_regular_file([]) -> error;
find_regular_file([File|Files]) ->
    case filelib:is_regular(File) of
        true -> {ok, File};
        false -> find_regular_file(Files)
    end.

keep_dir_search_rules(Rules) -> [T || {_, _} = T <- Rules].

-ifndef(NEED_get_search_rules_0).
-define(NEED_get_search_rules_0, true).
-endif.
-endif.

-ifndef(HAVE_filelib__find_source_1).
find_source(FilePath) -> find_source(filename:basename(FilePath), filename:dirname(FilePath)).
-endif.

-ifndef(HAVE_filelib__find_source_2).
find_source(Filename, Dir) -> find_source(Filename, Dir, []).
-endif.

-ifndef(HAVE_filelib__find_source_3).
find_source(Filename, Dir, []) -> find_source(Filename, Dir, get_search_rules());
find_source(Filename, Dir, Rules) -> try_suffix_rules(keep_suffix_search_rules(Rules), Filename, Dir).

try_suffix_rules(Rules, Filename, Dir) ->
    Ext = filename:extension(Filename),
    try_suffix_rules(Rules, filename:rootname(Filename, Ext), Dir, Ext).

try_suffix_rules([{Ext, Src, Rules}|Rest], Root, Dir, Ext) when is_list(Src), is_list(Rules) ->
    case try_dir_rules(add_local_search(Rules), Root ++ Src, Dir) of
        {ok, _File} = R -> R;
        _Other -> try_suffix_rules(Rest, Root, Dir, Ext)
    end;
try_suffix_rules([_|Rest], Root, Dir, Ext) -> try_suffix_rules(Rest, Root, Dir, Ext);
try_suffix_rules([], _Root, _Dir, _Ext) -> {error, not_found}.

add_local_search(Rules) -> [{"", ""}|[X || X <- Rules, X =/= {"", ""}]].

keep_suffix_search_rules(Rules) -> [T || {_, _, _} = T <- Rules].

-ifndef(NEED_get_search_rules_0).
-define(NEED_get_search_rules_0, true).
-endif.
-endif.

-ifdef(NEED_get_search_rules_0).
get_search_rules() ->
    case application:get_env(kernel, source_search_rules, []) of
        []  -> default_search_rules();
        R when is_list(R) -> R
    end.

default_search_rules() ->
    [%% suffix-speficic rules for source search
     {".beam", ".erl", erl_source_search_rules()},
     {".erl", ".yrl", []},
     {"", ".src", erl_source_search_rules()},
     {".so", ".c", c_source_search_rules()},
     {".o", ".c", c_source_search_rules()},
     {"", ".c", c_source_search_rules()},
     {"", ".in", basic_source_search_rules()},
     %% plain old directory rules, backwards compatible
     {"", ""}] ++ erl_source_search_rules().

basic_source_search_rules() -> erl_source_search_rules() ++ c_source_search_rules().

erl_source_search_rules() ->
    [{"ebin", "src"}, {"ebin", "esrc"}, {"ebin", filename:join("src", "*")}, {"ebin", filename:join("esrc", "*")}].

c_source_search_rules() -> [{"priv", "c_src"}, {"priv", "src"}, {"bin", "c_src"}, {"bin", "src"}, {"", "src"}].
-endif.
