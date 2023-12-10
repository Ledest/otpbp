-module(otpbp_code).

-ifndef(HAVE_code__module_status_1).
% OTP 20.0
-export([module_status/1]).
-endif.
-ifndef(HAVE_code__modified_modules_0).
% OTP 20.0
-export([modified_modules/0]).
-endif.
-ifndef(HAVE_code__all_available_0).
% OTP 23.0
-export([all_available/0]).
-endif.
-ifndef(HAVE_code__module_status_0).
% OTP 23.0
-export([module_status/0]).
-endif.
-ifndef(HAVE_code__get_doc_1).
% OTP 23.0
-export([get_doc/1]).
-endif.
-ifndef(HAVE_code__is_module_native_1).
% OTP < 26.0
-export([is_module_native/1]).
-endif.
-ifndef(HAVE_code__rehash_0).
% OTP < 26.0
-export([rehash/0]).
-endif.
-ifndef(HAVE_code__add_path_2).
% OTP 26.0
-export([add_path/2]).
-endif.
-ifndef(HAVE_code__add_patha_2).
% OTP 26.0
-export([add_patha/2]).
-endif.
-ifndef(HAVE_code__add_paths_2).
% OTP 26.0
-export([add_paths/2]).
-endif.
-ifndef(HAVE_code__add_pathsa_2).
% OTP 26.0
-export([add_pathsa/2]).
-endif.
-ifndef(HAVE_code__add_pathsz_2).
% OTP 26.0
-export([add_pathsz/2]).
-endif.
-ifndef(HAVE_code__add_pathz_2).
% OTP 26.0
-export([add_pathz/2]).
-endif.
-ifndef(HAVE_code__clear_cache_0).
% OTP 26.0
-export([clear_cache/0]).
-endif.
-ifndef(HAVE_code__del_paths_1).
% OTP 26.0
-export([del_paths/1]).
-endif.
-ifndef(HAVE_code__replace_path_3).
% OTP 26.0
-export([replace_path/3]).
-endif.
-ifndef(HAVE_code__set_path_2).
% OTP 26.0
-export([set_path/2]).
-endif.

-ifndef(HAVE_code__module_status_1).
module_status(Module) -> module_status(Module, code:get_path()).

-ifndef(NEED_module_status_2).
-define(NEED_module_status_2, true).
-endif.
-endif.

-ifndef(HAVE_code__modified_modules_0).
modified_modules() ->
    PathFiles = lists:foldr(fun(Path, A) ->
                                case erl_prim_loader:list_dir(Path) of
                                    {ok, Files} -> [{Path, Files}|A];
                                    _ -> A
                                end
                            end, [], code:get_path()),
    [M || {M, _} <- code:all_loaded(), module_status(M, PathFiles) =:= modified].

-ifndef(NEED_module_status_2).
-define(NEED_module_status_2, true).
-endif.
-endif.

-ifndef(HAVE_code__all_available_0).
all_available() ->
    all_available(case code:get_mode() of
                      interactive -> code:get_path();
                      embedded -> []
                  end,
                  #{}).

all_available([Path|Tail], Acc) ->
    all_available(Tail,
                  case erl_prim_loader:list_dir(Path) of
                      {ok, Files} -> all_available(Path, Files, Acc);
                      _Error ->  Acc
                  end);
all_available([], AllModules) ->
    lists:umerge(fun comp_modules/2,
                 lists:sort(fun comp_modules/2, [{atom_to_list(M), Path, true} || {M, Path} <- code:all_loaded()]),
                 lists:sort(fun comp_modules/2,
                            maps:fold(fun(File, Path, Acc) ->
                                          [{filename:rootname(File), filename:append(Path, File), false}|Acc]
                                      end, [], AllModules))).

all_available(Path, [File|T], Acc) ->
    all_available(Path, T,
                  case filename:extension(File) =/= ".beam" orelse maps:is_key(File, Acc) of
                      true -> Acc;
                      false -> Acc#{File => Path}
                  end);
all_available(_Path, [], Acc) -> Acc.

comp_modules({A, _, _}, {B, _, _}) -> comp_modules(A, B);
comp_modules(A, B) -> A =< B.
-endif.

-ifndef(HAVE_code__get_doc_1).
get_doc(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_code__module_status_0).
module_status() ->
    PathFiles = lists:filtermap(fun(Path) ->
                                    case erl_prim_loader:list_dir(Path) of
                                        {ok, Files} -> {true, {Path, Files}};
                                        _Error -> false
                                    end
                                end,
                                code:get_path()),
    [{M, module_status(M, PathFiles)} || {M, _} <- code:all_loaded()].

-ifndef(NEED_module_status_2).
-define(NEED_module_status_2, true).
-endif.
-endif.

-ifndef(HAVE_code__is_module_native_1).
is_module_native(Module) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, _} -> false;
        false -> undefined
    end;
is_module_native(Module) -> error(badarg, [Module]).
-endif.

-ifndef(HAVE_code__rehash_0).
rehash() -> error_logger:warning_report("The code path cache functionality has been removed").
-endif.

-ifndef(HAVE_code__add_path_2).
add_path(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_path(Dir).
-endif.

-ifndef(HAVE_code__add_patha_2).
add_patha(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_patha(Dir).
-endif.

-ifndef(HAVE_code__add_paths_2).
add_paths(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_paths(Dir).
-endif.

-ifndef(HAVE_code__add_pathsa_2).
add_pathsa(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_pathsa(Dir).
-endif.

-ifndef(HAVE_code__add_pathsz_2).
add_pathsz(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_pathsz(Dir).
-endif.

-ifndef(HAVE_code__add_pathz_2).
add_pathz(Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:add_pathz(Dir).
-endif.

-ifndef(HAVE_code__clear_cache_0).
clear_cache() -> ok.
-endif.

-ifndef(HAVE_code__del_paths_1).
del_paths(NamesOrDirs) -> lists:foreach(fun code:del_path/1, NamesOrDirs).
-endif.

-ifndef(HAVE_code__replace_path_3).
replace_path(Name, Dir, Cache) when Cache =:= cache; Cache =:= nocache -> code:replace_path(Name, Dir).
-endif.

-ifndef(HAVE_code__set_path_2).
set_path(Path, Cache) when Cache =:= cache; Cache =:= nocache -> code:set_path(Path).
-endif.

-ifdef(NEED_module_status_2).
module_status(Module, PathFiles) ->
    case code:is_loaded(Module) of
        false -> not_loaded;
        {file, Loaded} when Loaded =:= preloaded; Loaded =:= [] -> loaded;
        {file, Loaded} when Loaded =:= cover_compiled; is_list(Loaded) ->
            case where_is_file(PathFiles, atom_to_list(Module) ++ code:objfile_extension()) of
                non_existing -> removed;
                Path -> case Loaded =:= cover_compiled orelse module_changed_on_disk(Module, Path) of
                            true -> modified;
                            false -> loaded
                        end
            end
    end.

where_is_file([], _) -> non_existing;
where_is_file([{D, Files}|T], File) -> where_is_file(T, File, D, Files);
where_is_file([D|T], File) ->
    case erl_prim_loader:list_dir(D) of
        {ok, Files} -> where_is_file(T, File, D, Files);
        _Error -> where_is_file(T, File)
    end.

where_is_file(T, File, D, Files) ->
    case lists:member(File, Files) of
        true -> filename:append(D, File);
        _false -> where_is_file(T, File)
    end.

-compile({inline, [module_changed_on_disk/2]}).
module_changed_on_disk(Module, Path) ->
    Arch = erlang:system_info(hipe_architecture),
    case Arch =/= undefined andalso code:is_module_native(Module) of
        true ->
            try beam_lib:chunks(Path, [hipe_unified_loader:chunk_name(Arch)]) of
                {ok, {_, [{_, NativeCode}]}} when is_binary(NativeCode) -> erlang:md5(NativeCode);
                _ -> undefined
            catch
                _:_ -> undefined
            end;
        _false ->
            case beam_lib:md5(Path) of
                {ok, {_, MD5}} -> MD5;
                _ -> undefined
            end
    end =/= erlang:get_module_info(Module, md5).
-endif.
