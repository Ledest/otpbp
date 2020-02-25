-module(otpbp_code).

-ifndef(HAVE_code__module_status_1).
-export([module_status/1]).
-ifndef(HAVE_code__modified_modules_0).
-export([modified_modules/0]).
-endif.
-endif.
-ifndef(HAVE_code__all_available_0).
-export([all_available/0]).
-endif.

-ifndef(HAVE_code__module_status_1).
module_status(Module) -> module_status(Module, code:get_path()).

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

module_changed_on_disk(Module, Path) ->
    Arch = erlang:system_info(hipe_architecture),
    case Arch =/= undefined andalso code:is_module_native(Module) of
        true -> try beam_lib:chunks(Path, [hipe_unified_loader:chunk_name(Arch)]) of
                    {ok, {_, [{_, NativeCode}]}} when is_binary(NativeCode) -> erlang:md5(NativeCode)
                catch
                    _:_ -> undefined
                end;
        _false -> case beam_lib:md5(Path) of
                      {ok, {_, MD5}} -> MD5;
                      _ -> undefined
                  end
    end =/= module_md5(Module).
-compile({inline, [module_changed_on_disk/2]}).

-ifdef(HAVE_erlang__get_module_info__md5).
module_md5(Module) -> erlang:get_module_info(Module, md5).
-else.
module_md5(Module) ->
    case lists:keyfind(vsn, 1, erlang:get_module_info(Module, attributes)) of
        {_, [V]} when is_integer(V) -> binary:encode_unsigned(V);
        _ -> undefined
    end.
-endif.
-compile({inline, [module_md5/1]}).

-ifndef(HAVE_code__modified_modules_0).
modified_modules() ->
    PathFiles = lists:foldr(fun(Path, A) ->
                                case erl_prim_loader:list_dir(Path) of
                                    {ok, Files} -> [{Path, Files}|A];
                                    _ -> A
                                end
                            end, [], code:get_path()),
    [M || {M, _} <- code:all_loaded(), module_status(M, PathFiles) =:= modified].
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
