-module(otpbp_code).

-ifndef(HAVE_code__module_status_1).
-export([module_status/1]).
-ifndef(HAVE_code__modified_modules_0).
-export([modified_modules/0]).
-endif.
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
    erlang:get_module_info(Module, md5) =/=
        case (Arch = erlang:system_info(hipe_architecture)) =/= undefined andalso code:is_module_native(Module) of
            true -> try beam_lib:chunks(Path, [hipe_unified_loader:chunk_name(Arch)]) of
                        {ok, {_, [{_, NativeCode}]}} when is_binary(NativeCode) -> erlang:md5(NativeCode)
                    catch
                        _:_ -> undefined
                    end;
            _false -> case beam_lib:md5(Path) of
                          {ok, {_, MD5}} -> MD5;
                          _ -> undefined
                      end
        end.
-compile([{inline, [module_changed_on_disk/2]}]).

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
