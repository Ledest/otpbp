-module(otpbp_c).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_c__lm_0).
% OTP 20.0
-export([lm/0]).
-endif.
-ifndef(HAVE_c__c_3).
% OTP 20.0
-export([c/3]).
-endif.
-ifndef(HAVE_c__erlangrc_1).
% OTP 21.0
-export([erlangrc/1]).
-endif.
-ifndef(HAVE_c__h_1).
% OTP 23.0
-export([h/1]).
-endif.
-ifndef(HAVE_c__h_2).
% OTP 23.0
-export([h/2]).
-endif.
-ifndef(HAVE_c__h_3).
% OTP 23.0
-export([h/3]).
-endif.
-ifndef(HAVE_c__hcb_1).
% OTP 23.0
-export([hcb/1]).
-endif.
-ifndef(HAVE_c__hcb_2).
% OTP 23.0
-export([hcb/2]).
-endif.
-ifndef(HAVE_c__hcb_3).
% OTP 23.0
-export([hcb/3]).
-endif.
-ifndef(HAVE_c__ht_1).
% OTP 23.0
-export([ht/1]).
-endif.
-ifndef(HAVE_c__ht_2).
% OTP 23.0
-export([ht/2]).
-endif.
-ifndef(HAVE_c__ht_3).
% OTP 23.0
-export([ht/3]).
-endif.

-ifndef(HAVE_c__lm_0).
lm() -> lists:map(fun c:l/1, c:mm()).
-endif.

-ifndef(HAVE_c__erlangrc_1).
erlangrc([Home|_] = Paths) when is_list(Home) ->
    case file:path_eval(Paths, ".erlang") of
        {error, {Line, _Mod, _Term} =  E} = Error ->
            error_logger:error_msg("file:path_eval(~tp,.erlang): error on line ~p: ~ts~n",
                                   [Paths, Line, file:format_error(E)]),
            Error;
        {error, E} = Error when E =/= enoent ->
            error_logger:error_msg("file:path_eval(~tp,.erlang): ~ts~n", [Paths, file:format_error(E)]),
            Error;
        Other -> Other
    end.
-endif.

-ifndef(HAVE_c__c_3).
c(Module, Options, Filter) when is_atom(Module) ->
    case find_beam(Module) of
        BeamFile when is_list(BeamFile) ->
            case compile_info(Module, BeamFile) of
                Info when is_list(Info) ->
                    case find_source(BeamFile, Info) of
                        SrcFile when is_list(SrcFile) -> c(SrcFile, Options, Filter, BeamFile, Info);
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> {error, Error}
    end.

-compile({inline, [c/5]}).
c(SrcFile, Options, Filter, BeamFile, Info) ->
    io:fwrite("Recompiling ~ts\n", [SrcFile]),
    %% Note that it's possible that because of options such as 'to_asm',
    %% the compiler might not actually write a new beam file at all
    Backup = BeamFile ++ ".bak",
    case file:rename(BeamFile, Backup) of
        Status when Status =:= ok; Status =:= {error, enoent} ->
            case compile_and_load(SrcFile,
                                  %% Filter old options; also remove options that will be replaced.
                                  %% Write new beam over old beam unless other outdir is specified.
                                  Options ++ [{outdir, filename:dirname(BeamFile)}] ++
                                  case lists:keyfind(options, 1, Info) of
                                      {_, Opts} -> [O || O <- Opts, not is_outdir_opt(O) andalso Filter(O)];
                                      false -> []
                                  end) of
                {ok, _} = Result ->
                    Status =:= ok andalso file:delete(Backup),
                    Result;
                Error ->
                    Status =:= ok andalso file:rename(Backup, BeamFile),
                    Error
            end;
        Error -> Error
    end.

is_outdir_opt({outdir, _}) -> true;
is_outdir_opt(_) -> false.

-compile({inline, [find_beam/1]}).
find_beam(Module) ->
    case code:which(Module) of
        [_|_] = Beam ->
            case module_loaded(Module) of
                false -> Beam; % code:which/1 found this in the path
                true ->
                    case filelib:is_file(Beam) of
                        true -> Beam;
                        false -> code:where_is_file(atom_to_list(Module) ++ code:objfile_extension()) % file moved?
                    end
            end;
        Other when Other =:= ""; Other =:= cover_compiled ->
            %% module is loaded but not compiled directly from source
            code:where_is_file(atom_to_list(Module) ++ code:objfile_extension());
        Error -> Error
    end.

-compile({inline, [compile_info/2]}).
compile_info(Module, Beam) ->
    case module_loaded(Module) of
        true ->
            %% getting the compile info for a loaded module should normally
            %% work, but return an empty info list if it fails
            try
                erlang:get_module_info(Module, compile)
            catch
                _:_ -> []
            end;
        false ->
            case beam_lib:chunks(Beam, [compile_info]) of
                {ok, {_Module, [{compile_info, Info}]}} -> Info;
                Error -> Error
            end
    end.

-compile({inline, [find_source/2]}).
find_source(BeamFile, Info) ->
    case lists:keyfind(source, 1, Info) of
        {_, SrcFile} ->
            case filelib:is_file(SrcFile) of
                true -> SrcFile;
                false -> find_source(BeamFile)
            end;
        _ -> find_source(BeamFile)
    end.

find_source(BeamFile) ->
    case filelib:find_source(BeamFile) of
        {ok, SrcFile} -> SrcFile;
        _ -> {error, no_source}
    end.

compile_and_load(File, Opts0) when is_list(Opts0) ->
    {[Opt|_], Opts1} = lists:partition(fun is_outdir_opt/1, Opts0 ++ [{outdir, "."}]),
    Opts = [report_errors, report_warnings|ensure_from(filename:extension(File), [Opt|Opts1])],
    case compile:file(File, Opts) of
        {ok, Mod} -> purge_and_load(Mod, File, Opts); % Listing file.
        {ok, Mod, _Ws} -> purge_and_load(Mod, File, Opts); % Warnings maybe turned on.
        Other -> Other % Errors go here
    end;
compile_and_load(File, Opt) -> compile_and_load(File, [Opt]).

-compile({inline, [ensure_from/2]}).
ensure_from(Suffix, Opts0) ->
    case lists:partition(fun(O) -> O =:= from_core orelse O =:= from_asm end,
                         if
                             Suffix =:= ".core" -> Opts0 ++ [from_core];
                             Suffix =:= ".S" -> Opts0 ++ [from_asm];
                             true -> Opts0
                         end) of
        {[Opt|_], Opts} -> [Opt|Opts];
        {[], Opts} -> Opts
    end.

purge_and_load(Mod, File, Opts) ->
    case compile:output_generated(Opts) of
        true ->
            Base = filename:basename(File, src_suffix(Opts)),
            case atom_to_list(Mod) of
                Base ->
                    code:purge(Mod),
                    %% Note that load_abs() adds the object file suffix
                    case code:load_abs(filename:join(outdir(Opts), Base), Mod) of
                        {error, _} = Error -> Error;
                        _ -> {ok, Mod}
                    end;
                _OtherMod ->
                    io:fwrite("** Module name '~p' does not match file name '~tp' **~n", [Mod, File]),
                    {error, badfile}
            end;
        false -> io:fwrite("** Warning: No object file created - nothing loaded **~n")
    end.

src_suffix([from_core|_]) -> ".core";
src_suffix([from_asm|_])  -> ".S";
src_suffix([_|Opts]) -> src_suffix(Opts);
src_suffix([]) -> ".erl".

outdir([{outdir, D}|_]) -> D;
outdir([_|Rest]) -> outdir(Rest);
outdir([]) -> ".".
-endif.

-ifndef(HAVE_c__h_1).
h(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__h_2).
h(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__h_3).
h(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_1).
hcb(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_2).
hcb(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_3).
hcb(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_1).
ht(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_2).
ht(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_3).
ht(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.
