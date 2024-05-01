-module(otpbp_beam_lib).

-ifndef(HAVE_beam_lib__significant_chunks_0).
% OTP 22.0
-export([significant_chunks/0]).
-endif.
-ifndef(HAVE_beam_lib__strip_2).
% OTP 22.0
-export([strip/2]).
-ifndef(NEED_strip_file_2).
-define(NEED_strip_file_2, true).
-endif.
-endif.
-ifndef(HAVE_beam_lib__strip_files_2).
% OTP 22.0
-export([strip_files/2]).
-ifndef(NEED_strip_file_2).
-define(NEED_strip_file_2, true).
-endif.
-endif.
-ifndef(HAVE_beam_lib__strip_release_2).
% OTP 22.0
-export([strip_release/2]).
-ifndef(NEED_strip_file_2).
-define(NEED_strip_file_2, true).
-endif.
-endif.

-ifndef(NEED_strip_file_2).
-ifdef(HAVE_beam_lib__significant_chunks_0).
-import(beam_lib, [significant_chunks/0]).
-endif.
-endif.

-ifndef(HAVE_beam_lib__strip_2).
strip(FileName, AdditionalChunks) -> catch strip_file(FileName, AdditionalChunks).
-endif.

-ifndef(HAVE_beam_lib__strip_files_2).
-ifndef(NEED_strip_fils_2).
-define(NEED_strip_fils_2, true).
-endif.

strip_files(Files, AdditionalChunks) when is_list(Files) -> catch strip_fils(Files, AdditionalChunks).
-endif.

-ifndef(HAVE_beam_lib__strip_release_2).
strip_release(Root, AdditionalChunks) -> catch strip_rel(Root, AdditionalChunks).

-ifndef(NEED_strip_fils_2).
-define(NEED_strip_fils_2, true).
-endif.

-ifndef(NEED_error_1).
-define(NEED_error_1, true).
-endif.

-compile({inline, [strip_rel/2]}).
strip_rel(Root, AdditionalChunks) ->
    filelib:is_dir(Root) orelse error({not_a_directory, Root}),
    strip_fils(filelib:wildcard(filename:join([Root, "lib", "*", "ebin", "*.beam"])), AdditionalChunks).
-endif.

-ifdef(NEED_strip_fils_2).
strip_fils(Files, AdditionalChunks) ->
    {ok, lists:map(fun(F) ->
                       {ok, Reply} = strip_file(F, AdditionalChunks),
                       Reply
                   end, Files)}.
-endif.

-ifdef(NEED_strip_file_2).
-ifndef(NEED_error_1).
-define(NEED_error_1, true).
-endif.

strip_file(File, AdditionalChunks) ->
    {Mod, Chunks} = read_significant_chunks(File, AdditionalChunks ++ significant_chunks()),
    {ok, Stripped} = beam_lib:build_module(Chunks),
    strip_file(File, Mod, zlib:gzip(Stripped)).

-compile({inline, [strip_file/3]}).
strip_file(File, Mod, Stripped) when is_binary(File) -> {ok, {Mod, Stripped}};
strip_file(File, Mod, Stripped) ->
    FileName = filename:rootname(File, ".beam") ++ ".beam",
    case file:open(FileName, [raw, binary, write]) of
        {ok, Fd} ->
            case {file:write(Fd, Stripped), file:close(Fd)} of
                {ok, ok} -> {ok, {Mod, FileName}};
                {Error, ok} -> file_error(FileName, Error)
            end;
        Error -> file_error(FileName, Error)
    end.

-compile({inline, [read_significant_chunks/2]}).
read_significant_chunks(File, ChunkList) ->
    {ok, {Module, Chunks}} = beam_lib:chunks(File, ChunkList, [allow_missing_chunks]),
    {Module, lists:filter(fun({_, Data}) when is_binary(Data) -> true;
                             ({Id, missing_chunk}) ->
                              lists:member(Id, mandatory_chunks()) andalso error({missing_chunk, File, Id})
                          end, Chunks)}.

-compile({inline, [mandatory_chunks/0]}).
mandatory_chunks() -> ["Code", "ExpT", "ImpT", "StrT"].

file_error(FileName, {error, Reason}) -> error({file_error, FileName, Reason}).
-endif.

-ifdef(NEED_error_1).
-compile({no_auto_import, [error/1]}).
error(Reason) -> throw({error, beam_lib, Reason}).
-endif.

-ifndef(HAVE_beam_lib__significant_chunks_0).
significant_chunks() -> ["Line", "Atom", "AtU8", "Code", "StrT", "ImpT", "ExpT", "FunT", "LitT"].
-endif.
