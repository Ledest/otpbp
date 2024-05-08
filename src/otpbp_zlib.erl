-module(otpbp_zlib).

-ifndef(HAVE_zlib__compress_2).
-export([compress/2]).
-endif.

-ifndef(HAVE_zlib__zip_2).
-export([zip/2]).
-endif.

-ifndef(HAVE_zlib__gzip_2).
-export([gzip/2]).
-endif.

-ifndef(HAVE_zlib__safeInflate_2).
% OTP 20.1
-export([safeInflate/2]).
-endif.
-ifndef(HAVE_zlib__inflate_3).
% OTP 20.1
-export([inflate/3]).
-endif.
-ifndef(HAVE_zlib__adler32_2).
% OTP < 27.0
-export([adler32/2]).
-endif.
-ifndef(HAVE_zlib__adler32_3).
% OTP < 27.0
-export([adler32/3]).
-endif.
-ifndef(HAVE_zlib__adler32_combine_4).
% OTP < 27.0
-export([adler32_combine/4]).
-endif.
-ifndef(HAVE_zlib__crc32_2).
% OTP < 27.0
-export([crc32/2]).
-endif.
-ifndef(HAVE_zlib__crc32_3).
% OTP < 27.0
-export([crc32/3]).
-endif.
-ifndef(HAVE_zlib__crc32_combine_4).
% OTP < 27.0
-export([crc32_combine/4]).
-endif.
-ifndef(HAVE_zlib__inflateChunk_1).
% OTP < 27.0
-export([inflateChunk/1]).
-endif.
-ifndef(HAVE_zlib__inflateChunk_2).
% OTP < 27.0
-export([inflateChunk/2]).
-ifndef(NEED_zlib__inflateChunk_2).
-define(NEED_zlib__inflateChunk_2, true).
-endif.
-endif.
-ifndef(HAVE_zlib__setBufSize_2).
-export([setBufSize/2]).
-endif.
-ifndef(HAVE_zlib__getBufSize_1).
-export([getBufSize/1]).
-endif.

-define(MAX_WBITS, 15).

-ifndef(HAVE_zlib__compress_2).
-spec compress(Data::iodata(), Level::zlib:zlevel()) -> binary().
compress(Data, Level) ->
    Z = zlib:open(),
    iolist_to_binary(try
                         ok = zlib:deflateInit(Z, Level),
                         B = zlib:deflate(Z, Data, finish),
                         ok = zlib:deflateEnd(Z),
                         B
                     after
                         zlib:close(Z)
                     end).
-endif.

-ifndef(HAVE_zlib__zip_2).
-ifndef(NEED_zlib__z_3).
-define(NEED_zlib__z_3, true).
-endif.
-spec zip(Data::iodata(), Level::zlib:zlevel()) -> binary().
zip(Data, Level) -> z(Data, Level, -?MAX_WBITS).
-endif.

-ifndef(HAVE_zlib__gzip2_1).
-ifndef(NEED_zlib__z_3).
-define(NEED_zlib__z_3, true).
-endif.
-spec gzip(Data::iodata(), Level::zlib:zlevel()) -> binary().
gzip(Data, Level) -> z(Data, Level, 16 + ?MAX_WBITS).
-endif.

-ifdef(NEED_zlib__z_3).
z(Data, Level, WindowBits) ->
    Z = zlib:open(),
    iolist_to_binary(try
                         ok = zlib:deflateInit(Z, Level, deflated, WindowBits, 8, default),
                         B = zlib:deflate(Z, Data, finish),
                         ok = zlib:deflateEnd(Z),
                         B
                     after
                         zlib:close(Z)
                     end).
-endif.

-ifndef(HAVE_zlib__safeInflate_2).
safeInflate(Z, []) ->
    try zlib:inflateChunk(Z) of
        {more, Output} -> {continue, Output};
        Output -> {finished, Output}
    catch
        error:{need_dictionary, Adler} -> {need_dictionary, Adler, []};
        C:R:S -> erlang:raise(C, R, S)
    end;
safeInflate(Z, Data) ->
    try zlib:inflateChunk(Z, Data) of
        {more, Output} -> {continue, Output};
        Output -> {finished, Output}
    catch
        error:{need_dictionary, Adler} -> {need_dictionary, Adler, []};
        C:R:S -> erlang:raise(C, R, S)
    end.
-endif.

-ifndef(HAVE_zlib__inflate_3).
inflate(Z, Data, Options) ->
    case lists:keyfind(exception_on_need_dict, 1, Options) of
        {_, false} ->
            try
                zlib:inflate(Z, Data)
            catch
                error:{need_dictionary, Adler} -> {need_dictionary, Adler, []};
                C:R:S -> erlang:raise(C, R, S)
            end;
        _ -> zlib:inflate(Z, Data)
    end.
-endif.

-ifndef(HAVE_zlib__adler32_2).
adler32(Z, Data) when is_reference(Z) -> erlang:adler32(Data);
adler32(_Z, _Data) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__adler32_3).
adler32(Z, Adler, Data) when is_reference(Z) -> erlang:adler32(Adler, Data);
adler32(_Z, _Adler, _Data) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__adler32_combine_4).
adler32_combine(Z, Adler1, Adler2, Size2) when is_reference(Z) -> erlang:adler32_combine(Adler1, Adler2, Size2);
adler32_combine(_Z, _Adler1, _Adler2, _Size2) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__crc32_2).
crc32(Z, Data) when is_reference(Z) -> erlang:crc32(Data);
crc32(_Z, _Data) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__crc32_3).
crc32(Z, CRC, Data) when is_reference(Z) -> erlang:crc32(CRC, Data);
crc32(_Z, _CRC, _Data) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__crc32_combine_4).
crc32_combine(Z, CRC1, CRC2, Size2) when is_reference(Z) -> erlang:crc32_combine(CRC1, CRC2, Size2);
crc32_combine(_Z, _CRC1, _CRC2, _Size2) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__inflateChunk_1).
inflateChunk(Z) -> inflateChunk(Z, <<>>).
-ifndef(NEED_zlib__inflateChunk_2).
-define(NEED_zlib__inflateChunk_2, true).
-endif.
-endif.

-ifdef(NEED_zlib__inflateChunk_2).
inflateChunk(Z, Data) ->
    case zlib:safeInflate(Z, Data) of
        {finished, Output} -> Output;
        {continue, Output} -> {more, Output}
    end.
-endif.

-ifndef(HAVE_zlib__setBufSize_2).
setBufSize(Z, Size) when is_reference(Z), is_integer(Size), Size > 16, Size < 1 bsl 24 -> ok;
setBufSize(_Z, _Size) -> error(badarg).
-endif.

-ifndef(HAVE_zlib__getBufSize_1).
getBufSize(Z) when is_reference(Z) -> 4000;
getBufSize(_Z) -> error(badarg).
-endif.
