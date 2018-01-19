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
