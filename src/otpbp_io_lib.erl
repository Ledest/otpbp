-module(otpbp_io_lib).

-ifndef(HAVE_io_lib__bformat_2).
% OTP 28.0
-export([bformat/2]).
-endif.
-ifndef(HAVE_io_lib__bformat_3).
% OTP 28.0
-export([bformat/3]).
-endif.
-ifndef(HAVE_io_lib__bwrite_1).
-export([bwrite/1]).
-endif.
-ifndef(HAVE_io_lib__bwrite_2).
% OTP 28.0
-export([bwrite/2]).
-endif.
-ifndef(HAVE_io_lib__bwrite_string_2).
-export([bwrite_string/2]).
-endif.
-ifndef(HAVE_io_lib__bwrite_string_3).
% OTP 28.0
-export([bwrite_string/3]).
-endif.
-ifndef(HAVE_io_lib__write_5).
% OTP 28.0
-export([write/5]).
-endif.
-ifndef(HAVE_io_lib__write_bin_5).
% OTP 28.0
-export([write_bin/5]).
-endif.
-ifndef(HAVE_io_lib__write_string_bin_3).
% OTP 28.0
-export([write_string_bin/3]).
-endif.

-ifndef(HAVE_io_lib__bwrite_string_2).
-ifdef(HAVE_io_lib__bwrite_string_3).
-import(io_lib, [bwrite_string/3]).
-endif.
-endif.
-ifndef(HAVE_io_lib__write_bin_5).
-ifdef(HAVE_io_lib__write_5).
-import(io_lib, [write/5]).
-endif.
-endif.
-ifndef(HAVE_io_lib__write_string_bin_3).
-ifdef(HAVE_io_lib__bwrite_string_3).
-import(io_lib, [bwrite_string/3]).
-endif.
-endif.

-ifndef(HAVE_io_lib__bformat_2).
bformat(Format, Data) -> unicode:characters_to_binary(io_lib:format(Format, Data)).
-endif.

-ifndef(HAVE_io_lib__bformat_3).
bformat(Format, Data, Options) -> unicode:characters_to_binary(io_lib:format(Format, Data, Options)).
-endif.

-ifndef(HAVE_io_lib__bwrite_1).
bwrite(Term) -> unicode:characters_to_binary(io_lib:write(Term, [{encoding, utf8}])).
-endif.

-ifndef(HAVE_io_lib__bwrite_2).
bwrite(Term, Options) -> unicode:characters_to_binary(io_lib:write(Term, Options, [{encoding, utf8}])).
-endif.

-ifndef(HAVE_io_lib__bwrite_string_2).
bwrite_string(String, Qoute) -> bwrite_string(String, Qoute, unicode).
-endif.

-ifndef(HAVE_io_lib__bwrite_string_3).
bwrite_string(String, Qoute, latin1) when is_binary(String) ->
    unicode:characters_to_binary(io_lib:write_string(String, Qoute), latin1, utf8);
bwrite_string(String, Qoute, InEnc) when is_list(String); is_binary(String), InEnc =:= unicode ->
    unicode:characters_to_binary(io_lib:write_string(String, Qoute)).
-endif.

-ifndef(HAVE_io_lib__write_5).
write(Term, Depth, Encoding, MapsOrder, CharsLimit) ->
    io_lib:write(Term, [{depth, Depth}, {encoding, Encoding}, {maps_order, MapsOrder}, {chars_limit, CharsLimit}]).
-endif.

-ifndef(HAVE_io_lib__write_bin_5).
write_bin(Term, Depth, Encoding, MapsOrder, CharsLimit) ->
    unicode:characters_to_binary(write(Term, Depth, Encoding, MapsOrder, CharsLimit)).
-endif.

-ifndef(HAVE_io_lib__write_string_bin_3).
write_string_bin(String, Qoute, InEnc) ->
    B = bwrite_string(String, Qoute, InEnc),
    {B, string:length(B)}.
-endif.
