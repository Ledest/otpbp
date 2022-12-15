-module(otpbp_binary).

-ifndef(HAVE_binary__decode_hex_1).
% OTP 24.0
-export([decode_hex/1]).
-endif.
-ifndef(HAVE_binary__encode_hex_1).
% OTP 24.0
-export([encode_hex/1]).
-endif.

-ifndef(HAVE_binary__decode_hex_1).
decode_hex(Bin) ->
    is_binary(Bin) orelse error(badarg),
    decode_hex(Bin, <<>>).

decode_hex(<<>>, Acc) -> Acc;
decode_hex(<<A:8, B:8, Rest/binary>>, Acc) ->
    decode_hex(Rest, <<Acc/binary, (decode_hex_char(A)):4, (decode_hex_char(B)):4>>);
decode_hex(_Bin, _Acc) -> error(badarg).

decode_hex_char(Char) when Char >= $a, Char =< $f -> Char - ($a - 10);
decode_hex_char(Char) when Char >= $A, Char =< $F -> Char - ($A - 10);
decode_hex_char(Char) when Char >= $0, Char =< $9 -> Char - $0;
decode_hex_char(_Char) -> error(badarg).
-endif.

-ifndef(HAVE_binary__encode_hex_1).
encode_hex(Bin) when is_binary(Bin) -> encode_hex_(Bin, <<>>);
encode_hex(Bin) -> error(badarg, [Bin]).

encode_hex_(<<>>, Acc) -> Acc;
encode_hex_(<<A:4, B:4, Rest/binary>>, Acc) ->
    encode_hex_(Rest, <<Acc/binary,  (encode_hex_digit(A)), (encode_hex_digit(B))>>).

encode_hex_digit(Char) when Char =< 9 -> Char + $0;
encode_hex_digit(Char) when Char =< 16#F -> Char + ($A - 10).
-endif.
