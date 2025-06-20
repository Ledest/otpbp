-module(otpbp_binary).

-ifndef(HAVE_binary__decode_hex_1).
% OTP 24.0
-export([decode_hex/1]).
-endif.
-ifndef(HAVE_binary__encode_hex_1).
% OTP 24.0
-export([encode_hex/1]).
-endif.
-ifndef(HAVE_binary__encode_hex_2).
% OTP 26.0
-export([encode_hex/2]).
-endif.
-ifndef(HAVE_binary__join_2).
% OTP 28.0
-export([join/2]).
-endif.

-ifndef(HAVE_binary__decode_hex_1).
decode_hex(Bin) when byte_size(Bin) rem 2 =:= 0 ->
    <<<<(decode_hex_char(A)):4, (decode_hex_char(B)):4>> || <<A, B>> <= Bin>>;
decode_hex(Bin) -> error(badarg, [Bin]).

decode_hex_char(Char) when Char >= $a, Char =< $f -> Char - ($a - 10);
decode_hex_char(Char) when Char >= $A, Char =< $F -> Char - ($A - 10);
decode_hex_char(Char) when Char >= $0, Char =< $9 -> Char - $0;
decode_hex_char(Char) -> error(badarg, [Char]).
-endif.

-ifndef(HAVE_binary__encode_hex_1).
encode_hex(Bin) when is_binary(Bin) -> <<<<(encode_hex_digit(A)), (encode_hex_digit(B))>> || <<A:4, B:4>> <= Bin>>;
encode_hex(Bin) -> error(badarg, [Bin]).

encode_hex_digit(Char) when Char =< 9 -> Char + $0;
encode_hex_digit(Char) -> Char + ($A - 10).
-endif.

-ifndef(HAVE_binary__encode_hex_2).
-ifdef(HAVE_binary__encode_hex_1).
encode_hex(Bin, uppercase) when is_binary(Bin) -> binary:encode_hex(Bin);
encode_hex(Bin, lowercase) when is_binary(Bin) ->
    <<<<(encode_hex_digit_lowercase(A)), (encode_hex_digit_lowercase(B))>> || <<A:4, B:4>> <= Bin>>;
encode_hex(Bin, Case) -> error(badarg, [Bin, Case]).

encode_hex_digit_lowercase(Char) when Char =< 9 -> Char + $0;
encode_hex_digit_lowercase(Char) -> Char + ($a - 10).
-else.
encode_hex(Bin, Case) when is_binary(Bin), Case =:= uppercase orelse Case =:= lowercase ->
    <<<<(encode_hex_digit(A, Case)), (encode_hex_digit(B, Case))>> || <<A:4, B:4>> <= Bin>>;
encode_hex(Bin, Case) -> error(badarg, [Bin, Case]).

encode_hex_digit(Char, _) when Char =< 9 -> Char + $0;
encode_hex_digit(Char, uppercase) -> Char + ($A - 10);
encode_hex_digit(Char, _lowercase) -> Char + ($a - 10).
-endif.
-endif.

-ifndef(HAVE_binary__join_2).
join([], Separator) when is_binary(Separator) -> <<>>;
join([H], Separator) when is_binary(H), is_binary(Separator) -> H;
join([H|T] = List, Separator) when is_binary(Separator) ->
    Acc = <<>>, %Enable private-append optimization
    try
        join(T, Separator, <<Acc/binary, H/binary>>)
    catch
        error:_ -> error(badarg, [List, Separator])
    end;
join(Arg, Separator) -> error(badarg, [Arg, Separator]).

join([], _Separator, Acc) -> Acc;
join([H|T], Separator, Acc) -> join(T, Separator, <<Acc/binary, Separator/binary, H/binary>>).
-endif.
