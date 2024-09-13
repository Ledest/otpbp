-module(otpbp_json).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_json__decode_1).
% OTP 27.0
-export([decode/1]).
-endif.
-ifndef(HAVE_json__decode_3).
% OTP 27.0
-export([decode/3]).
-endif.
-ifndef(HAVE_json__decode_continue_2).
% OTP 27.0
-export([decode_continue/2]).
-endif.
-ifndef(HAVE_json__decode_start_3).
% OTP 27.0
-export([decode_start/3]).
-endif.
-ifndef(HAVE_json__encode_1).
% OTP 27.0
-export([encode/1]).
-endif.
-ifndef(HAVE_json__encode_2).
% OTP 27.0
-export([encode/2]).
-endif.
-ifndef(HAVE_json__encode_atom_2).
% OTP 27.0
-export([encode_atom/2]).
-endif.
-ifndef(HAVE_json__encode_binary_1).
% OTP 27.0
-export([encode_binary/1]).
-endif.
-ifndef(HAVE_json__encode_binary_escape_all_1).
% OTP 27.0
-export([encode_binary_escape_all/1]).
-endif.
-ifndef(HAVE_json__encode_float_1).
% OTP 27.0
-export([encode_float/1]).
-endif.
-ifndef(HAVE_json__encode_key_value_list_2).
% OTP 27.0
-export([encode_key_value_list/2]).
-endif.
-ifndef(HAVE_json__encode_key_value_list_checked_2).
% OTP 27.0
-export([encode_key_value_list_checked/2]).
-endif.
-ifndef(HAVE_json__encode_list_2).
% OTP 27.0
-export([encode_list/2]).
-endif.
-ifndef(HAVE_json__encode_map_2).
% OTP 27.0
-export([encode_map/2]).
-endif.
-ifndef(HAVE_json__encode_map_checked_2).
% OTP 27.0
-export([encode_map_checked/2]).
-endif.
-ifndef(HAVE_json__encode_value_2).
% OTP 27.0
-export([encode_value/2]).
-endif.
-ifndef(HAVE_json__format_1).
% OTP 27.1
-export([format/1]).
-endif.
-ifndef(HAVE_json__format_2).
% OTP 27.1
-export([format/2]).
-endif.
-ifndef(HAVE_json__format_3).
% OTP 27.1
-export([format/3]).
-endif.
-ifndef(HAVE_json__format_value_3).
% OTP 27.1
-export([format_value/3]).
-endif.

-ifndef(HAVE_json__encode_value_2).
-ifdef(HAVE_json__encode_atom_2).
-import(json, [encode_atom/2]).
-endif.
-ifdef(HAVE_json__encode_float_1).
-import(json, [encode_float/1]).
-endif.
-endif.
-ifndef(HAVE_json__encode_1).
-ifdef(HAVE_json__encode_2).
-import(json, [encode/2]).
-endif.
-ifdef(HAVE_json__encode_atom_2).
-import(json, [encode_atom/2]).
-endif.
-ifdef(HAVE_json__encode_float_1).
-import(json, [encode_float/1]).
-endif.
-endif.
-ifndef(HAVE_json__format_1).
-ifdef(HAVE_json__format_3).
-import(json, [format/3, format_value/3]).
-endif.
-endif.
-ifndef(HAVE_json__format_2).
-ifdef(HAVE_json__format_3).
-import(json, [format/3, format_value/3]).
-endif.
-endif.
-ifndef(HAVE_json__format_value_3).
-ifdef(HAVE_json__encode_atom_2).
-import(json, [encode_atom/2]).
-endif.
-ifdef(HAVE_json__encode_binary_1).
-import(json, [encode_binary/1]).
-endif.
-ifdef(HAVE_json__encode_float_1).
-import(json, [encode_float/1]).
-endif.
-endif.

-ifndef(HAVE_json__decode_1).
-ifndef(NEED_record__decode).
-define(NEED_record__decode, true).
-endif.
-endif.
-ifndef(HAVE_json__decode_3).
-ifndef(NEED_record__decode).
-define(NEED_record__decode, true).
-endif.
-endif.
-ifndef(HAVE_json__decode_continue_2).
-ifndef(NEED_record__decode).
-define(NEED_record__decode, true).
-endif.
-endif.
-ifndef(HAVE_json__decode_start_3).
-ifndef(NEED_record__decode).
-define(NEED_record__decode, true).
-endif.
-endif.

-ifdef(NEED_record__decode).
-type from_binary_fun() :: fun((binary()) -> any()).
-type array_start_fun() :: fun((Acc :: any()) -> ArrayAcc :: any()).
-type array_push_fun() :: fun((Value :: any(), Acc :: any()) -> NewAcc :: any()).
-type array_finish_fun() :: fun((ArrayAcc :: any(), OldAcc :: any()) -> {any(), any()}).
-type object_start_fun() :: fun((Acc :: any()) -> ObjectAcc :: any()).
-type object_push_fun() :: fun((Key :: any(), Value :: any(), Acc :: any()) -> NewAcc :: any()).
-type object_finish_fun() :: fun((ObjectAcc :: any(), OldAcc :: any()) -> {any(), any()}).

-record(decode, {array_start :: array_start_fun() | undefined,
                 array_push = fun(Value, Acc) -> [Value|Acc] end :: array_push_fun(),
                 array_finish :: array_finish_fun() | undefined,
                 object_start :: object_start_fun() | undefined,
                 object_push = fun(Key, Value, Acc) -> [{Key, Value}|Acc] end :: object_push_fun(),
                 object_finish :: object_finish_fun() | undefined,
                 float = fun erlang:binary_to_float/1 :: from_binary_fun(),
                 integer = fun erlang:binary_to_integer/1 :: from_binary_fun(),
                 string = fun(Binary) -> Binary end :: from_binary_fun(),
                 null = null :: term()}).
-endif.

-ifndef(HAVE_json__decode_1).
decode(Binary) when is_binary(Binary) ->
    case value(Binary, Binary, 0, ok, [], #decode{}) of
        {Result, _Acc, <<>>} -> Result;
        {_, _, Rest} -> invalid_byte(Rest, 0);
        {continue, {_Bin, _Acc, [], _Decode, {number, Number}}} -> Number;
        {continue, {_, _, _, _, {float_error, Token, _Skip}}} -> unexpected_sequence(Token);
        {continue, _} -> error(unexpected_end)
    end.

-ifndef(NEED_value_6).
-define(NEED_value_6, true).
-endif.
-ifndef(NEED_invalid_byte_2).
-define(NEED_invalid_byte_2, true).
-endif.
-endif.

-ifndef(HAVE_json__decode_3).
decode(Binary, Acc0, Decoders) when is_binary(Binary) ->
    case value(Binary, Binary, 0, Acc0, [], maps:fold(fun parse_decoder/3, #decode{}, Decoders)) of
        {continue, {_Bin, Acc, [], _Decode, {number, Val}}} -> {Val, Acc, <<>>};
        {continue, {_, _, _, _, {float_error, Token, _Skip}}} -> unexpected_sequence(Token);
        {continue, _} -> error(unexpected_end);
        Result -> Result
    end.

-ifndef(NEED_value_6).
-define(NEED_value_6, true).
-endif.
-ifndef(NEED_parse_decoder_3).
-define(NEED_parse_decoder_3, true).
-endif.
-endif.

-ifndef(HAVE_json__decode_continue_2).
decode_continue(end_of_input, {_, Acc, [], _Decode, {number, Val}}) -> {Val, Acc, <<>>};
decode_continue(end_of_input, {_, _, _, _, {float_error, Token, _Skip}}) -> unexpected_sequence(Token);
decode_continue(end_of_input, _State) -> error(unexpected_end);
decode_continue(Cont, {Rest, Acc, Stack, #decode{} = Decode, FuncData}) when is_binary(Cont) ->
    Binary = <<Rest/binary, Cont/binary>>,
    case FuncData of
        value -> value(Binary, Binary, 0, Acc, Stack, Decode);
        {number, _} -> value(Binary, Binary, 0, Acc, Stack, Decode);
        {float_error, _Token, _Skip} -> value(Binary, Binary, 0, Acc, Stack, Decode);
        {array_push, Val} -> array_push(Binary, Binary, 0, Acc, Stack, Decode, Val);
        {object_value, Key} -> object_value(Binary, Binary, 0, Acc, Stack, Decode, Key);
        {object_push, Value, Key} -> object_push(Binary, Binary, 0, Acc, Stack, Decode, Value, Key);
        object_key -> object_key(Binary, Binary, 0, Acc, Stack, Decode)
    end.

-ifndef(NEED_value_6).
-define(NEED_value_6, true).
-endif.
-endif.

-ifndef(HAVE_json__decode_start_3).
decode_start(Binary, Acc, Decoders) when is_binary(Binary) ->
    value(Binary, Binary, 0, Acc, [], maps:fold(fun parse_decoder/3, #decode{}, Decoders)).

-ifndef(NEED_value_6).
-define(NEED_value_6, true).
-endif.
-ifndef(NEED_parse_decoder_3).
-define(NEED_parse_decoder_3, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_1).
encode(Term) -> encode(Term, fun do_encode/2).

-ifndef(NEED_do_encode_2).
-define(NEED_do_encode_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_2).
encode(Term, Encoder) when is_function(Encoder, 2) -> Encoder(Term, Encoder).
-endif.

-ifndef(HAVE_json__encode_atom_2).
encode_atom(null, _Encode) -> <<"null">>;
encode_atom(true, _Encode) -> <<"true">>;
encode_atom(false, _Encode) -> <<"false">>;
encode_atom(Other, Encode) -> Encode(atom_to_binary(Other, utf8), Encode).
-endif.

-ifndef(HAVE_json__encode_binary_1).
encode_binary(Bin) when is_binary(Bin) -> escape_binary(Bin).

-ifndef(NEED_escape_binary_1).
-define(NEED_escape_binary_1, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_binary_escape_all_1).
encode_binary_escape_all(Bin) when is_binary(Bin) -> escape_all_ascii(Bin, [$"], Bin, 0, 0).

-ifndef(NEED_escape_all_ascii_5).
-define(NEED_escape_all_ascii_5, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_float_1).
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-define(HAVE_float_to_binary_2__short, true).
-endif.
-endif.
-ifdef(HAVE_float_to_binary_2__short).
encode_float(Float) -> float_to_binary(Float, [short]).
-else.
encode_float(Float) -> io_lib_format:fwrite_g(Float).
-endif.
-endif.

-ifndef(HAVE_json__encode_key_value_list_2).
encode_key_value_list(List, Encode) when is_function(Encode, 2) ->
    encode_object([[$,, key(Key, Encode), $:|Encode(Value, Encode)] || {Key, Value} <- List]).

-ifndef(NEED_encode_object_1).
-define(NEED_encode_object_1, true).
-endif.
-ifndef(NEED_key_2).
-define(NEED_key_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_key_value_list_checked_2).
encode_key_value_list_checked(List, Encode) -> do_encode_checked(List, Encode).

-ifndef(NEED_do_encode_checked_2).
-define(NEED_do_encode_checked_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_list_2).
encode_list(List, Encode) when is_list(List) -> do_encode_list(List, Encode).

-ifndef(NEED_do_encode_list_2).
-define(NEED_do_encode_list_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_map_2).
encode_map(Map, Encode) when is_map(Map) -> do_encode_map(Map, Encode).

-ifndef(NEED_do_encode_map_2).
-define(NEED_do_encode_map_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_map_checked_2).
encode_map_checked(Map, Encode) -> do_encode_checked(maps:to_list(Map), Encode).

-ifndef(NEED_do_encode_checked_2).
-define(NEED_do_encode_checked_2, true).
-endif.
-endif.

-ifndef(HAVE_json__encode_value_2).
encode_value(Value, Encode) -> do_encode(Value, Encode).

-ifndef(NEED_do_encode_2).
-define(NEED_do_encode_2, true).
-endif.
-endif.

-ifndef(HAVE_json__format_1).
format(Term) -> format(Term, fun format_value/3, #{}).
-endif.

-ifndef(HAVE_json__format_2).
format(Term, Options) when is_map(Options) -> format(Term, fun format_value/3, Options);
format(Term, Encoder) when is_function(Encoder, 3) -> format(Term, Encoder, #{}).
-endif.

-ifndef(HAVE_json__format_3).
format(Term, Encoder, Options) when is_function(Encoder, 3) ->
    [Encoder(Term, Encoder, maps:merge(#{level => 0, col => 0, indent => 2, max => 100}, Options)), $\n].
-endif.

-ifndef(HAVE_json__format_value_3).
format_value(Atom, UserEnc, State) when is_atom(Atom) ->
    encode_atom(Atom, fun(Value, Enc) ->  UserEnc(Value, Enc, State) end);
format_value(Bin, _Enc, _State) when is_binary(Bin) -> encode_binary(Bin);
format_value(Int, _Enc, _State) when is_integer(Int) -> integer_to_binary(Int);
format_value(Float, _Enc, _State) when is_float(Float) -> encode_float(Float);
format_value(List, UserEnc, State) when is_list(List) -> format_list(List, UserEnc, State);
format_value(Map, UserEnc, State) when is_map(Map) ->
    %% Ensure order of maps are the same in each export
    format_key_value_list(lists:keysort(1, maps:to_list(Map)), UserEnc, State);
format_value(Other, _Enc, _State) -> error({unsupported_type, Other}).

format_list([Head|Rest], UserEnc, #{level := Level, col := Col0, max := Max} = State0) ->
    State1 = State0#{level := Level + 1},
    {Len, IndentElement} = indent(State1),
    if
        is_list(Head);   %% Indent list in lists
        is_map(Head);    %% Indent maps
        is_binary(Head); %% Indent Strings
        Col0 > Max ->    %% Throw in the towel
            State = State1#{col := Len},
            {_, IndLast} = indent(State0),
            [$[, IndentElement, UserEnc(Head, UserEnc, State),
             format_tail(Rest, UserEnc, State, IndentElement, IndentElement), IndLast, $]];
       true ->
            First = UserEnc(Head, UserEnc, State1),
            [$[, First,
             format_tail(Rest, UserEnc, State1#{col := Col0 + 1 + erlang:iolist_size(First)}, [], IndentElement), $]]
    end;
format_list([], _, _) -> <<"[]">>.

format_tail([Head|Tail], Enc, #{max := Max, col := Col0} = State, [], IndentRow) when Col0 < Max ->
    EncHead = Enc(Head, Enc, State),
    [[$,|EncHead]|format_tail(Tail, Enc, State#{col := Col0 + 1 + erlang:iolist_size(EncHead)}, [], IndentRow)];
format_tail([Head|Tail], Enc, State, [], IndentRow) ->
    String = [[$,|IndentRow]|Enc(Head, Enc, State)],
    [String|format_tail(Tail, Enc, State#{col := erlang:iolist_size(String) - 2}, [], IndentRow)];
format_tail([Head|Tail], Enc, State, IndentAll, IndentRow) ->
    %% These are handling their own indentation, so optimize away size calculation
    [[[$,|IndentAll]|Enc(Head, Enc, State)]|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([], _, _, _, _) -> [].

format_key_value_list(KVList, UserEnc, #{level := Level} = State) ->
    {_, Indent} = indent(State),
    NextState = State#{level := Level+1},
    {KISize, KeyIndent} = indent(NextState),
    EncKeyFun = fun(KeyVal, _Fun) -> UserEnc(KeyVal, UserEnc, NextState) end,
    format_object(lists:map(fun({Key, Value}) ->
                                EncKey = key(Key, EncKeyFun),
                                ValState = NextState#{col := KISize + 2 + erlang:iolist_size(EncKey)},
                                [$,, KeyIndent, EncKey, ": "|UserEnc(Value, UserEnc, ValState)]
                            end,
                            KVList),
                  Indent).

format_object([], _) -> <<"{}">>;
format_object([[_Comma, KeyIndent|Entry]], Indent) ->
    [_Key, _Colon|Value] = Entry,
    {_, Rest} = string:take(Value, [$\s, $\n]),
    [CP|_] = unicode_util:cp(Rest),
    if
        CP =:= ${; CP =:= $[ -> [${, KeyIndent, Entry, Indent, $}];
        true -> ["{ ", Entry, " }"]
    end;
format_object([[_Comma, KeyIndent|Entry]|Rest], Indent) -> [${, KeyIndent, Entry, Rest, Indent, $}].

indent(#{level := Level, indent := Indent}) ->
    Steps = Level * Indent,
    {Steps, steps(Steps)}.

steps(N) ->  [$\n|lists:duplicate(N, $\s)].

-ifndef(NEED_key_2).
-define(NEED_key_2, true).
-endif.
-endif.

-define(UTF8_ACCEPT, 0).
-define(UTF8_REJECT, 12).

-define(is_ascii_escape(Byte),
        Byte =:= 0 orelse
        Byte =:= 1 orelse
        Byte =:= 2 orelse
        Byte =:= 3 orelse
        Byte =:= 4 orelse
        Byte =:= 5 orelse
        Byte =:= 6 orelse
        Byte =:= 7 orelse
        Byte =:= 8 orelse
        Byte =:= 9 orelse
        Byte =:= 10 orelse
        Byte =:= 11 orelse
        Byte =:= 12 orelse
        Byte =:= 13 orelse
        Byte =:= 14 orelse
        Byte =:= 15 orelse
        Byte =:= 16 orelse
        Byte =:= 17 orelse
        Byte =:= 18 orelse
        Byte =:= 19 orelse
        Byte =:= 20 orelse
        Byte =:= 21 orelse
        Byte =:= 22 orelse
        Byte =:= 23 orelse
        Byte =:= 24 orelse
        Byte =:= 25 orelse
        Byte =:= 26 orelse
        Byte =:= 27 orelse
        Byte =:= 28 orelse
        Byte =:= 29 orelse
        Byte =:= 30 orelse
        Byte =:= 31 orelse
        Byte =:= 34 orelse
        Byte =:= 92).
-define(is_ascii_plain(Byte),
        Byte =:= 32 orelse
        Byte =:= 33 orelse
        Byte =:= 35 orelse
        Byte =:= 36 orelse
        Byte =:= 37 orelse
        Byte =:= 38 orelse
        Byte =:= 39 orelse
        Byte =:= 40 orelse
        Byte =:= 41 orelse
        Byte =:= 42 orelse
        Byte =:= 43 orelse
        Byte =:= 44 orelse
        Byte =:= 45 orelse
        Byte =:= 46 orelse
        Byte =:= 47 orelse
        Byte =:= 48 orelse
        Byte =:= 49 orelse
        Byte =:= 50 orelse
        Byte =:= 51 orelse
        Byte =:= 52 orelse
        Byte =:= 53 orelse
        Byte =:= 54 orelse
        Byte =:= 55 orelse
        Byte =:= 56 orelse
        Byte =:= 57 orelse
        Byte =:= 58 orelse
        Byte =:= 59 orelse
        Byte =:= 60 orelse
        Byte =:= 61 orelse
        Byte =:= 62 orelse
        Byte =:= 63 orelse
        Byte =:= 64 orelse
        Byte =:= 65 orelse
        Byte =:= 66 orelse
        Byte =:= 67 orelse
        Byte =:= 68 orelse
        Byte =:= 69 orelse
        Byte =:= 70 orelse
        Byte =:= 71 orelse
        Byte =:= 72 orelse
        Byte =:= 73 orelse
        Byte =:= 74 orelse
        Byte =:= 75 orelse
        Byte =:= 76 orelse
        Byte =:= 77 orelse
        Byte =:= 78 orelse
        Byte =:= 79 orelse
        Byte =:= 80 orelse
        Byte =:= 81 orelse
        Byte =:= 82 orelse
        Byte =:= 83 orelse
        Byte =:= 84 orelse
        Byte =:= 85 orelse
        Byte =:= 86 orelse
        Byte =:= 87 orelse
        Byte =:= 88 orelse
        Byte =:= 89 orelse
        Byte =:= 90 orelse
        Byte =:= 91 orelse
        Byte =:= 93 orelse
        Byte =:= 94 orelse
        Byte =:= 95 orelse
        Byte =:= 96 orelse
        Byte =:= 97 orelse
        Byte =:= 98 orelse
        Byte =:= 99 orelse
        Byte =:= 100 orelse
        Byte =:= 101 orelse
        Byte =:= 102 orelse
        Byte =:= 103 orelse
        Byte =:= 104 orelse
        Byte =:= 105 orelse
        Byte =:= 106 orelse
        Byte =:= 107 orelse
        Byte =:= 108 orelse
        Byte =:= 109 orelse
        Byte =:= 110 orelse
        Byte =:= 111 orelse
        Byte =:= 112 orelse
        Byte =:= 113 orelse
        Byte =:= 114 orelse
        Byte =:= 115 orelse
        Byte =:= 116 orelse
        Byte =:= 117 orelse
        Byte =:= 118 orelse
        Byte =:= 119 orelse
        Byte =:= 120 orelse
        Byte =:= 121 orelse
        Byte =:= 122 orelse
        Byte =:= 123 orelse
        Byte =:= 124 orelse
        Byte =:= 125 orelse
        Byte =:= 126 orelse
        Byte =:= 127).

-define(are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8),
        (?is_ascii_plain(B1)) andalso
        (?is_ascii_plain(B2)) andalso
        (?is_ascii_plain(B3)) andalso
        (?is_ascii_plain(B4)) andalso
        (?is_ascii_plain(B5)) andalso
        (?is_ascii_plain(B6)) andalso
        (?is_ascii_plain(B7)) andalso
        (?is_ascii_plain(B8))).

-ifdef(NEED_do_encode_2).
do_encode(Value, Encode) when is_atom(Value) -> encode_atom(Value, Encode);
do_encode(Value, _Encode) when is_binary(Value) -> escape_binary(Value);
do_encode(Value, _Encode) when is_integer(Value) -> integer_to_binary(Value);
do_encode(Value, _Encode) when is_float(Value) -> encode_float(Value);
do_encode(Value, Encode) when is_list(Value) -> do_encode_list(Value, Encode);
do_encode(Value, Encode) when is_map(Value) -> do_encode_map(Value, Encode);
do_encode(Other, _Encode) -> error({unsupported_type, Other}).

-ifndef(NEED_escape_binary_1).
-define(NEED_escape_binary_1, true).
-endif.
-ifndef(NEED_do_encode_list_2).
-define(NEED_do_encode_list_2, true).
-endif.
-ifndef(NEED_do_encode_map_2).
-define(NEED_do_encode_map_2, true).
-endif.
-endif.

-ifdef(NEED_escape_binary_1).
escape_binary(Bin) -> escape_binary_ascii(Bin, [$"], Bin, 0, 0).

escape_binary_ascii(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Acc, Orig, Skip, Len)
  when ?are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8) ->
    escape_binary_ascii(Rest, Acc, Orig, Skip, Len + 8);
escape_binary_ascii(Binary, Acc, Orig, Skip, Len) -> escape_binary(Binary, Acc, Orig, Skip, Len).

escape_binary(<<Byte, Rest/binary>>, Acc, Orig, Skip, Len) when ?is_ascii_plain(Byte) ->
    escape_binary(Rest, Acc, Orig, Skip, Len + 1);
escape_binary(<<Byte, Rest/binary>>, Acc, Orig, Skip, 0) when ?is_ascii_escape(Byte) ->
    escape_binary_ascii(Rest, [Acc|escape(Byte)], Orig, Skip + 1, 0);
escape_binary(<<Byte, Rest/binary>>, Acc, Orig, Skip, Len) when ?is_ascii_escape(Byte) ->
    escape_binary_ascii(Rest, [Acc, binary_part(Orig, Skip, Len)|escape(Byte)], Orig, Skip + Len + 1, 0);
escape_binary(<<Byte, Rest/binary>>, Acc, Orig, Skip, Len) ->
    case element(Byte - 127, utf8s0()) of
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len);
        State -> escape_binary_utf8(Rest, Acc, Orig, Skip, Len, State)
    end;
escape_binary(_, _Acc, Orig, 0, _Len) -> [$", Orig, $"];
escape_binary(_, Acc, _Orig, _Skip, 0) -> [Acc, $"];
escape_binary(_, Acc, Orig, Skip, Len) -> [Acc, binary_part(Orig, Skip, Len), $"].

escape_binary_utf8(<<Byte, Rest/binary>>, Acc, Orig, Skip, Len, State0) ->
    case element(State0 + element(Byte + 1, utf8t()), utf8s()) of
        ?UTF8_ACCEPT -> escape_binary_ascii(Rest, Acc, Orig, Skip, Len + 2);
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len + 1);
        State -> escape_binary_utf8(Rest, Acc, Orig, Skip, Len + 1, State)
    end;
escape_binary_utf8(_, _Acc, Orig, Skip, Len, _State) -> unexpected_utf8(Orig, Skip + Len + 1).

unexpected_utf8(Original, Skip) when byte_size(Original) =:= Skip -> error(unexpected_end);
unexpected_utf8(Original, Skip) -> invalid_byte(Original, Skip).

-ifndef(NEED_escape_all_ascii_5).
-define(NEED_escape_all_ascii_5, true).
-endif.
-ifndef(NEED_escape_1).
-define(NEED_escape_1, true).
-endif.
-ifndef(NEED_invalid_byte_2).
-define(NEED_invalid_byte_2, true).
-endif.
-ifndef(NEED_utf8s0_0).
-define(NEED_utf8s0_0, true).
-endif.
-ifndef(NEED_utf8t_0).
-define(NEED_utf8t_0, true).
-endif.
-ifndef(NEED_utf8s_0).
-define(NEED_utf8s_0, true).
-endif.
-endif.

-ifdef(NEED_escape_all_ascii_5).
escape_all_ascii(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Acc, Orig, Skip, Len)
  when ?are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8) ->
    escape_all_ascii(Rest, Acc, Orig, Skip, Len + 8);
escape_all_ascii(Binary, Acc, Orig, Skip, Len) -> escape_all(Binary, Acc, Orig, Skip, Len).

-ifndef(NEED_escape_all_5).
-define(NEED_escape_all_5, true).
-endif.
-endif.

-ifdef(NEED_escape_all_5).
escape_all(<<Byte, Rest/binary>>, Acc, Orig, Skip, Len) when ?is_ascii_plain(Byte) ->
    escape_all(Rest, Acc, Orig, Skip, Len + 1);
escape_all(<<Byte, Rest/bits>>, Acc, Orig, Skip, 0) when ?is_ascii_escape(Byte) ->
    escape_all(Rest, [Acc|escape(Byte)], Orig, Skip + 1, 0);
escape_all(<<Byte, Rest/bits>>, Acc, Orig, Skip, Len) when ?is_ascii_escape(Byte) ->
    escape_all(Rest, [Acc, binary_part(Orig, Skip, Len)|escape(Byte)], Orig, Skip + Len + 1, 0);
escape_all(<<Char/utf8, Rest/bits>>, Acc, Orig, Skip, 0) -> escape_char(Rest, Acc, Orig, Skip, Char);
escape_all(<<Char/utf8, Rest/bits>>, Acc, Orig, Skip, Len) ->
    escape_char(Rest, [Acc|binary_part(Orig, Skip, Len)], Orig, Skip + Len, Char);
escape_all(<<>>, _Acc, Orig, 0, _Len) -> [$", Orig, $"];
escape_all(<<>>, Acc, _Orig, _Skip, 0) -> [Acc, $"];
escape_all(<<>>, Acc, Orig, Skip, Len) -> [Acc, binary_part(Orig, Skip, Len), $"];
escape_all(_Other, _Acc, Orig, Skip, Len) -> invalid_byte(Orig, Skip + Len).

escape_char(<<Rest/bits>>, Acc, Orig, Skip, Char) when Char =< 16#FF ->
    escape_all(Rest, [Acc, "\\u00"|integer_to_binary(Char, 16)], Orig, Skip + 2, 0);
escape_char(<<Rest/bits>>, Acc, Orig, Skip, Char) when Char =< 16#7FF ->
    escape_all(Rest, [Acc, "\\u0"|integer_to_binary(Char, 16)], Orig, Skip + 2, 0);
escape_char(<<Rest/bits>>, Acc, Orig, Skip, Char) when Char =< 16#FFF ->
    escape_all(Rest, [Acc, "\\u0"|integer_to_binary(Char, 16)], Orig, Skip + 3, 0);
escape_char(<<Rest/bits>>, Acc, Orig, Skip, Char) when Char =< 16#FFFF ->
    escape_all(Rest, [Acc, "\\u"|integer_to_binary(Char, 16)], Orig, Skip + 3, 0);
escape_char(<<Rest/bits>>, Acc, Orig, Skip, Char) ->
    C = Char - 16#10000,
    escape_all(Rest,
               [Acc, "\\uD",
                integer_to_binary(16#800 bor (C bsr 10), 16), "\\uD"|integer_to_binary(16#C00 bor (C band 16#3FF), 16)],
               Orig, Skip + 4, 0).

-ifndef(NEED_escape_1).
-define(NEED_escape_1, true).
-endif.
-ifndef(NEED_invalid_byte_2).
-define(NEED_invalid_byte_2, true).
-endif.
-endif.

-ifdef(NEED_do_encode_2).
do_encode_list([], _Encode) -> <<"[]">>;
do_encode_list([First|Rest], Encode) when is_function(Encode, 2) -> [$[, Encode(First, Encode)|list_loop(Rest, Encode)].

list_loop([Elem|Rest], Encode) -> [$,, Encode(Elem, Encode)|list_loop(Rest, Encode)];
list_loop([], _Encode) -> "]".
-endif.

-ifdef(NEED_do_encode_map_2).
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 26).
-define(HAVE_MAP_COMPREHENTION, true).
-endif.
-endif.
-ifdef(HAVE_MAP_COMPREHENTION).
do_encode_map(Map, Encode) when is_function(Encode, 2) ->
    encode_object([[$,, key(Key, Encode), $:|Encode(Value, Encode)] || Key := Value <- Map]).
-else.
do_encode_map(Map, Encode) when is_function(Encode, 2) ->
    encode_object([[$,, key(Key, Encode), $:|Encode(Value, Encode)] || {Key, Value} <- maps:to_list(Map)]).
-endif.

-ifndef(NEED_encode_object_1).
-define(NEED_encode_object_1, true).
-endif.
-ifndef(NEED_key_2).
-define(NEED_key_2, true).
-endif.
-endif.

-ifdef(NEED_do_encode_checked_2).
do_encode_checked(List, Encode) when is_function(Encode, 2) -> encode_object(do_encode_checked(List, Encode, #{})).

do_encode_checked([{Key, Value}|Rest], Encode, Visited) ->
    EncodedKey = iolist_to_binary(key(Key, Encode)),
    maps:is_key(EncodedKey, Visited) andalso error({duplicate_key, Key}),
    [[$,, EncodedKey, $:|Encode(Value, Encode)]|do_encode_checked(Rest, Encode, Visited#{EncodedKey => true})];
do_encode_checked([], _, _) -> [].

-ifndef(NEED_key_2).
-define(NEED_key_2, true).
-endif.
-endif.

-ifdef(NEED_encode_object_1).
encode_object([]) -> <<"{}">>;
encode_object([[_Comma|Entry]|Rest]) -> ["{", Entry, Rest, "}"].
-endif.

-ifdef(NEED_key_2).
key(Key, Encode) when is_binary(Key) -> Encode(Key, Encode);
key(Key, Encode) when is_atom(Key) -> Encode(atom_to_binary(Key, utf8), Encode);
key(Key, _Encode) when is_integer(Key) -> [$", integer_to_binary(Key), $"];
key(Key, _Encode) when is_float(Key) -> [$", encode_float(Key), $"].
-endif.

-ifdef(NEED_escape_1).
escape($\x00) -> <<"\\u0000">>;
escape($\x01) -> <<"\\u0001">>;
escape($\x02) -> <<"\\u0002">>;
escape($\x03) -> <<"\\u0003">>;
escape($\x04) -> <<"\\u0004">>;
escape($\x05) -> <<"\\u0005">>;
escape($\x06) -> <<"\\u0006">>;
escape($\x07) -> <<"\\u0007">>;
escape($\b) -> <<"\\b">>;
escape($\t) -> <<"\\t">>;
escape($\n) -> <<"\\n">>;
escape($\x0b) -> <<"\\u000B">>;
escape($\f) -> <<"\\f">>;
escape($\r) -> <<"\\r">>;
escape($\x0e) -> <<"\\u000E">>;
escape($\x0f) -> <<"\\u000F">>;
escape($\x10) -> <<"\\u0010">>;
escape($\x11) -> <<"\\u0011">>;
escape($\x12) -> <<"\\u0012">>;
escape($\x13) -> <<"\\u0013">>;
escape($\x14) -> <<"\\u0014">>;
escape($\x15) -> <<"\\u0015">>;
escape($\x16) -> <<"\\u0016">>;
escape($\x17) -> <<"\\u0017">>;
escape($\x18) -> <<"\\u0018">>;
escape($\x19) -> <<"\\u0019">>;
escape($\x1A) -> <<"\\u001A">>;
escape($\x1B) -> <<"\\u001B">>;
escape($\x1C) -> <<"\\u001C">>;
escape($\x1D) -> <<"\\u001D">>;
escape($\x1E) -> <<"\\u001E">>;
escape($\x1F) -> <<"\\u001F">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(_) -> no.
-endif.

-ifdef(NEED_value_6).
-define(is_1_to_9(X),
        X =:= $1 orelse
        X =:= $2 orelse
        X =:= $3 orelse
        X =:= $4 orelse
        X =:= $5 orelse
        X =:= $6 orelse
        X =:= $7 orelse
        X =:= $8 orelse
        X =:= $9).
-define(is_0_to_9(X), X >= $0 andalso X =< $9).
-define(is_ws(X), X =:= $\s; X =:= $\t; X =:= $\r; X =:= $\n).
-define(ARRAY, array).
-define(OBJECT, object).

value(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode) when ?is_ws(Byte) ->
    value(Rest, Original, Skip + 1, Acc, Stack, Decode);
value(<<$0, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> number_zero(Rest, Original, Skip, Acc, Stack, Decode, 1);
value(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode) when ?is_1_to_9(Byte) ->
    number(Rest, Original, Skip, Acc, Stack, Decode, 1);
value(<<$-, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> number_minus(Rest, Original, Skip, Acc, Stack, Decode);
value(<<$t, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> true(Rest, Original, Skip, Acc, Stack, Decode);
value(<<$f, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> false(Rest, Original, Skip, Acc, Stack, Decode);
value(<<$n, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> null(Rest, Original, Skip, Acc, Stack, Decode);
value(<<$", Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> string(Rest, Original, Skip + 1, Acc, Stack, Decode);
value(<<$[, Rest/bits>>, Original, Skip, Acc, Stack, Decode) -> array_start(Rest, Original, Skip, Acc, Stack, Decode, 1);
value(<<${, Rest/bits>>, Original, Skip, Acc, Stack, Decode) ->
    object_start(Rest, Original, Skip, Acc, Stack, Decode, 1);
value(<<Byte, _/bits>>, Original, Skip, _Acc, _Stack, _Decode) when ?is_ascii_plain(Byte) ->
    invalid_byte(Original, Skip);
value(_, Original, Skip, Acc, Stack, Decode) -> unexpected(Original, Skip, Acc, Stack, Decode, 0, 0, value).

true(<<"rue", Rest/bits>>, Original, Skip, Acc, Stack, Decode) ->
    continue(Rest, Original, Skip + 4, Acc, Stack, Decode, true);
true(_Rest, Original, Skip, Acc, Stack, Decode) -> unexpected(Original, Skip, Acc, Stack, Decode, 1, 3, value).

false(<<"alse", Rest/bits>>, Original, Skip, Acc, Stack, Decode) ->
    continue(Rest, Original, Skip + 5, Acc, Stack, Decode, false);
false(_Rest, Original, Skip, Acc, Stack, Decode) -> unexpected(Original, Skip, Acc, Stack, Decode, 1, 4, value).

null(<<"ull", Rest/bits>>, Original, Skip, Acc, Stack, #decode{null = Null} = Decode) ->
    continue(Rest, Original, Skip + 4, Acc, Stack, Decode, Null);
null(_Rest, Original, Skip, Acc, Stack, Decode) -> unexpected(Original, Skip, Acc, Stack, Decode, 1, 3, value).

number_minus(<<$0, Rest/bits>>, Original, Skip, Acc, Stack, Decode) ->
    number_zero(Rest, Original, Skip, Acc, Stack, Decode, 2);
number_minus(<<Num, Rest/bits>>, Original, Skip, Acc, Stack, Decode) when ?is_1_to_9(Num) ->
    number(Rest, Original, Skip, Acc, Stack, Decode, 2);
number_minus(_Rest, Original, Skip, Acc, Stack, Decode) -> unexpected(Original, Skip, Acc, Stack, Decode, 1, 0, value).

number_zero(<<$., Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) ->
    number_frac(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_zero(<<E, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when E =:= $E; E =:= $e ->
    number_exp_copy(Rest, Original, Skip, Acc, Stack, Decode, Len + 1, <<"0">>);
number_zero(<<>>, Original, Skip, Acc, Stack, #decode{integer = Integer} = Decode, Len) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, {number, Integer(<<"0">>)});
number_zero(Rest, Original, Skip, Acc, Stack, #decode{integer = Integer} = Decode, Len) ->
    continue(Rest, Original, Skip + Len, Acc, Stack, Decode, Integer(<<"0">>)).

number(<<Num, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Num) ->
    number(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number(<<$., Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) ->
    number_frac(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number(<<E, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when E =:= $E; E =:= $e ->
    number_exp_copy(Rest, Original, Skip, Acc, Stack, Decode, Len + 1, binary_part(Original, Skip, Len));
number(<<>>, Original, Skip, Acc, Stack, #decode{integer = Fun} = Decode, Len) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, {number, Fun(binary_part(Original, Skip, Len))});
number(Rest, Original, Skip, Acc, Stack, #decode{integer = Fun} = Decode, Len) ->
    continue(Rest, Original, Skip + Len, Acc, Stack, Decode, Fun(binary_part(Original, Skip, Len))).

number_frac(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Byte) ->
    number_frac_cont(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_frac(_, Original, Skip, Acc, Stack, Decode, Len) -> unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value).

number_frac_cont(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Byte) ->
    number_frac_cont(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_frac_cont(<<E, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when E =:= $e; E =:= $E ->
    number_exp(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_frac_cont(Rest, Original, Skip, Acc, Stack, Decode, Len) ->
    float_decode(Rest, Original, Skip, Acc, Stack, Decode, Len, binary_part(Original, Skip, Len)).

float_decode(<<>>, Original, Skip, Acc, Stack, #decode{float = Fun} = Decode, Len, Token) ->
    try Fun(Token) of
        Float -> unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, {number, Float})
    catch
        _:_ -> unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, {float_error, Token, Skip})
    end;
float_decode(<<Rest/bits>>, Original, Skip, Acc, Stack, #decode{float = Fun} = Decode, Len, Token) ->
    try Fun(Token) of
        Float -> continue(Rest, Original, Skip + Len, Acc, Stack, Decode, Float)
    catch
        _:_ -> unexpected_sequence(Token)
    end.

number_exp(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_exp(<<Sign, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when Sign =:= $+; Sign =:= $- ->
    number_exp_sign(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_exp(_, Original, Skip, Acc, Stack, Decode, Len) -> unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value).

number_exp_sign(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_exp_sign(_, Original, Skip, Acc, Stack, Decode, Len) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value).

number_exp_cont(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len) ->
    float_decode(Rest, Original, Skip, Acc, Stack, Decode, Len, binary_part(Original, Skip, Len)).

number_exp_copy(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len, Prefix) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len, Prefix, 1);
number_exp_copy(<<Sign, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len, Prefix) when Sign =:= $+; Sign =:= $- ->
    number_exp_sign(Rest, Original, Skip, Acc, Stack, Decode, Len, Prefix, 1);
number_exp_copy(_, Original, Skip, Acc, Stack, Decode, Len, _Prefix) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value).

number_exp_sign(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len, Prefix, ExpLen) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len, Prefix, ExpLen + 1);
number_exp_sign(_, Original, Skip, Acc, Stack, Decode, Len, _Prefix, ExpLen) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len + ExpLen, 0, value).

number_exp_cont(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len, Prefix, ExpLen) when ?is_0_to_9(Byte) ->
    number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len, Prefix, ExpLen + 1);
number_exp_cont(Rest, Original, Skip, Acc, Stack, Decode, Len, Prefix, ExpLen) ->
    float_decode(Rest, Original, Skip, Acc, Stack, Decode, Len + ExpLen,
                 <<Prefix/binary, ".0e", (binary_part(Original, Skip + Len, ExpLen))/binary>>).

string(Binary, Original, Skip, Acc, Stack, Decode) -> string_ascii(Binary, Original, Skip, Acc, Stack, Decode, 0).

string_ascii(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Original, Skip, Acc, Stack, Decode, Len)
  when ?are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8) ->
    string_ascii(Rest, Original, Skip, Acc, Stack, Decode, Len + 8);
string_ascii(Binary, Original, Skip, Acc, Stack, Decode, Len) ->
    string(Binary, Original, Skip, Acc, Stack, Decode, Len).

string(<<Byte, Rest/bits>>, Orig, Skip, Acc, Stack, Decode, Len) when ?is_ascii_plain(Byte) ->
    string(Rest, Orig, Skip, Acc, Stack, Decode, Len + 1);
string(<<$\\, Rest/bits>>, Orig, Skip, Acc, Stack, Decode, Len) ->
    unescape(Rest, Orig, Skip, Acc, Stack, Decode, Skip - 1, Len, binary_part(Orig, Skip, Len));
string(<<$", Rest/bits>>, Orig, Skip, Acc, Stack, #decode{string = Fun} = Decode, Len) ->
    continue(Rest, Orig, Skip + Len + 1, Acc, Stack, Decode, Fun(binary_part(Orig, Skip, Len)));
string(<<Byte, _/bits>>, Orig, Skip, _Acc, _Stack, _Decode, Len) when ?is_ascii_escape(Byte) ->
    invalid_byte(Orig, Skip + Len);
string(<<Byte, Rest/bytes>>, Orig, Skip, Acc, Stack, Decode, Len) ->
    case element(Byte - 127, utf8s0()) of
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len);
        State -> string_utf8(Rest, Orig, Skip, Acc, Stack, Decode, Len, State)
    end;
string(_, Orig, Skip, Acc, Stack, Decode, Len) -> unexpected(Orig, Skip - 1, Acc, Stack, Decode, Len + 1, 0, value).

string_utf8(<<Byte, Rest/binary>>, Orig, Skip, Acc, Stack, Decode, Len, State0) ->
    case element(State0 + element(Byte + 1, utf8t()), utf8s()) of
        ?UTF8_ACCEPT -> string_ascii(Rest, Orig, Skip, Acc, Stack, Decode, Len + 2);
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len + 1);
        State -> string_utf8(Rest, Orig, Skip, Acc, Stack, Decode, Len + 1, State)
    end;
string_utf8(_, Orig, Skip, Acc, Stack, Decode, Len, _State0) ->
    unexpected(Orig, Skip-1, Acc, Stack, Decode, Len + 2, 0, value).

array_start(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_ws(Byte) ->
    array_start(Rest, Original, Skip, Acc, Stack, Decode, Len + 1);
array_start(<<"]", Rest/bits>>, Original, Skip, Acc, Stack,
            #decode{array_start = undefined, array_finish = undefined} = Decode, Len) ->
    continue(Rest, Original, Skip + Len + 1, Acc, Stack, Decode, []);
array_start(<<"]", Rest/bits>>, Original, Skip, Acc, Stack,
            #decode{array_start = Start, array_finish = undefined} = Decode, Len) ->
    continue(Rest, Original, Skip + Len + 1, Acc, Stack, Decode, lists:reverse(Start(Acc)));
array_start(<<"]", Rest/bits>>, Original, Skip, Acc, Stack,
            #decode{array_start = undefined, array_finish = Finish} = Decode, Len) ->
    {Value, NewAcc} = Finish([], Acc),
    continue(Rest, Original, Skip + Len + 1, NewAcc, Stack, Decode, Value);
array_start(<<"]", Rest/bits>>, Original, Skip, Acc, Stack,
            #decode{array_start = Start, array_finish = Finish} = Decode, Len) ->
    {Value, NewAcc} = Finish(Start(Acc), Acc),
    continue(Rest, Original, Skip + Len + 1, NewAcc, Stack, Decode, Value);
array_start(<<>>, Original, Skip, Acc, Stack, Decode, Len) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value);
array_start(Rest, Original, Skip, OldAcc, Stack, #decode{array_start = undefined} = Decode, Len) ->
    value(Rest, Original, Skip + Len, [], [?ARRAY, OldAcc|Stack], Decode);
array_start(Rest, Original, Skip, OldAcc, Stack, #decode{array_start = Fun} = Decode, Len) ->
    value(Rest, Original, Skip + Len, Fun(OldAcc), [?ARRAY, OldAcc|Stack], Decode).

object_start(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Len) when ?is_ws(Byte) ->
    object_start(Rest, Original, Skip, Acc, Stack, Decode, Len+1);
object_start(<<"}", Rest/bits>>, Original, Skip, Acc, Stack,
             #decode{object_start = undefined, object_finish = undefined} = Decode, Len) ->
    continue(Rest, Original, Skip + Len + 1, Acc, Stack, Decode, #{});
object_start(<<"}", Rest/bits>>, Original, Skip, Acc, Stack,
             #decode{object_start = Start, object_finish = undefined} = Decode, Len) ->
    continue(Rest, Original, Skip + Len + 1, Acc, Stack, Decode, maps:from_list(Start(Acc)));
object_start(<<"}", Rest/bits>>, Original, Skip, Acc, Stack,
             #decode{object_start = undefined, object_finish = Finish} = Decode, Len) ->
    {Value, NewAcc} = Finish([], Acc),
    continue(Rest, Original, Skip + Len + 1, NewAcc, Stack, Decode, Value);
object_start(<<"}", Rest/bits>>, Original, Skip, Acc, Stack,
             #decode{object_start = Start, object_finish = Finish} = Decode, Len) ->
    {Value, NewAcc} = Finish(Start(Acc), Acc),
    continue(Rest, Original, Skip + Len + 1, NewAcc, Stack, Decode, Value);
object_start(<<$", Rest/bits>>, Original, Skip, OldAcc, Stack, #decode{object_start = undefined} = Decode, Len) ->
    string(Rest, Original, Skip + Len + 1, [], [?OBJECT, OldAcc|Stack], Decode);
object_start(<<$", Rest/bits>>, Original, Skip, OldAcc, Stack, #decode{object_start = Fun} = Decode, Len) ->
    string(Rest, Original, Skip + Len + 1, Fun(OldAcc), [?OBJECT, OldAcc|Stack], Decode);
object_start(_, Original, Skip, Acc, Stack, Decode, Len) ->
    unexpected(Original, Skip, Acc, Stack, Decode, Len, 0, value).

unexpected(Original, Skip, Acc, Stack, Decode, Pos, Len, FuncData) ->
    case byte_size(Original) - Skip of
        Size when Size =< Pos + Len ->
            {continue, {binary_part(Original, Skip, Size), Acc, Stack, Decode, FuncData}};
        _ -> invalid_byte(Original, Skip + Pos)
    end.

continue(<<Rest/bits>>, Original, Skip, Acc, [], _Decode, Value) -> terminate(Rest, Original, Skip, Acc, Value);
continue(<<Rest/bits>>, Original, Skip, Acc, [?ARRAY|_] = Stack0, Decode, Value) ->
    array_push(Rest, Original, Skip, Acc, Stack0, Decode, Value);
continue(<<Rest/bits>>, Original, Skip, Acc, [?OBJECT|_] = Stack0, Decode, Value) ->
    object_value(Rest, Original, Skip, Acc, Stack0, Decode, Value);
continue(<<Rest/bits>>, Original, Skip, Acc, [Key|Stack], Decode, Value) ->
    object_push(Rest, Original, Skip, Acc, Stack, Decode, Value, Key).

unescape(<<$u, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    unescapeu(Rest, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc);
unescape(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    case unescape_ascii(Byte) of
        error -> invalid_byte(Original, Skip + Len + 1);
        Int -> string_ascii(Rest, Original, Skip + Len + 2, Acc, Stack, Decode, Start, 0, <<SAcc/binary, Int>>)
    end;
unescape(_, Original, Skip, Acc, Stack, Decode, Start, Len, _SAcc) ->
    unexpected(Original, Start, Acc, Stack, Decode, Len + 1 + Skip - Start, 0, value).

unescape_ascii($b) -> $\b;
unescape_ascii($f) -> $\f;
unescape_ascii($n) -> $\n;
unescape_ascii($r) -> $\r;
unescape_ascii($t) -> $\t;
unescape_ascii(B) when B =:= $"; B =:= $\\; B =:= $/ -> B;
unescape_ascii(_) -> error.

unescapeu(<<E1, E2, E3, E4, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    try hex_to_int(E1, E2, E3, E4) of
        CP when CP >= 16#D800, CP =< 16#DBFF ->
            unescape_surrogate(Rest, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc, CP);
        CP ->
            try <<SAcc/binary, CP/utf8>> of
                SAcc1 -> string_ascii(Rest, Original, Skip + Len + 6, Acc, Stack, Decode, Start, 0, SAcc1)
            catch
                _:_ -> unexpected_sequence(binary_part(Original, Skip + Len, 6))
            end
    catch
        _:_ -> unexpected_sequence(binary_part(Original, Skip + Len, 6))
    end;
unescapeu(_Rest, Original, Skip, Acc, Stack, Decode, Start, Len, _SAcc) ->
    unexpected(Original, Start, Acc, Stack, Decode, Len + 2 + Skip - Start, 4, value).

unescape_surrogate(<<"\\u", E1, E2, E3, E4, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc, Hi) ->
    try hex_to_int(E1, E2, E3, E4) of
        Lo when Lo >= 16#DC00, Lo =< 16#DFFF ->
            CP = 16#10000 + ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF),
            try <<SAcc/binary, CP/utf8>> of
                SAcc1 -> string_ascii(Rest, Original, Skip + Len + 12, Acc, Stack, Decode, Start, 0, SAcc1)
            catch
                _:_ -> unexpected_sequence(binary_part(Original, Skip + Len, 12))
            end;
        _ -> unexpected_sequence(binary_part(Original, Skip + Len, 12))
    catch
        _:_ -> unexpected_sequence(binary_part(Original, Skip + Len, 12))
    end;
unescape_surrogate(_Rest, Original, Skip, Acc, Stack, Decode, Start, Len, _SAcc, _Hi) ->
    unexpected(Original, Start, Acc, Stack, Decode, Len + 6 + Skip - Start, 5, value).

terminate(<<Byte, Rest/bits>>, Original, Skip, Acc, Value) when ?is_ws(Byte) ->
    terminate(Rest, Original, Skip, Acc, Value);
terminate(<<>>, _, _Skip, Acc, Value) -> {Value, Acc, <<>>};
terminate(<<_/bits>>, Original, Skip, Acc, Value) ->
    <<_:Skip/binary, Rest/binary>> = Original,
    {Value, Acc, Rest}.

array_push(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Value) when ?is_ws(Byte) ->
    array_push(Rest, Original, Skip + 1, Acc, Stack, Decode, Value);
array_push(<<"]", Rest/bits>>, Original, Skip, Acc0, [_, OldAcc|Stack],
           #decode{array_push = Push, array_finish = Finish} = Decode, Value) ->
    Acc = Push(Value, Acc0),
    {ArrayValue, NewAcc} = if
                               Finish =:= undefined -> {lists:reverse(Acc), OldAcc};
                               true ->  Finish(Acc, OldAcc)
                           end,
    continue(Rest, Original, Skip + 1, NewAcc, Stack, Decode, ArrayValue);
array_push(<<$,, Rest/bits>>, Original, Skip, Acc, Stack, #decode{array_push = Fun} = Decode, Value) ->
    value(Rest, Original, Skip + 1, Fun(Value, Acc), Stack, Decode);
array_push(_, Original, Skip, Acc, Stack, Decode, Value) ->
    unexpected(Original, Skip, Acc, Stack, Decode, 0, 0, {?FUNCTION_NAME, Value}).

object_value(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Key) when ?is_ws(Byte) ->
    object_value(Rest, Original, Skip + 1, Acc, Stack, Decode, Key);
object_value(<<$:, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Key) ->
    value(Rest, Original, Skip + 1, Acc, [Key|Stack], Decode);
object_value(_, Original, Skip, Acc, Stack, Decode, Key) ->
    unexpected(Original, Skip, Acc, Stack, Decode, 0, 0, {?FUNCTION_NAME, Key}).

object_push(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode, Value, Key) when ?is_ws(Byte) ->
    object_push(Rest, Original, Skip + 1, Acc, Stack, Decode, Value, Key);
object_push(<<"}", Rest/bits>>, Original, Skip, Acc0, [_, OldAcc|Stack],
            #decode{object_push = Push, object_finish = Finish} = Decode, Value, Key) ->
    Acc = Push(Key, Value, Acc0),
    {ObjectValue, NewAcc} = if
                                Finish =:= undefined -> {maps:from_list(Acc), OldAcc};
                                true -> Finish(Acc, OldAcc)
                            end,
    continue(Rest, Original, Skip + 1, NewAcc, Stack, Decode, ObjectValue);
object_push(<<$,, Rest/bits>>, Original, Skip, Acc0, Stack, #decode{object_push = Push} = Decode, Value, Key) ->
    object_key(Rest, Original, Skip + 1, Push(Key, Value, Acc0), Stack, Decode);
object_push(_, Original, Skip, Acc, Stack, Decode, Value, Key) ->
    unexpected(Original, Skip, Acc, Stack, Decode, 0, 0, {?FUNCTION_NAME, Value, Key}).

string_ascii(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc)
  when ?are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8) ->
    string_ascii(Rest, Original, Skip, Acc, Stack, Decode, Start, Len + 8, SAcc);
string_ascii(Binary, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    string(Binary, Original, Skip, Acc, Stack, Decode, Start, Len, SAcc).

-define(hex_digit(C),
        element(C - $0 + 1,
                {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, n, n, n, n, n, %% 0x30
                 n, n, 10,11,12,13,14,15,n, n, n, n, n, n, n, %% 0x40
                 n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, %% 0x50
                 n, n, n, n, 10,11,12,13,14,15})).            %% 0x60

hex_to_int(H1, H2, H3, H4) -> ?hex_digit(H4) + 16 * (?hex_digit(H3) + 16 * (?hex_digit(H2) + 16 * ?hex_digit(H1))).

object_key(<<Byte, Rest/bits>>, Original, Skip, Acc, Stack, Decode) when ?is_ws(Byte) ->
    object_key(Rest, Original, Skip + 1, Acc, Stack, Decode);
object_key(<<$", Rest/bits>>, Original, Skip, Acc, Stack, Decode) ->
    string(Rest, Original, Skip + 1, Acc, Stack, Decode);
object_key(_, Original, Skip, Acc, Stack, Decode) ->
    unexpected(Original, Skip, Acc, Stack, Decode, 0, 0, ?FUNCTION_NAME).

string(<<Byte, Rest/bits>>, Orig, Skip, Acc, Stack, Decode, Start, Len, SAcc) when ?is_ascii_plain(Byte) ->
    string(Rest, Orig, Skip, Acc, Stack, Decode, Start, Len + 1, SAcc);
string(<<$\\, Rest/bits>>, Orig, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    unescape(Rest, Orig, Skip, Acc, Stack, Decode, Start, Len, <<SAcc/binary, (binary_part(Orig, Skip, Len))/binary>>);
string(<<$", Rest/bits>>, Orig, Skip, Acc, Stack, #decode{string = Fun} = Decode, _Start, Len, SAcc) ->
    continue(Rest, Orig, Skip + Len + 1, Acc, Stack, Decode,
             Fun(<<SAcc/binary, (binary_part(Orig, Skip, Len))/binary>>));
string(<<Byte, _/bits>>, Orig, Skip, _Acc, _Stack, _Decode, _Start, Len, _SAcc) when ?is_ascii_escape(Byte) ->
    invalid_byte(Orig, Skip + Len);
string(<<Byte, Rest/bytes>>, Orig, Skip, Acc, Stack, Decode, Start, Len, SAcc) ->
    case element(Byte - 127, utf8s0()) of
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len);
        State -> string_utf8(Rest, Orig, Skip, Acc, Stack, Decode, Start, Len, SAcc, State)
    end;
string(_, Orig, Skip, Acc, Stack, Decode, Start, Len, _SAcc) ->
    unexpected(Orig, Start, Acc, Stack, Decode, Len + Skip - Start, 0, value).

string_utf8(<<Byte, Rest/binary>>, Orig, Skip, Acc, Stack, Decode, Start, Len, SAcc, State0) ->
    case element(State0 + element(Byte + 1, utf8t()), utf8s()) of
        ?UTF8_ACCEPT -> string_ascii(Rest, Orig, Skip, Acc, Stack, Decode, Start, Len + 2, SAcc);
        ?UTF8_REJECT -> invalid_byte(Orig, Skip + Len + 1);
        State -> string_utf8(Rest, Orig, Skip, Acc, Stack, Decode, Start, Len + 1, SAcc, State)
    end;
string_utf8(_, Orig, Skip, Acc, Stack, Decode, Start, Len, _SAcc, _State0) ->
    unexpected(Orig, Start, Acc, Stack, Decode, Len + 1 + Skip - Start, 0, value).

unexpected_sequence(Value) -> error({unexpected_sequence, Value}).

-ifndef(NEED_invalid_byte_2).
-define(NEED_invalid_byte_2, true).
-endif.
-ifndef(NEED_utf8s0_0).
-define(NEED_utf8s0_0, true).
-endif.
-ifndef(NEED_utf8t_0).
-define(NEED_utf8t_0, true).
-endif.
-ifndef(NEED_utf8s_0).
-define(NEED_utf8s_0, true).
-endif.
-endif.

-ifdef(NEED_parse_decoder_3).
parse_decoder(array_start, Fun, Decode) when is_function(Fun, 1) -> Decode#decode{array_start = Fun};
parse_decoder(array_push, Fun, Decode) when is_function(Fun, 2) -> Decode#decode{array_push = Fun};
parse_decoder(array_finish, Fun, Decode) when is_function(Fun, 2) -> Decode#decode{array_finish = Fun};
parse_decoder(object_start, Fun, Decode) when is_function(Fun, 1) -> Decode#decode{object_start = Fun};
parse_decoder(object_push, Fun, Decode) when is_function(Fun, 3) -> Decode#decode{object_push = Fun};
parse_decoder(object_finish, Fun, Decode) when is_function(Fun, 2) -> Decode#decode{object_finish = Fun};
parse_decoder(float, Fun, Decode) when is_function(Fun, 1) -> Decode#decode{float = Fun};
parse_decoder(integer, Fun, Decode) when is_function(Fun, 1) -> Decode#decode{integer = Fun};
parse_decoder(string, Fun, Decode) when is_function(Fun, 1) -> Decode#decode{string = Fun};
parse_decoder(null, Null, Decode) -> Decode#decode{null = Null}.
-endif.

-ifdef(NEED_invalid_byte_2).
invalid_byte(Bin, Skip) -> error({invalid_byte, binary:at(Bin, Skip)}).
-endif.

-ifdef(NEED_utf8t_0).
utf8t() ->
    { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
     10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8}.
-endif.

-ifdef(NEED_utf8s_0).
utf8s() ->
    {   12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
     12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
     12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
     12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
     12,36,12,12,12,12,12,12,12,12,12,12}.
-endif.

-ifdef(NEED_utf8s0_0).
utf8s0() ->
    {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
     12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
     12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
     12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
     12,12,24,24,24,24,24,24,24,24,24,24,24,24,24,24,
     24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,
     48,36,36,36,36,36,36,36,36,36,36,36,36,60,36,36,
     72,84,84,84,96,12,12,12,12,12,12,12,12,12,12,12}.
-endif.
