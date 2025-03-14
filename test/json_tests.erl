-module(json_tests).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE =:= 27).
-define(OTP_RELEASE_27, true).
-endif.
-endif.

-ifndef(OTP_RELEASE_27).
-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

encode_atom_test() ->
    ?assertEqual(<<"true">>, encode(true)),
    ?assertEqual(<<"false">>, encode(false)),
    ?assertEqual(<<"null">>, encode(null)),
    %?assertEqual(<<"\"☃a\""/utf8>>, encode('☃a')),
    ?assertEqual(<<"\"json\"">>, encode(json)).

encode_integer_test() ->
    ?assertEqual(<<"123">>, encode(123)),
    ?assertEqual(<<"-123">>, encode(-123)),
    ?assertEqual(<<"0">>, encode(0)).

encode_float_test() ->
    ?assertEqual(<<"0.0">>, encode(0.0)),
    ?assertEqual(<<"-0.1">>, encode(-0.1)),
    ?assertEqual(<<"99.99">>, encode(99.99)),
    ?assertEqual(<<"9.9e100">>, encode(9.9e100)),
    ?assertEqual(<<"9.9e-100">>, encode(9.9e-100)).

encode_binary_test() ->
    ?assertEqual(<<"\"hello world\"">>, encode(<<"hello world">>)),
    ?assertEqual(<<"\"hello\\nworld\"">>, encode(<<"hello\nworld">>)),
    ?assertEqual(<<"\"\\nhello\\nworld\\n\"">>, encode(<<"\nhello\nworld\n">>)),
    ?assertEqual(<<"\"\\\"\"">>, encode(<<"\"">>)),
    ?assertEqual(<<"\"\\\\\"">>, encode(<<"\\">>)),
    ?assertEqual(<<"\"\\b\"">>, encode(<<"\b">>)),
    ?assertEqual(<<"\"\\f\"">>, encode(<<"\f">>)),
    ?assertEqual(<<"\"\\n\"">>, encode(<<"\n">>)),
    ?assertEqual(<<"\"\\r\"">>, encode(<<"\r">>)),
    ?assertEqual(<<"\"\\t\"">>, encode(<<"\t">>)),
    ?assertEqual(<<"\"\\u0000\"">>, encode(<<"\0">>)),
    ?assertEqual(<<"\"\\u001F\"">>, encode(<<"\x1F">>)),
    ?assertEqual(<<"\"/\"">>, encode(<<"/">>)),
    ?assertEqual(<<"\"áéíóúàèìòùâêîôûãẽĩõũ\""/utf8>>, encode(<<"áéíóúàèìòùâêîôûãẽĩõũ"/utf8>>)),
    ?assertEqual(<<"\"☃a\""/utf8>>, encode(<<"☃a"/utf8>>)),
    ?assertError({unsupported_type, <<0:1>>}, encode(<<0:1>>)),
    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, encode(<<"\xa0\xa1">>)),
    ?assertEqual(<<"\"\xc3\xb1\"">>, encode(<<"\xc3\xb1">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xc3\x28">>)),
    ?assertEqual(<<"\"\xe2\x82\xa1\"">>, encode(<<"\xe2\x82\xa1">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xe2\x28\xa1">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xe2\x82\x28">>)),
    ?assertEqual(<<"\"\xf0\x90\x8c\xbc\"">>, encode(<<"\xf0\x90\x8c\xbc">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x28\x8c\xbc">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x90\x28\xbc">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x28\x8c\x28">>)),
    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, encode(<<"\xf8\xa1\xa1\xa1\xa1">>)),
    ?assertError({invalid_byte, $\xfc}, encode(<<"\xfc\xa1\xa1\xa1\xa1\xa1">>)),
    %% overlong
    ?assertError({invalid_byte, $\xc0}, encode(<<"\xc0\x80">>)),
    ?assertError({invalid_byte, $\x80}, encode(<<"\xe0\x80\x80">>)),
    ?assertError({invalid_byte, $\x80}, encode(<<"\xf0\x80\x80\x80">>)),
    %% surrogate halves
    ?assertError({invalid_byte, $\xa0}, encode(<<"\xed\xa0\x80">>)),
    ?assertError({invalid_byte, $\xaf}, encode(<<"\xed\xaf\xbf">>)),
    ?assertError({invalid_byte, $\xb0}, encode(<<"\xed\xb0\x80">>)),
    ?assertError({invalid_byte, $\xbf}, encode(<<"\xed\xbf\xbf">>)).

encode_escape_all_test() ->
    %?assertEqual(<<"\"\\u2603a\"">>, encode_escape_all('☃a')),
    ?assertEqual(<<"\"\\u2603a\"">>, encode_escape_all(<<"☃a"/utf8>>)),
    ?assertEqual(<<"\"\\uD834\\uDD1Eb\"">>, encode_escape_all(<<"𝄞b"/utf8>>)),
    %% because of how /utf8 works, we always report 1st byte as invalid
    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, encode_escape_all(<<"\xa0\xa1">>)),
    ?assertEqual(<<"\"\\u00F1\"">>, encode_escape_all(<<"\xc3\xb1">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\xc3}, encode_escape_all(<<"\xc3\x28">>)),
    ?assertEqual(<<"\"\\u20A1\"">>, encode_escape_all(<<"\xe2\x82\xa1">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\xe2}, encode_escape_all(<<"\xe2\x28\xa1">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\xe2}, encode_escape_all(<<"\xe2\x82\x28">>)),
    ?assertEqual(<<"\"\\uD800\\uDF3C\"">>, encode_escape_all(<<"\xf0\x90\x8c\xbc">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x28\x8c\xbc">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x90\x28\xbc">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x28\x8c\x28">>)),
    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, encode_escape_all(<<"\xf8\xa1\xa1\xa1\xa1">>)),
    ?assertError({invalid_byte, $\xfc}, encode_escape_all(<<"\xfc\xa1\xa1\xa1\xa1\xa1">>)),
    %% overlong
    ?assertError({invalid_byte, $\xc0}, encode_escape_all(<<"\xc0\x80">>)),
    ?assertError({invalid_byte, $\xe0}, encode_escape_all(<<"\xe0\x80\x80">>)),
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x80\x80\x80">>)),
    %% surrogate halves
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xa0\x80">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xaf\xbf">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xb0\x80">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xbf\xbf">>)).

encode_map_test() ->
    ?assertEqual(<<"{}">>, encode(#{})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{foo => bar})),
    ?assertEqual(<<"{\"42\":\"bar\"}">>, encode(#{42 => bar})),
    MultiKeyMap = #{<<"foo">> => <<"foo1">>, foo => <<"foo2">>},
    ?assertError({duplicate_key, <<"foo">>}, encode_checked(MultiKeyMap)),
    ?assertEqual(<<"{\"foo\":\"foo2\",\"foo\":\"foo1\"}">>, encode(MultiKeyMap)).

encode_list_test() ->
    ?assertEqual(<<"[]">>, encode([])),
    ?assertEqual(<<"[1,2,3]">>, encode([1, 2, 3])).

encode_proplist_test() ->
    ?assertError({unsupported_type, {a, 1}}, encode([{a, 1}])),
    ?assertEqual(<<"{\"a\":1}">>, encode_proplist([{a, 1}])),
    MultiKeyProplist = [{<<"foo">>, <<"foo1">>}, {foo, <<"foo2">>}],
    ?assertError({duplicate_key, foo}, encode_proplist_checked(MultiKeyProplist)),
    ?assertEqual(<<"{\"foo\":\"foo1\",\"foo\":\"foo2\"}">>, encode_proplist(MultiKeyProplist)).

encode(Term) -> iolist_to_binary(json:encode(Term)).

encode(Term, Encode) -> iolist_to_binary(json:encode(Term, Encode)).

encode_escape_all(Term) ->
    encode(Term,
           fun(Binary, _) when is_binary(Binary) -> json:encode_binary_escape_all(Binary);
              (Other, Encode) -> json:encode_value(Other, Encode)
           end).

encode_checked(Term) ->
    encode(Term,
           fun(Map, Encode) when is_map(Map) -> json:encode_map_checked(Map, Encode);
              (Other, Encode) -> json:encode_value(Other, Encode)
           end).

encode_proplist(Term) ->
    encode(Term,
           fun([{_, _}|_], Encode) -> json:encode_key_value_list(Term, Encode);
              (Other, Encode) -> json:encode_value(Other, Encode)
           end).

encode_proplist_checked(Term) ->
    encode(Term,
           fun([{_, _}|_], Encode) -> json:encode_key_value_list_checked(Term, Encode);
              (Other, Encode) -> json:encode_value(Other, Encode)
           end).

decode_atoms_test() ->
    ?assertEqual(true, decode(<<"true">>)),
    ?assertEqual(false, decode(<<"false">>)),
    ?assertEqual(null, decode(<<"null">>)).

decode_numbers_test() ->
    ?assertError(unexpected_end, decode(<<"-">>)),
    ?assertError({invalid_byte, $-}, decode(<<"--1">>)),
    ?assertError({invalid_byte, $1}, json:decode(<<"01">>)),
    ?assertError({invalid_byte, $.}, decode(<<".1">>)),
    ?assertError(unexpected_end, decode(<<"1.">>)),
    ?assertError(unexpected_end, decode(<<"1e">>)),
    ?assertError(unexpected_end, decode(<<"1.0e+">>)),
    ?assertError({unexpected_sequence, <<"1.0e999">>}, decode(<<"1e999">>)),
    ?assertEqual(0, decode(<<"0">>)),
    ?assertEqual(1, decode(<<"1">>)),
    ?assertEqual(0, decode(<<"-0">>)),
    ?assertEqual(-1, decode(<<"-1">>)),
    ?assertEqual(0.0, decode(<<"0.0">>)),
    ?assertEqual(-0.0, decode(<<"-0.0">>)),
    ?assertEqual(0.1, decode(<<"0.1">>)),
    ?assertEqual(-0.1, decode(<<"-0.1">>)),
    ?assertEqual(0.0, decode(<<"0e0">>)),
    ?assertEqual(0.0, decode(<<"0E0">>)),
    ?assertEqual(1.0, decode(<<"1e0">>)),
    ?assertEqual(1.0, decode(<<"1E0">>)),
    ?assertEqual(1.0, decode(<<"1.0e0">>)),
    ?assertEqual(1.0, decode(<<"1e+0">>)),
    ?assertEqual(1.0, decode(<<"1.0e+0">>)),
    ?assertEqual(0.1e1, decode(<<"0.1e1">>)),
    ?assertEqual(0.1e-1, decode(<<"0.1e-1">>)),
    ?assertEqual(99.99e99, decode(<<"99.99e99">>)),
    ?assertEqual(-99.99e-99, decode(<<"-99.99e-99">>)),
    ?assertEqual(123456789.123456789e123, decode(<<"123456789.123456789e123">>)).

decode_strings_test() ->
    ?assertError(unexpected_end, decode(<<"\"">>)),
    ?assertError(unexpected_end, decode(<<"\"\\\"">>)),
    ?assertError({invalid_byte, $k}, decode(<<"\"\\k\"">>)),
    ?assertError({invalid_byte, $\r}, decode(<<"\"a\r\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\x80\"">>)),
    ?assertError({invalid_byte, $\x1F}, decode(<<"\"\x1F\"">>)),
    ?assertError(unexpected_end, decode(<<"\"\\u2603\\\"">>)),
    ?assertError(unexpected_end, decode(<<"\"Here's a snowman for you: ☃. Good day!"/utf8>>)),
    ?assertError(unexpected_end, decode(<<"\"𝄞"/utf8>>)),
    ?assertError({unexpected_sequence, <<"\\ud8aa\\udcxx">>}, decode(<<"\"\\ud8aa\\udcxx\"">>)),
    ?assertError({unexpected_sequence, <<"\\ud8aa\\uda00">>}, decode(<<"\"\\ud8aa\\uda00\"">>)),
    ?assertError({unexpected_sequence, <<"\\uxxxx">>}, decode(<<"\"\\uxxxx\"">>)),
    ?assertEqual(<<"\"\\/\b\f\n\r\t">>, decode(<<"\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"">>)),
    ?assertEqual(<<"☃"/utf8>>, decode(<<"\"\\u2603\"">>)),
    ?assertEqual(<<"\x{2028}\x{2029}"/utf8>>, decode(<<"\"\\u2028\\u2029\"">>)),
    ?assertEqual(<<"𝄞"/utf8>>, decode(<<"\"\\uD834\\uDD1E\"">>)),
    ?assertEqual(<<"𝄞"/utf8>>, decode(<<"\"\\ud834\\udd1e\"">>)),
    ?assertEqual(<<"힙힙"/utf8>>, decode(<<"\"\\uD799\\uD799\"">>)),
    ?assertEqual(<<"✔"/utf8>>, decode(<<"\"✔\""/utf8>>)),
    %% test-case with & without \n to test copying & non-copying implementation
    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\xa0\xa1\"">>)),
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\\n\xa0\xa1\"">>)),
    ?assertEqual(<<"\xc3\xb1">>, decode(<<"\"\xc3\xb1\"">>)),
    ?assertEqual(<<"\n\xc3\xb1">>, decode(<<"\"\\n\xc3\xb1\"">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xc3\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xc3\x28\"">>)),
    ?assertEqual(<<"\xe2\x82\xa1">>, decode(<<"\"\xe2\x82\xa1\"">>)),
    ?assertEqual(<<"\n\xe2\x82\xa1">>, decode(<<"\"\\n\xe2\x82\xa1\"">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xe2\x28\xa1\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xe2\x28\xa1\"">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xe2\x82\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xe2\x82\x28\"">>)),
    ?assertEqual(<<"\xf0\x90\x8c\xbc">>, decode(<<"\"\xf0\x90\x8c\xbc\"">>)),
    ?assertEqual(<<"\n\xf0\x90\x8c\xbc">>, decode(<<"\"\\n\xf0\x90\x8c\xbc\"">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x28\x8c\xbc\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x28\x8c\xbc\"">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x90\x28\xbc\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x90\x28\xbc\"">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x28\x8c\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x28\x8c\x28\"">>)),
    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, decode(<<"\"\xf8\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xf8}, decode(<<"\"\\n\xf8\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xfc}, decode(<<"\"\xfc\xa1\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xfc}, decode(<<"\"\\n\xfc\xa1\xa1\xa1\xa1\xa1\"">>)),
    %% overlong
    ?assertError({invalid_byte, $\xc0}, decode(<<"\"\xc0\x80\"">>)),
    ?assertError({invalid_byte, $\xc0}, decode(<<"\"\\n\xc0\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\xe0\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\\n\xe0\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\xf0\x80\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\\n\xf0\x80\x80\x80\"">>)),
    %% surrogate halves
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\xed\xa0\x80\"">>)),
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\\n\xed\xa0\x80\"">>)),
    ?assertError({invalid_byte, $\xaf}, decode(<<"\"\xed\xaf\xbf\"">>)),
    ?assertError({invalid_byte, $\xaf}, decode(<<"\"\\n\xed\xaf\xbf\"">>)),
    ?assertError({invalid_byte, $\xb0}, decode(<<"\"\xed\xb0\x80\"">>)),
    ?assertError({invalid_byte, $\xb0}, decode(<<"\"\\n\xed\xb0\x80\"">>)),
    ?assertError({invalid_byte, $\xbf}, decode(<<"\"\xed\xbf\xbf\"">>)).

decode_arrays_test() ->
    ?assertError(unexpected_end, decode(<<"[">>)),
    ?assertError({invalid_byte, $,}, decode(<<"[,">>)),
    ?assertError({invalid_byte, $]}, decode(<<" ]">>)),
    ?assertError(unexpected_end, decode(<<"[1,">>)),
    ?assertEqual([], decode(<<"[]">>)),
    ?assertEqual([1, 2, 3], decode(<<"[1,2,3]">>)),
    ?assertEqual([<<"foo">>, <<"bar">>, <<"baz">>], decode(<<"[\"foo\", \"bar\", \"baz\"]">>)),
    ?assertEqual([#{<<"foo">> => <<"bar">>}], decode(<<"[{\"foo\":\"bar\"}]">>)).

decode_objects_test() ->
    ?assertError(unexpected_end, decode(<<"{">>)),
    ?assertError({invalid_byte, $,}, decode(<<"{,">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\"}">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\":}">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\":1,}">>)),
    ?assertEqual(#{}, decode(<<"{}">>)),
    ?assertEqual(#{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\": \"bar\"}">>)),
    ?assertEqual(#{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\" : \"bar\"}">>)),
    ?assertEqual(#{<<"foo">> => #{}}, decode(<<"{\"foo\":{}}">>)),
    ?assertEqual(#{<<"foo">> => []}, decode(<<"{\"foo\":[]}">>)),
    ?assertEqual(#{<<"foo">> => 1, <<"bar">> => 2}, decode(<<"{\"foo\":1,\"bar\":2}">>)).

decode_whitespace_test() ->
    ?assertError(unexpected_end, decode(<<"">>)),
    ?assertError(unexpected_end, decode(ews(<<" ">>))),
    ?assertEqual([], decode(ews(<<" [ ] ">>))),
    ?assertEqual(#{}, decode(ews(<<" { } ">>))),
    ?assertEqual([1, 2], decode(ews(<<" [ 1 , 2 ] ">>))),
    ?assertEqual(#{<<"foo">> => 1, <<"bar">> => 2}, decode(ews(<<" { \"foo\" : 1 , \"bar\" : 2 } ">>))).

decode(Bin) ->
    try json:decode(Bin) of
        Result ->
            {Res, [], <<>>} = byte_loop(Bin),
            ?assertEqual(Result, Res),
            Result
    catch Class:Reason:ST ->
            ?assertError(Reason, byte_loop(Bin)),
            erlang:raise(Class, Reason, ST)
    end.

ews(Str) -> unicode:characters_to_binary(string:replace(Str, <<" ">>, <<" \s\t\r\n">>, all)).

-define(is_ws(X), X =:= $\s; X =:= $\t; X =:= $\r; X =:= $\n).

byte_loop(Bin) ->
    {continue, State} = json:decode_start(<<>>, [], #{}),
    byte_loop(Bin, State, []).

byte_loop(<<Byte, Rest/binary>> = Orig, State0, Bytes) ->
    case json:decode_continue(<<Byte>>, State0) of
        {continue, State} -> byte_loop(Rest, State, [Byte|Bytes]);
        {Result, [], <<>>} ->
            %% trim to match the binary in return value
            {Result, [],
             case string:trim(Rest, leading) of
                 <<>> -> <<>>;
                 _ when ?is_ws(Byte) -> Orig;
                 _ -> Rest
             end}
    end;
byte_loop(<<>>, State, _Bytes) -> json:decode_continue(end_of_input, State).

decode_api_test() ->
    put(history, []),
    ?assertEqual({#{a => [[], #{}, true, false, nil, #{foo => baz}], b => [1, 2.0, three, 0]},
                  {[], 25},
                  <<>>},
                 json:decode(<<"{\"a\": [[], {}, true, false, null, {\"foo\": \"baz\"}], \"b\": [1, 2.0, \"three\", 0]}">>,
                             {[], 0},
                             #{array_start => set_history1(array_start, fun({_, Int}) -> {[], Int + 1} end),
                               array_push => set_history2(array_push, fun(Val, {Acc, Int}) -> {[Val|Acc], Int + 1} end),
                               array_finish => set_history2(array_finish,
                                                            fun({Acc, Int}, {OldAcc, _OldInt}) ->
                                                                {lists:reverse(Acc), {OldAcc, Int + 1}}
                                                            end),
                               object_start => set_history1(object_start, fun({_, Int}) -> {[], Int + 1} end),
                               object_push => set_history3(object_push,
                                                           fun(Key, Val, {Acc, Int}) -> {[{Key, Val}|Acc], Int + 1} end),
                               object_finish => set_history2(object_finish,
                                                             fun({Acc, Int}, {OldAcc, _OldInt}) ->
                                                                 {maps:from_list(Acc), {OldAcc, Int + 1}}
                                                             end),
                               float => set_history1(float, fun binary_to_float/1),
                               integer => set_history1(integer, fun binary_to_integer/1),
                               string => set_history1(string, fun binary_to_atom/1),
                               null => nil})),
    ?assertEqual([{object_start, {[], 0}, {[], 1}},
                  {string, <<"a">>, a},
                  {array_start, {[], 1}, {[], 2}},
                  {array_start, {[], 2}, {[], 3}},
                  {array_finish, {{[], 3}, {[], 2}}, {[], {[], 4}}},
                  {array_push, {[], {[], 4}}, {[[]], 5}},
                  {object_start, {[[]], 5}, {[], 6}},
                  {object_finish, {{[], 6}, {[[]], 5}}, {#{}, {[[]], 7}}},
                  {array_push, {#{}, {[[]], 7}}, {[#{}, []], 8}},
                  {array_push, {true, {[#{}, []], 8}}, {[true, #{}, []], 9}},
                  {array_push, {false, {[true, #{}, []], 9}}, {[false, true, #{}, []], 10}},
                  {array_push, {nil, {[false, true, #{}, []], 10}}, {[nil, false, true, #{}, []], 11}},
                  {object_start, {[nil, false, true, #{}, []], 11}, {[], 12}},
                  {string, <<"foo">>, foo},
                  {string, <<"baz">>, baz},
                  {object_push, {foo, baz, {[], 12}}, {[{foo, baz}], 13}},
                  {object_finish,
                   {{[{foo, baz}], 13}, {[nil, false, true, #{}, []], 11}},
                   {#{foo => baz}, {[nil, false, true, #{}, []], 14}}},
                  {array_push,
                   {#{foo => baz}, {[nil, false, true, #{}, []], 14}},
                   {[#{foo => baz}, nil, false, true, #{}, []], 15}},
                  {array_finish,
                   {{[#{foo => baz}, nil, false, true, #{}, []], 15}, {[], 1}},
                   {[[], #{}, true, false, nil, #{foo => baz}], {[], 16}}},
                  {object_push,
                   {a, [[], #{}, true, false, nil, #{foo => baz}], {[], 16}},
                   {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17}},
                  {string, <<"b">>, b},
                  {array_start, {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17}, {[], 18}},
                  {integer, <<"1">>, 1},
                  {array_push, {1, {[], 18}}, {[1], 19}},
                  {float, <<"2.0">>, 2.0},
                  {array_push, {2.0, {[1], 19}}, {[2.0, 1], 20}},
                  {string, <<"three">>, three},
                  {array_push, {three, {[2.0, 1], 20}}, {[three, 2.0, 1], 21}},
                  {integer, <<"0">>, 0},
                  {array_push, {0, {[three, 2.0, 1], 21}}, {[0, three, 2.0, 1], 22}},
                  {array_finish,
                   {{[0, three, 2.0, 1], 22}, {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17}},
                   {[1, 2.0, three, 0], {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 23}}},
                  {object_push,
                   {b, [1, 2.0, three, 0], {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 23}},
                   {[{b, [1, 2.0, three, 0]}, {a, [[], #{}, true, false, nil, #{foo => baz}]}], 24}},
                  {object_finish,
                   {{[{b, [1, 2.0, three, 0]}, {a, [[], #{}, true, false, nil, #{foo => baz}]}], 24}, {[], 0}},
                   {#{a => [[], #{}, true, false, nil, #{foo => baz}], b => [1, 2.0, three, 0]}, {[], 25}}}],
                 lists:reverse(get(history))).

set_history1(Ty, Fun) -> fun(Arg) -> set_history(Ty, Arg, Fun(Arg)) end.

set_history2(Ty, Fun) -> fun(Arg1, Arg2) -> set_history(Ty, {Arg1, Arg2}, Fun(Arg1, Arg2)) end.

set_history3(Ty, Fun) -> fun(Arg1, Arg2, Arg3) -> set_history(Ty, {Arg1, Arg2, Arg3}, Fun(Arg1, Arg2, Arg3)) end.

set_history(Ty, Acc, Res) ->
    put(history, [{Ty, Acc, Res}|get(history)]),
    Res.

decode_api_stream_test() ->
    Types = <<"{\"types\": [[], {}, true, false, null, {\"foo\": \"baz\"}],\n               "
              "\"numbers\": [1, -10, 0.0, -0.0, 2.0, -2.0, 31e2, 31e-2, 0.31e2, -0.31e2, 0.13e-2],\n               "
              "\"strings\": [\"three\", \"åäö\", \"mixed_Ω\"],\n               "
              "\"escaped\": [\"\\n\", \"\\u2603\", \"\\ud834\\uDD1E\", \"\\nÃ±\"]\n              }\n"/utf8>>,
    ?assertEqual(ok, stream_decode(Types)),
    R = json:decode(ews(<<" 12345 \"foo\" ">>), ok, #{}),
    ?assertMatch({12345, ok, <<" \s\t\r\n", _/binary>>}, R),
    {_, _, <<" \s\t\r\n", _/binary>> = B1} = R,
    ?assertEqual({<<"foo">>, ok, <<>>}, json:decode(B1, ok, #{})),
    list_to_integer(erlang:system_info(otp_release)) >= 20 andalso
        ?assertEqual(ok, multi_stream_decode(<<"12345 1.30 \"String1\" -0.31e2\n[\"an array\"]12345\n">>)).

stream_decode(Str) ->
    {R1, [], <<>>} = byte_loop(Str),
    case json:decode(Str) of
        R1 -> ok;
        R2 ->
            io:fwrite("~p ~p~n", [R1, R2]),
            error
    end.

multi_stream_decode(<<>>) -> ok;
multi_stream_decode(Strs) ->
    {R1, [], ContBin} = byte_loop(Strs),
    case json:decode(Strs, [], #{}) of
        {R1, [], ContBin} -> multi_stream_decode(ContBin);
        Other ->
            io:format("~p '~tp'~n~p~n", [R1, ContBin, Other]),
            error
    end.

format(Term) -> iolist_to_binary(json:format(Term)).
format(Term, Arg) -> iolist_to_binary(json:format(Term, Arg)).

format_list_test() ->
    ?assertEqual(<<"[]\n">>, format([])),
    List10 = <<"[1,2,3,4,5,6,7,8,9,10]\n">>,
    ?assertEqual(List10, format(lists:seq(1, 10))),
    ListWithLists = <<
    "[\n"
    "  [1,2],\n"
    "  [3,4]\n"
    "]\n"
    >>,
    ?assertEqual(ListWithLists, format([[1, 2], [3, 4]])),
    ListWithListWithList = <<
    "[\n"
    "  [\n"
    "    []\n"
    "  ],\n"
    "  [\n"
    "    [3,4]\n"
    "  ]\n"
    "]\n"
    >>,
    ?assertEqual(ListWithListWithList, format([[[]],[[3,4]]])),
    ListWithMap = <<
    "[\n"
    "  { \"key\": 1 }\n"
    "]\n"
    >>,
    ?assertEqual(ListWithMap, format([#{key => 1}])),
    ListList10 = <<
    "[\n"
    "    [1,2,3,4,5,\n"
    "        6,7,8,9,\n"
    "        10]\n"
    "]\n"
    >>,
    ?assertEqual(ListList10, format([lists:seq(1,10)], #{indent => 4, max => 14})),
    ListString = <<
    "[\n"
    "   \"foo\",\n"
    "   \"bar\",\n"
    "   \"baz\"\n"
    "]\n"
    >>,
    ?assertEqual(ListString, format([<<"foo">>, <<"bar">>, <<"baz">>], #{indent => 3})),
    ok.

format_proplist_test() ->
    Formatter = fun({kvlist, KVList}, Fun, State) -> json:format_key_value_list(KVList, Fun, State);
                   ({kvlist_checked, KVList}, Fun, State) -> json:format_key_value_list_checked(KVList, Fun, State);
                   (Other, Fun, State) -> json:format_value(Other, Fun, State)
                end,
    ?assertEqual(<<
                  "{\n"
                  "  \"a\": 1,\n"
                  "  \"b\": \"str\"\n"
                  "}\n"
                  >>,
                  format({kvlist, [{a, 1}, {b, <<"str">>}]}, Formatter)),
    ?assertEqual(<<
                  "{\n"
                  "  \"a\": 1,\n"
                  "  \"b\": \"str\"\n"
                  "}\n"
                  >>,
                  format({kvlist_checked, [{a, 1}, {b, <<"str">>}]}, Formatter)),
    ?assertEqual(<<
                  "{\n"
                  "  \"10\": 1.0,\n"
                  "  \"1.0\": 10,\n"
                  "  \"a\": \"αβ\",\n"
                  "  \"αβ\": \"a\"\n"
                  "}\n"
                  /utf8>>,
                 format({kvlist, [{10, 1.0}, {1.0, 10}, {a, <<"αβ"/utf8>>}, {<<"αβ"/utf8>>, a}]}, Formatter)),
    ?assertEqual(<<
                  "{\n"
                  "  \"10\": 1.0,\n"
                  "  \"1.0\": 10,\n"
                  "  \"a\": \"αβ\",\n"
                  "  \"αβ\": \"a\"\n"
                  "}\n"
                  /utf8>>,
                 format({kvlist_checked, [{10, 1.0}, {1.0, 10}, {a, <<"αβ"/utf8>>}, {<<"αβ"/utf8>>, a}]}, Formatter)),
    ?assertEqual(<<
                  "{\n"
                  "  \"a\": 1,\n"
                  "  \"b\": {\n"
                  "    \"aa\": 10,\n"
                  "    \"bb\": 20\n"
                  "  },\n"
                  "  \"c\": \"str\"\n"
                  "}\n"
                  >>,
                 format({kvlist, [{a, 1}, {b, {kvlist, [{aa, 10}, {bb, 20}]}}, {c, <<"str">>}]}, Formatter)),
    ?assertEqual(<<
                  "[{\n"
                  "    \"a1\": 1,\n"
                  "    \"b1\": [{\n"
                  "        \"a11\": 1,\n"
                  "        \"b11\": 2\n"
                  "      },{\n"
                  "        \"a12\": 3,\n"
                  "        \"b12\": 4\n"
                  "      }],\n"
                  "    \"c1\": \"str1\"\n"
                  "  },\n"
                  "  {\n"
                  "    \"a2\": 2,\n"
                  "    \"b2\": [{\n"
                  "        \"a21\": 5,\n"
                  "        \"b21\": 6\n"
                  "      },{\n"
                  "        \"a22\": 7,\n"
                  "        \"b22\": 8\n"
                  "      }],\n"
                  "    \"c2\": \"str2\"\n"
                  "  }]\n"
                  >>,
                 format([{kvlist,
                          [{a1, 1},
                           {b1, [{kvlist, [{a11, 1}, {b11, 2}]}, {kvlist, [{a12, 3}, {b12, 4}]}]},
                           {c1, <<"str1">>}]},
                         {kvlist,
                          [{a2, 2},
                           {b2, [{kvlist, [{a21, 5}, {b21, 6}]}, {kvlist, [{a22, 7}, {b22, 8}]}]},
                           {c2, <<"str2">>}]}],
                        Formatter)),
    ?assertEqual(<<
                  "{\n"
                  "  \"a\": 1,\n"
                  "  \"b\": {\n"
                  "    \"aa\": 10,\n"
                  "    \"bb\": 20\n"
                  "  },\n"
                  "  \"c\": \"str\"\n"
                  "}\n"
                  >>,
                 format({kvlist_checked, [{a, 1}, {b, {kvlist_checked, [{aa, 10}, {bb,20}]}}, {c, <<"str">>}]},
                        Formatter)),
    ?assertEqual(<<
                  "[{\n"
                  "    \"a1\": 1,\n"
                  "    \"b1\": [{\n"
                  "        \"a11\": 1,\n"
                  "        \"b11\": 2\n"
                  "      },{\n"
                  "        \"a12\": 3,\n"
                  "        \"b12\": 4\n"
                  "      }],\n"
                  "    \"c1\": \"str1\"\n"
                  "  },\n"
                  "  {\n"
                  "    \"a2\": 2,\n"
                  "    \"b2\": [{\n"
                  "        \"a21\": 5,\n"
                  "        \"b21\": 6\n"
                  "      },{\n"
                  "        \"a22\": 7,\n"
                  "        \"b22\": 8\n"
                  "      }],\n"
                  "    \"c2\": \"str2\"\n"
                  "  }]\n"
                  >>,
                 format([{kvlist_checked,
                          [{a1, 1},
                           {b1, [{kvlist_checked, [{a11, 1}, {b11, 2}]}, {kvlist_checked, [{a12, 3}, {b12, 4}]}]},
                           {c1, <<"str1">>}]},
                         {kvlist_checked,
                          [{a2, 2},
                           {b2, [{kvlist_checked, [{a21, 5}, {b21, 6}]}, {kvlist_checked, [{a22, 7}, {b22, 8}]}]},
                           {c2, <<"str2">>}]}],
                        Formatter)),
    ?assertError({duplicate_key, a}, format({kvlist_checked, [{a, 1}, {b, <<"str">>}, {a, 2}]}, Formatter)),
    %% on invalid input exact error is not specified
    ?assertError(_, format({kvlist, [{a, 1}, b]}, Formatter)),
    ?assertError(_, format({kvlist, x}, Formatter)),
    ?assertError(_, format({kvlist, [{#{}, 1}]}, Formatter)),
    ?assertError(_, format({kvlist_checked, [{a, 1}, b]}, Formatter)),
    ?assertError(_, format({kvlist_checked, x}, Formatter)),
    ?assertError(_, format({kvlist_checked, [{#{}, 1}]}, Formatter)),
    ok.

format_map_test() ->
    ?assertEqual(<<"{}\n">>, format(#{})),
    ?assertEqual(<<"{ \"key\": \"val\" }\n">>, format(#{key => val})),
    MapSingleMap = <<
    "{\n"
    "  \"key1\": { \"key3\": \"val3\" },\n"
    "  \"key2\": 42\n"
    "}\n"
    >>,
    ?assertEqual(MapSingleMap, format(#{key1 => #{key3 => val3}, key2 => 42})),
    MapNestedMap = <<
    "{\n"
    "  \"key1\": {\n"
    "    \"key3\": true,\n"
    "    \"key4\": {\n"
    "      \"deep1\": 4711,\n"
    "      \"deep2\": \"string\"\n"
    "    }\n"
    "  },\n"
    "  \"key2\": 42\n"
    "}\n"
    >>,
    ?assertEqual(MapNestedMap,
                 format(#{key1 => #{key3 => true, key4 => #{deep1 => 4711, deep2 => <<"string">>}}, key2 => 42})),
    MapIntList = <<
    "{\n"
    "  \"key1\": [1,2,3,4,5],\n"
    "  \"key2\": 42\n"
    "}\n"
    >>,
    ?assertEqual(MapIntList, format(#{key1 => lists:seq(1, 5), key2 => 42})),
    MapObjList = <<
    "{\n"
    "  \"key1\": [\n"
    "    {\n"
    "      \"key3\": true,\n"
    "      \"key4\": [1,2,3,4,5]\n"
    "    },\n"
    "    {\n"
    "      \"key3\": true,\n"
    "      \"key4\": [1,2,3,4,5]\n"
    "    }\n"
    "  ],\n"
    "  \"key2\": 42\n"
    "}\n"
    >>,
    ?assertEqual(MapObjList,
                 format(#{key1 => [#{key3 => true, key4 => lists:seq(1, 5)}, #{key3 => true, key4 => lists:seq(1, 5)}],
                          key2 => 42})),
    MapObjList2 = <<
    "{\n"
    " \"key1\": [\n"
    "  {\n"
    "   \"key3\": true,\n"
    "   \"key4\": [1,2,\n"
    "    3,4,5,6,7,8,\n"
    "    9,10]\n"
    "  },\n"
    "  {\n"
    "   \"key3\": true,\n"
    "   \"key_longer_name\": [\n"
    "    1,\n"
    "    2,\n"
    "    3\n"
    "   ]\n"
    "  }\n"
    " ],\n"
    " \"key2\": 42\n"
    "}\n"
    >>,
    ?assertEqual(MapObjList2,
                 format(#{key1 => [#{key3 => true, key4 => lists:seq(1, 10)},
                                   #{key3 => true, key_longer_name => lists:seq(1, 3)}],
                          key2 => 42},
                        #{indent => 1, max => 15})),
    ok.

-record(rec, {a,b,c}).

format_fun_test() ->
    All = #{types => [[], #{}, true, false, null, #{foo => <<"baz">>}],
            numbers => [1, -10, 0.0, -0.1, 2.0, -2.0],
            strings => [<<"three">>, <<"åäö"/utf8>>, <<"mixed_Ω"/utf8>>],
            user_data => #rec{a = 1, b = 2, c = 3}},
    Formatter = fun(#rec{a = A, b = B, c = C}, _Fun, _State) ->
                        encode_proplist([{type, rec}, {a, A}, {b, B}, {c, C}]);
                   (Other, Fun, State) -> json:format_value(Other, Fun, State)
                end,
    Formatted = <<
    "{\n"
    "  \"numbers\": [1,-10,0.0,-0.1,2.0,-2.0],\n"
    "  \"strings\": [\n"
    "    \"three\",\n"
    "    \"åäö\",\n"
    "    \"mixed_Ω\"\n"
    "  ],\n"
    "  \"types\": [\n"
    "    [],\n"
    "    {},\n"
    "    true,\n"
    "    false,\n"
    "    null,\n"
    "    { \"foo\": \"baz\" }\n"
    "  ],\n"
    "  \"user_data\": {\"type\":\"rec\",\"a\":1,\"b\":2,\"c\":3}\n"
    "}\n"
    /utf8>>,
    ?assertEqual(Formatted, format(All, Formatter)),
    ok.
-endif.
