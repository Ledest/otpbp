-module(erlang_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

-if(?OTP_RELEASE >= 23).
-define(OTP_RELEASE_23, true).
-else.
-compile({nowarn_deprecated_function, {erlang, get_stacktrace, 0}}).
-endif.

-ifndef(OTP_RELEASE_23).
stacktrace_test() ->
    ?assertMatch({error, test, S1, S1}, stacktrace(error, test)),
    ?assertMatch({throw, test, S1, S1}, stacktrace(throw, test)),
    ?assertMatch({exit, test, S1, S1}, stacktrace(exit, test)).

stacktrace(C, R) ->
    try
        erlang:C(R)
    catch
        C:R:S -> {C, R, S, erlang:get_stacktrace()}
    end.
-endif.

integer_signed_width_test() ->
    ?assertEqual(1, integer_signed_width(0)),
    ?assertEqual(2, integer_signed_width(1)),
    ?assertEqual(3, integer_signed_width(2)),
    ?assertEqual(3, integer_signed_width(3)),
    ?assertEqual(4, integer_signed_width(4)),
    ?assertEqual(4, integer_signed_width(7)),
    ?assertEqual(5, integer_signed_width(8)),
    ?assertEqual(5, integer_signed_width(15)),
    ?assertEqual(8, integer_signed_width(127)),
    ?assertEqual(9, integer_signed_width(128)),
    ?assertEqual(9, integer_signed_width(255)),
    ?assertEqual(1, integer_signed_width(-1)),
    ?assertEqual(2, integer_signed_width(-2)),
    ?assertEqual(3, integer_signed_width(-3)),
    ?assertEqual(3, integer_signed_width(-4)),
    ?assertEqual(4, integer_signed_width(-5)),
    ?assertEqual(4, integer_signed_width(-7)),
    ?assertEqual(4, integer_signed_width(-8)),
    ?assertEqual(5, integer_signed_width(-9)),
    ?assertEqual(8, integer_signed_width(-127)),
    ?assertEqual(8, integer_signed_width(-128)),
    ?assertEqual(9, integer_signed_width(-129)),
    ?assertEqual(9, integer_signed_width(-256)),
    ?assertEqual(10, integer_signed_width(-257)),
    ?assertEqual(33, integer_signed_width(1 bsl 32 - 1)),
    ?assertEqual(32, integer_signed_width(-(1 bsl 31))),
    ?assertEqual(64, integer_signed_width(1 bsl 62)),
    ?assertEqual(63, integer_signed_width(-(1 bsl 62))),
    lists:foreach(fun(P) ->
                      I = 1 bsl P,
                      ?assertEqual(P, integer_signed_width(I - 1) - 1),
                      ?assertEqual(P, integer_signed_width(I) - 2),
                      ?assertEqual(P, integer_signed_width(-(I + 1)) - 2),
                      ?assertEqual(P, integer_signed_width(-I) - 1),
                      ?assertEqual(P, integer_signed_width(-(I - 1)) - 1)
                  end,
                  [27, 28, 29, 32, 48, 58, 59, 60, 62, 63]),
    BigNum64 = 1 bsl 64,
    ?assertEqual(66, integer_signed_width(BigNum64)),
    BigNum100 = 1 bsl 100,
    ?assertEqual(102, integer_signed_width(BigNum100)),
    BigNum128 = 1 bsl 128,
    ?assertEqual(130, integer_signed_width(BigNum128)),
    %% Negated powers of two are the extreme value of their width, so they do not need an extra bit
    ?assertEqual(65, integer_signed_width(-BigNum64)),
    ?assertEqual(101, integer_signed_width(-BigNum100)),
    ?assertEqual(129, integer_signed_width(-BigNum128)),
    %% One step beyond the extreme value does need an extra bit
    ?assertEqual(66, integer_signed_width(-BigNum64 - 1)),
    ?assertEqual(102, integer_signed_width(-BigNum100 - 1)),
    %% Multi-limb values with more than one set bit
    BigMixed = (1 bsl 70) + (1 bsl 5),
    ?assertEqual(72, integer_signed_width(BigMixed)),
    ?assertEqual(72, integer_signed_width(-BigMixed)),
    BigSpan = (1 bsl 192) + (1 bsl 130) + (1 bsl 5),
    ?assertEqual(194, integer_signed_width(BigSpan)),
    ?assertEqual(194, integer_signed_width(-BigSpan)),
    %% All-ones bignums are never powers of two, so both signs need N+1 bits
    AllOnes64 = (1 bsl 64) - 1,
    ?assertEqual(65, integer_signed_width(AllOnes64)),
    ?assertEqual(65, integer_signed_width(-AllOnes64)),
    AllOnes100 = (1 bsl 100) - 1,
    ?assertEqual(101, integer_signed_width(AllOnes100)),
    ?assertEqual(101, integer_signed_width(-AllOnes100)),
    AllOnes200 = (1 bsl 200) - 1,
    ?assertEqual(201, integer_signed_width(AllOnes200)),
    ?assertEqual(201, integer_signed_width(-AllOnes200)),
    lists:foreach(fun(A) -> ?assertError(badarg, integer_signed_width(A)) end, [3.14, -3.14, foo, [1, 2, 3], {}]),
    ok.

integer_unsigned_width_test() ->
    ?assertEqual(0, integer_unsigned_width(0)),
    ?assertEqual(1, integer_unsigned_width(1)),
    ?assertEqual(2, integer_unsigned_width(2)),
    ?assertEqual(2, integer_unsigned_width(3)),
    ?assertEqual(3, integer_unsigned_width(4)),
    ?assertEqual(3, integer_unsigned_width(7)),
    ?assertEqual(4, integer_unsigned_width(8)),
    ?assertEqual(4, integer_unsigned_width(15)),
    ?assertEqual(5, integer_unsigned_width(16)),
    ?assertEqual(8, integer_unsigned_width(255)),
    ?assertEqual(9, integer_unsigned_width(256)),
    %% Negative numbers have no unsigned representation
    lists:foreach(fun(A) -> ?assertError(badarg, integer_unsigned_width(A)) end, [-1, -8 -100]),
    lists:foreach(fun(P) ->
                      I = 1 bsl P,
                      ?assertEqual(P, integer_unsigned_width(I - 1)),
                      ?assertEqual(P, integer_unsigned_width(I) - 1)
                  end,
                  [27, 28, 29, 32, 48, 58, 59, 60, 62, 63]),
    ?assertEqual(65, integer_unsigned_width(1 bsl 64)),
    BigNum100 = 1 bsl 100,
    ?assertEqual(101, integer_unsigned_width(BigNum100)),
    ?assertEqual(129, integer_unsigned_width(1 bsl 128)),
    %% Only the most significant bit decides the width; lower limbs are irrelevant
    ?assertEqual(71, integer_unsigned_width((1 bsl 70) + (1 bsl 5))),
    ?assertEqual(193, integer_unsigned_width((1 bsl 192) + (1 bsl 130) + (1 bsl 5))),
    %% All-ones bignums: N bits set means a width of N
    ?assertEqual(64, integer_unsigned_width((1 bsl 64) - 1)),
    ?assertEqual(100, integer_unsigned_width((1 bsl 100) - 1)),
    AllOnes200 = (1 bsl 200) - 1,
    ?assertEqual(200, integer_unsigned_width(AllOnes200)),
    %% Negative bignums are rejected just like negative smalls
    lists:foreach(fun(A) -> ?assertError(badarg, integer_unsigned_width(A)) end, [-BigNum100, -AllOnes200]),
    lists:foreach(fun(A) -> ?assertError(badarg, integer_unsigned_width(A)) end, [3.14, atom, [1, 2, 3], {}]),
    ok.

integer_trailing_zeroes_test() ->
    ?assertEqual(0, integer_trailing_zeroes(1)),
    ?assertEqual(1, integer_trailing_zeroes(2)),
    ?assertEqual(0, integer_trailing_zeroes(3)),
    ?assertEqual(2, integer_trailing_zeroes(4)),
    ?assertEqual(3, integer_trailing_zeroes(8)),
    ?assertEqual(4, integer_trailing_zeroes(16)),
    ?assertError(badarg, integer_trailing_zeroes(0)),
    ?assertEqual(32, integer_trailing_zeroes(1 bsl 32)),
    ?assertEqual(48, integer_trailing_zeroes(1 bsl 48)),
    ?assertEqual(62, integer_trailing_zeroes(1 bsl 62)),
    ?assertEqual(63, integer_trailing_zeroes(1 bsl 63)),
    lists:foreach(fun(P) ->
                      I = 1 bsl P,
                      ?assertEqual(0, integer_trailing_zeroes(I - 1)),
                      ?assertEqual(P, integer_trailing_zeroes(I))
                  end,
                  [27, 28, 29, 32, 48, 58, 59, 60, 62, 63]),
    ?assertEqual(64, integer_trailing_zeroes(1 bsl 64)),
    ?assertEqual(100, integer_trailing_zeroes(1 bsl 100)),
    ?assertEqual(128, integer_trailing_zeroes(1 bsl 128)),
    %% multi-limb bignum with low set bit in a lower limb
    ?assertEqual(5, integer_trailing_zeroes((1 bsl 70) + (1 bsl 5))),
    %% bignum with trailing zeros spanning multiple limbs
    ?assertEqual(5, integer_trailing_zeroes((1 bsl 192) + (1 bsl 130) + (1 bsl 5))),
    %% All-ones bignums: bit 0 is always set, so integer_trailing_zeroes = 0
    ?assertEqual(0, integer_trailing_zeroes((1 bsl 64) - 1)),
    ?assertEqual(0, integer_trailing_zeroes((1 bsl 100) - 1)),
    ?assertEqual(0, integer_trailing_zeroes((1 bsl 200) - 1)),
    lists:foreach(fun(A) -> ?assertError(badarg, integer_trailing_zeroes(A)) end, [3.14, atom, [1, 2, 3], {}]),
    ok.

integer_set_bits_test() ->
    ?assertEqual(0, integer_set_bits(0)),
    ?assertEqual(1, integer_set_bits(1)),
    ?assertEqual(1, integer_set_bits(2)),
    ?assertEqual(2, integer_set_bits(3)),
    ?assertEqual(1, integer_set_bits(4)),
    ?assertEqual(2, integer_set_bits(5)),
    ?assertEqual(2, integer_set_bits(6)),
    ?assertEqual(3, integer_set_bits(7)),
    ?assertEqual(1, integer_set_bits(8)),
    ?assertEqual(4, integer_set_bits(15)),
    lists:foreach(fun(_) ->
                      N = rand:uniform(1 bsl 59),
                      ?assertEqual(count_bits(N), integer_set_bits(N))
                  end,
                  lists:seq(1, 10)),
    lists:foreach(fun(A) -> ?assertError(badarg, integer_set_bits(A)) end, [-1, -7, -128]),
    lists:foreach(fun(P) ->
                      I = 1 bsl P,
                      ?assertEqual(P, integer_set_bits(I - 1)),
                      ?assertEqual(1, integer_set_bits(I))
                  end,
                  [27, 28, 29, 32, 48, 58, 59, 60, 62, 63]),
    ?assertEqual(128, integer_set_bits((1 bsl 128) - 1)),
    ?assertEqual(1, integer_set_bits(1 bsl 128)),
    ?assertEqual(4, integer_set_bits((1 bsl 96) + (1 bsl 65) + (1 bsl 32) + 1)),
    %% Larger multi-limb all-ones
    ?assertEqual(192, integer_set_bits((1 bsl 192) - 1)),
    %% Scattered bits across many limbs
    ?assertEqual(4, integer_set_bits((1 bsl 200) + (1 bsl 150) + (1 bsl 70) + (1 bsl 1))),
    %% All-ones bignums: all bits up to N are set, so integer_set_bits = N
    ?assertEqual(64, integer_set_bits((1 bsl 64) - 1)),
    ?assertEqual(100, integer_set_bits((1 bsl 100) - 1)),
    ?assertEqual(200, integer_set_bits((1 bsl 200) - 1)),
    lists:foreach(fun(_) ->
                      N = rand:uniform(1 bsl 60) + (1 bsl 60),
                      ?assertEqual(count_bits(N), integer_set_bits(N))
                  end,
                  lists:seq(1, 10)),
    lists:foreach(fun(A) -> ?assertError(badarg, integer_unsigned_width(A)) end, [3.14, foo, ok, [1, 2, 3]]),
    ok.

count_bits(N) -> count_bits(N, 0).

count_bits(0, Bits) -> Bits;
count_bits(N, Bits) when N > 0 -> count_bits(N band (N - 1), Bits + 1).
