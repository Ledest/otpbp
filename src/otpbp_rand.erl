-module(otpbp_rand).

-ifndef(HAVE_rand__splitmix64_next_1).
% OTP 25.0
-export([splitmix64_next/1]).
-endif.
-ifndef(HAVE_rand__exsp_next_1).
% OTP 25.0
-export([exsp_next/1]).
-endif.
-ifndef(HAVE_rand__exsp_jump_1).
% OTP 25.0
-export([exsp_jump/1]).
-endif.
-ifndef(HAVE_rand__mwc59_1).
% OTP 25.0
-export([mwc59/1]).
-endif.
-ifndef(HAVE_rand__mwc59_value32_1).
% OTP 25.0
-export([mwc59_value32/1]).
-endif.
-ifndef(HAVE_rand__mwc59_value_1).
% OTP 25.0
-export([mwc59_value/1]).
-endif.
-ifndef(HAVE_rand__mwc59_float_1).
% OTP 25.0
-export([mwc59_float/1]).
-endif.
-ifndef(HAVE_rand__mwc59_seed_0).
% OTP 25.0
-export([mwc59_seed/0]).
-endif.
-ifndef(HAVE_rand__mwc59_seed_1).
% OTP 25.0
-export([mwc59_seed/1]).
-endif.
-ifndef(HAVE_rand__bytes_1).
% OTP 24.0
-export([bytes/1]).
-endif.
-ifndef(HAVE_rand__bytes_s_2).
% OTP 24.0
-export([bytes_s/2]).
-endif.

-ifndef(HAVE_rand__exsp_jump_1).
-ifdef(HAVE_rand__exsp_next_1).
-import(rand, [exsp_next/1]).
-endif.
-endif.
-ifndef(HAVE_rand__bytes_1).
-ifdef(HAVE_rand__bytes_s_2).
-import(rand, [bytes_s/2]).
-endif.
-endif.

-define(SEED_DICT, rand_seed).
-define(BIT(Bits), (1 bsl (Bits))).
-define(MASK(Bits), (?BIT(Bits) - 1)).
-define(MASK(Bits, X), ((X) band ?MASK(Bits))).
-define(BSL(Bits, X, N), (?MASK((Bits) - (N), (X)) bsl (N))).

-define(MWC59_XS1, 4).
-define(MWC59_XS2, 27).

-ifndef(HAVE_rand__bytes_1).
bytes(N) ->
    {Bytes, State} = bytes_s(N, seed_get()),
    put(?SEED_DICT, State),
    Bytes.

-define(DEFAULT_ALG_HANDLER, exrop).
-compile({inline, seed_get/0}).
seed_get() ->
    case get(?SEED_DICT) of
        undefined -> rand:seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.
-endif.

-ifndef(HAVE_rand__bytes_s_2).
bytes_s(N, {#{bits := Bits, next := Next} = AlgHandler, R}) when is_integer(N), 0 =< N ->
    bytes_r(N, AlgHandler, Next, R, Bits, maps:get(weak_low_bits, AlgHandler, 0));
bytes_s(N, {#{max := Mask, next := Next} = AlgHandler, R}) when is_integer(N), 0 =< N, ?MASK(58) =< Mask ->
    %% Old spec - assume 58 bits and 2 weak low bits
    %% giving 56 bits i.e precisely 7 bytes per generated number
    bytes_r(N, AlgHandler, Next, R, 58, 2).

-compile({inline, bytes_r/6}).
bytes_r(N, AlgHandler, Next, R, Bits, WeakLowBits) ->
    %% We use whole bytes from each generator word,
    %% GoodBytes: that number of bytes
    GoodBytes = (Bits - WeakLowBits) bsr 3,
    GoodBits = GoodBytes bsl 3,
    %% Shift: how many bits of each generator word to waste by shifting right - we use the bits from the big end
    bytes_r(N, AlgHandler, Next, R, <<>>, GoodBytes, GoodBits, Bits - GoodBits).

bytes_r(N0, AlgHandler, Next, R0, Bytes0, GoodBytes, GoodBits, Shift) when GoodBytes < N0 ->
    {V, R1} = Next(R0),
    bytes_r(N0 - GoodBytes, AlgHandler, Next, R1, <<Bytes0/binary, (V bsr Shift):GoodBits>>, GoodBytes, GoodBits, Shift);
bytes_r(N, AlgHandler, Next, R0, Bytes, _GoodBytes, GoodBits, _Shift) ->
    {V, R1} = Next(R0),
    Bits = N bsl 3,
    %% Use the big end bits
    {<<Bytes/binary, (V bsr (GoodBits - Bits)):Bits>>, {AlgHandler, R1}}.
-endif.

-ifndef(HAVE_rand__splitmix64_next_1).
splitmix64_next(X_0) ->
    X = ?MASK(64, X_0 + 16#9e3779b97f4a7c15),
    Z_0 = ?MASK(64, (X bxor (X bsr 30)) * 16#bf58476d1ce4e5b9),
    Z_1 = ?MASK(64, (Z_0 bxor (Z_0 bsr 27)) * 16#94d049bb133111eb),
    {?MASK(64, Z_1 bxor (Z_1 bsr 31)), X}.
-endif.

-ifndef(HAVE_rand__exsp_next_1).
-define(exs_next(S0, S1, S1_b),
        begin
        S1_b = ?MASK(58, S1) bxor ?BSL(58, S1, 24),
        S1_b bxor S0 bxor (S1_b bsr 11) bxor (S0 bsr 41)
        end).

exsp_next([S1|S0]) ->
    S0_1 = ?MASK(58, S0),
    NewS1 = ?exs_next(S0_1, S1, S1_b),
    {?MASK(58, S0_1 + NewS1), [S0_1|NewS1]}.
-endif.

-ifndef(HAVE_rand__exsp_jump_1).
-define(JUMPCONST1, 16#02f8ea6bc32c797).
-define(JUMPCONST2, 16#345d2a0f85f788c).
-define(JUMPELEMLEN, 58).

exsp_jump(S) ->
    {S1, AS1} = exsplus_jump(S, [0|0], ?JUMPCONST1, ?JUMPELEMLEN),
    {_,  AS2} = exsplus_jump(S1, AS1,  ?JUMPCONST2, ?JUMPELEMLEN),
    AS2.

-dialyzer({no_improper_lists, exsplus_jump/4}).
exsplus_jump(S, AS, _, 0) -> {S, AS};
exsplus_jump(S, [AS0|AS1], J, N) ->
    {_, NS} = exsp_next(S),
    exsplus_jump(NS,
                 case ?MASK(1, J) of
                     1 ->
                         [S0|S1] = S,
                         [AS0 bxor S0|AS1 bxor S1];
                     0 -> [AS0|AS1]
                 end,
                 J bsr 1,
                 N - 1).
-endif.

-ifndef(HAVE_rand__mwc59_1).
-define(MWC59_A, 16#7fa6502).
-define(MWC59_B, 32).
mwc59(CX0) ->
    CX = ?MASK(59, CX0),
    ?MWC59_A * ?MASK(?MWC59_B, CX) + CX bsr ?MWC59_B.
-endif.

-ifndef(HAVE_rand__mwc59_value32_1).
-define(MWC59_XS, 8).
mwc59_value32(CX1) ->
    CX = ?MASK(32, CX1),
    CX bxor ?BSL(32, CX, ?MWC59_XS).
-endif.

-ifndef(HAVE_rand__mwc59_value_1).
mwc59_value(CX1) ->
    CX = ?MASK(59, CX1),
    CX2 = CX bxor ?BSL(59, CX, ?MWC59_XS1),
    CX2 bxor ?BSL(59, CX2, ?MWC59_XS2).
-endif.

-ifndef(HAVE_rand__mwc59_float_1).
-define(TWO_POW_MINUS53, 1.11022302462515657e-16).
mwc59_float(CX1) ->
    CX = ?MASK(53, CX1),
    CX2 = CX bxor ?BSL(53, CX, ?MWC59_XS1),
    (CX2 bxor ?BSL(53, CX2, ?MWC59_XS2)) * ?TWO_POW_MINUS53.
-endif.

-ifndef(HAVE_rand__mwc59_seed_0).
mwc59_seed() ->
    {A1, A2, A3} = default_seed(),
    hash58(A1) bxor hash58(A2) bxor hash58(A3) + 1.

-compile({inline, default_seed/0}).
default_seed() -> {erlang:phash2([{node(), self()}]), erlang:system_time(), erlang:unique_integer()}.

-ifndef(NEED_hash58_1).
-define(NEED_hash58_1, true).
-endif.
-endif.

-ifndef(HAVE_rand__mwc59_seed_1).
mwc59_seed(S) when is_integer(S), 0 =< S, S =< ?MASK(58) -> hash58(S) + 1.

-ifndef(NEED_hash58_1).
-define(NEED_hash58_1, true).
-endif.
-endif.

-ifdef(NEED_hash58_1).
hash58(X) ->
    X0 = ?MASK(58, X),
    X1 = ?MASK(58, (X0 bxor (X0 bsr 29)) * 16#351afd7ed558ccd),
    X2 = ?MASK(58, (X1 bxor (X1 bsr 29)) * 16#0ceb9fe1a85ec53),
    X2 bxor (X2 bsr 29).
-endif.
