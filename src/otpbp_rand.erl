-module(otpbp_rand).

-ifndef(HAVE_rand__normal_2).
% OTP 20.0
-export([normal/2]).
-endif.
-ifndef(HAVE_rand__normal_s_3).
% OTP 20.0
-export([normal_s/3]).
-endif.

-ifndef(HAVE_rand__uniform_real_0).
% OTP 21.0
-export([uniform_real/0]).
-endif.
-ifndef(HAVE_rand__uniform_real_s_1).
% OTP 21.0
-export([uniform_real_s/1]).
-endif.

-ifndef(HAVE_rand__normal_2).
-ifdef(HAVE_rand__normal_s_3).
-import(rand, [normal_s/3]).
-endif.
-endif.

-ifndef(HAVE_rand__uniform_real_0).
-ifdef(HAVE_rand__uniform_real_s_1).
-import(rand, [uniform_real_s/1]).
-endif.
-endif.

-ifndef(HAVE_rand__bytes_1).
-ifndef(HAVE_rand__bytes_s_2).
-ifndef(NEED_rand__bytes_s_2).
-define(NEED_rand__bytes_s_2, true).
-endif.
-endif.
% OTP 24.0
-export([bytes/1]).
-endif.
-ifndef(HAVE_rand__bytes_s_2).
-ifndef(NEED_rand__bytes_s_2).
-define(NEED_rand__bytes_s_2, true).
-endif.
% OTP 24.0
-export([bytes_s/2]).
-endif.

-define(SEED_DICT, rand_seed).
-define(BIT(Bits), (1 bsl (Bits))).
-define(MASK(Bits), (?BIT(Bits) - 1)).

-ifndef(HAVE_rand__normal_2).
normal(Mean, Variance) -> Mean + math:sqrt(Variance) * rand:normal().
-endif.

-ifndef(HAVE_rand__normal_s_3).
normal_s(Mean, Variance, State0) when Variance > 0 ->
    {X, State} = rand:normal_s(State0),
    {Mean + math:sqrt(Variance) * X, State}.
-endif.

-ifndef(HAVE_rand__bytes_1).
bytes(N) ->
    {Bytes, State} = bytes_s(N, seed_get()),
    seed_put(State),
    Bytes.

-ifndef(NEED_seed_get_0).
-define(NEED_seed_get_0, true).
-endif.
-ifndef(NEED_seed_put_1).
-define(NEED_seed_put_1, true).
-endif.
-endif.

-ifdef(NEED_rand__bytes_s_2).
bytes_s(N, {#{bits := Bits, next := Next} = AlgHandler, R}) when is_integer(N), 0 =< N ->
    bytes_r(N, AlgHandler, Next, R, Bits, maps:get(weak_low_bits, AlgHandler, 0));
bytes_s(N, {#{max := Mask, next := Next} = AlgHandler, R}) when is_integer(N), 0 =< N, ?MASK(58) =< Mask ->
    %% Old spec - assume 58 bits and 2 weak low bits
    %% giving 56 bits i.e precisely 7 bytes per generated number
    bytes_r(N, AlgHandler, Next, R, 58, 2).

-compile({inline, [bytes_r/6]}).

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

-ifndef(HAVE_rand__uniform_real_0).
uniform_real() ->
    {X, Seed} = uniform_real_s(seed_get()),
    seed_put(Seed),
    X.

-ifndef(NEED_seed_get_0).
-define(NEED_seed_get_0, true).
-endif.
-ifndef(NEED_seed_put_1).
-define(NEED_seed_put_1, true).
-endif.
-endif.

-ifndef(HAVE_rand__uniform_real_s_1).
-define(MASK(Bits, X), ((X) band ?MASK(Bits))).
-define(BC(V, N), bc((V), ?BIT((N) - 1), N)).

uniform_real_s({#{next := Next} = Alg, R0}) ->
    {V1, R1} = Next(R0),
    uniform_real_s(Alg, V1, R1, Next).

-compile({inline, [uniform_real_s/5]}).

uniform_real_s(#{bits := Bits} = Alg, V1, R1, Next) ->
    case V1 bsr (Bits - 56) of
        M1 when ?BIT(55) =< M1 -> %% We have 56 bits - waste 3
            {(M1 bsr 3) * math:pow(2.0, -53), {Alg, R1}};
        M1 when ?BIT(54) =< M1 -> %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {Alg, R1}};
        M1 when ?BIT(53) =< M1 -> %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {Alg, R1}};
        M1 when ?BIT(52) =< M1 -> %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {Alg, R1}};
        M1 -> %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(Alg, Next, M1, -56, R2, V2, Bits)
    end;
uniform_real_s(#{max := _} = Alg, V1, R1, Next) ->
    case ?MASK(56, V1) of
        M1 when ?BIT(55) =< M1 -> %% We have 56 bits - waste 3
            {(M1 bsr 3) * math:pow(2.0, -53), {Alg, R1}};
        M1 when ?BIT(54) =< M1 -> %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {Alg, R1}};
        M1 when ?BIT(53) =< M1 -> %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {Alg, R1}};
        M1 when ?BIT(52) =< M1 -> %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {Alg, R1}};
        M1 -> %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(Alg, Next, M1, -56, R2, V2, 56)
    end.

uniform_real_s(Alg, _Next, M0, -1064, R1, V1, Bits) -> % 19*56
    B0 = (53 - ?BC(M0, 52)), % Missing bits
    {(((M0 bsl B0) bor (V1 bsr (Bits - B0))) * math:pow(2.0, -1064 - B0)), {Alg, R1}};
uniform_real_s(Alg, _Next, M0, BitNo, R1, V1, Bits) when ?BIT(51) =< M0 ->
    {(((M0 bsl 1) bor (V1 bsr (Bits - 1))) * math:pow(2.0, BitNo - 1)), {Alg, R1}};
uniform_real_s(Alg, _Next, M0, BitNo, R1, V1, Bits) when ?BIT(50) =< M0 ->
    {(((M0 bsl 2) bor (V1 bsr (Bits - 2))) * math:pow(2.0, BitNo - 2)), {Alg, R1}};
uniform_real_s(Alg, _Next, M0, BitNo, R1, V1, Bits) when ?BIT(49) =< M0 ->
    {(((M0 bsl 3) bor (V1 bsr (Bits - 3))) * math:pow(2.0, BitNo - 3)), {Alg, R1}};
uniform_real_s(Alg, Next, 0, BitNo, R1, V1, Bits) ->
    case V1 bsr (Bits - 56) of
        M1 when ?BIT(55) =< M1 -> %% We have 56 bits - waste 3
            {(M1 bsr 3) * math:pow(2.0, BitNo - 53), {Alg, R1}};
        M1 when ?BIT(54) =< M1 -> %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, BitNo - 54), {Alg, R1}};
        M1 when ?BIT(53) =< M1 -> %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, BitNo - 55), {Alg, R1}};
        M1 when ?BIT(52) =< M1 -> %% We have 53 bits - use all
            {M1 * math:pow(2.0, BitNo - 56), {Alg, R1}};
        M1 when BitNo =/= -1008; ?BIT(42) =< M1 -> %% Need more bits; We have 43 bits - get the last bits
            uniform_real_s(Alg, Next, M1, BitNo - 56, R1);
        _ -> uniform_real_s({Alg, R1})
    end;
uniform_real_s(Alg, _Next, M0, BitNo, R1, V1, Bits) ->
    %% Fill up to 53 bits
    B0 = 53 - ?BC(M0, 49), % Number of bits we need to append
    {(((M0 bsl B0) bor (V1 bsr (Bits - B0))) * math:pow(2.0, BitNo - B0)), {Alg, R1}}.

uniform_real_s(Alg, Next, M0, BitNo, R0) ->
    {V1, R1} = Next(R0),
    case Alg of
        #{bits := Bits} -> uniform_real_s(Alg, Next, M0, BitNo, R1, V1, Bits);
        #{max := _} -> uniform_real_s(Alg, Next, M0, BitNo, R1, ?MASK(56, V1), 56)
    end.

bc(V, B, N) when B =< V -> N;
bc(V, B, N) -> bc(V, B bsr 1, N - 1).
-endif.

-ifdef(NEED_seed_get_0).
-define(DEFAULT_ALG_HANDLER, exrop).
seed_get() ->
    case get(?SEED_DICT) of
        undefined -> rand:seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.
-endif.

-ifdef(NEED_seed_put_1).
seed_put(Seed) ->
    put(?SEED_DICT, Seed),
    Seed.
-endif.
