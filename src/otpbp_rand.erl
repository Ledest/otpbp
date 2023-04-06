-module(otpbp_rand).

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

-ifndef(HAVE_rand__bytes_1).
bytes(N) ->
    {Bytes, State} = bytes_s(N, seed_get()),
    seed_put(State),
    Bytes.

-compile({inline, [seed_get/0, seed_put/1]}).

-define(DEFAULT_ALG_HANDLER, exrop).
-define(SEED_DICT, rand_seed).

seed_put(Seed) ->
    put(?SEED_DICT, Seed),
    Seed.

seed_get() ->
    case get(?SEED_DICT) of
        undefined -> rand:seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.
-endif.

-ifdef(NEED_rand__bytes_s_2).
-define(BIT(Bits), (1 bsl (Bits))).
-define(MASK(Bits), (?BIT(Bits) - 1)).

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
