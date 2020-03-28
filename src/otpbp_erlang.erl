-module(otpbp_erlang).

-ifndef(HAVE_erlang__get_keys_0).
% OTP 18.0
-export([get_keys/0]).
-endif.
-ifndef(HAVE_erlang__convert_time_unit_3).
% OTP 18.0
-export([convert_time_unit/3]).
-endif.
-ifndef(HAVE_erlang__system_time_0).
% OTP 18.0
-export([system_time/0]).
-endif.
-ifndef(HAVE_erlang__system_time_1).
% OTP 18.0
-export([system_time/1]).
-endif.
-ifndef(HAVE_erlang__monotonic_time_0).
% OTP 18.0
-export([monotonic_time/0]).
-endif.
-ifndef(HAVE_erlang__monotonic_time_1).
% OTP 18.0
-export([monotonic_time/1]).
-endif.
-ifndef(HAVE_erlang__time_offset_0).
% OTP 18.0
-export([time_offset/0]).
-endif.
-ifndef(HAVE_erlang__time_offset_1).
% OTP 18.0
-export([time_offset/1]).
-endif.
-ifndef(HAVE_erlang__unique_integer_0).
% OTP 18.0
-export([unique_integer/0]).
-endif.
-ifndef(HAVE_erlang__unique_integer_1).
% OTP 18.0
-export([unique_integer/1]).
-endif.
-ifndef(HAVE_erlang__ceil_1).
% OTP 20.0
-export([ceil/1]).
-endif.
-ifndef(HAVE_erlang__floor_1).
% OTP 20.0
-export([floor/1]).
-endif.
-ifndef(HAVE_erlang__atom_to_binary_1).
% OTP 23.0
-export([atom_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__binary_to_atom_1).
% OTP 23.0
-export([binary_to_atom/1]).
-endif.
-ifndef(HAVE_erlang__binary_to_existing_atom_1).
% OTP 23.0
-export([binary_to_existing_atom/1]).
-endif.

-ifndef(HAVE_erlang__get_keys_0).
get_keys() -> [K || {K, _} <- get()].
-endif.

-ifndef(HAVE_erlang__convert_time_unit_3).
convert_time_unit(Time, FromUnit, ToUnit) when is_integer(Time) ->
    FU = integer_time_unit(FromUnit),
    TU = integer_time_unit(ToUnit) * Time,
    if
        Time < 0 -> TU - FU + 1;
        true -> TU
    end div FU.

integer_time_unit(native) -> 1000000;
integer_time_unit(nano_seconds) -> 1000000000;
integer_time_unit(micro_seconds) -> 1000000;
integer_time_unit(milli_seconds) -> 1000;
integer_time_unit(seconds) -> 1;
integer_time_unit(I) when is_integer(I), I > 0 -> I;
integer_time_unit(BadRes) -> error(badarg, [BadRes]).
-endif.

-ifndef(HAVE_erlang__system_time_0).
system_time() ->
    {MS, S, US} = os:timestamp(),
    (MS * 1000000 + S) * 1000000 + US.
-endif.

-ifndef(HAVE_erlang__system_time_1).
system_time(nano_seconds) -> system_time() * 1000;
system_time(micro_seconds) -> system_time();
system_time(milli_seconds) -> system_time() div 1000;
system_time(seconds) ->
    {MS, S, _} = os:timestamp(),
    MS * 1000000 + S;
system_time(I) when is_integer(I), I > 0 -> system_time() * I div 1000000;
system_time(BadArg) -> error(badarg, [BadArg]).
-endif.

-ifndef(HAVE_erlang__monotonic_time_0).
monotonic_time() ->
    {MS, S, US} = now(),
    (MS * 1000000 + S) * 1000000 + US.
-endif.

-ifndef(HAVE_erlang__monotonic_time_1).
monotonic_time(nano_seconds) -> monotonic_time() * 1000;
monotonic_time(micro_seconds) -> monotonic_time();
monotonic_time(milli_seconds) -> monotonic_time() div 1000;
monotonic_time(seconds) ->
    {MS, S, _} = now(),
    MS * 1000000 + S;
monotonic_time(I) when is_integer(I), I > 0 -> monotonic_time() * I div 1000000;
monotonic_time(BadArg) -> error(badarg, [BadArg]).
-endif.

-ifndef(HAVE_erlang__time_offset_0).
time_offset() -> 0.
-endif.

-ifndef(HAVE_erlang__time_offset_1).
time_offset(nano_seconds) -> 0;
time_offset(micro_seconds) -> 0;
time_offset(milli_seconds) -> 0;
time_offset(seconds) -> 0;
time_offset(I) when is_integer(I), I > 0 -> 0;
time_offset(BadArg) -> error(badarg, [BadArg]).
-endif.

-ifndef(HAVE_erlang__unique_integer_0).
unique_integer() -> unique_integer([]).
-endif.

-ifndef(HAVE_erlang__unique_integer_1).
unique_integer([]) -> monotonic_time();
unique_integer([positive|O]) -> unique_integer(O);
unique_integer([monotonic|O]) -> unique_integer(O);
unique_integer(O) -> error(badarg, [O]).
-endif.

-ifndef(HAVE_erlang__ceil_1).
-ifdef(HAVE_math__ceil_1).
ceil(X) -> round(math:ceil(X)).
-else.
ceil(X) -> round(X + 0.5).
-endif.
-endif.

-ifndef(HAVE_erlang__floor_1).
-ifdef(HAVE_math__floor_1).
floor(X) -> round(math:floor(X)).
-else.
floor(X) -> round(X - 0.5).
-endif.
-endif.

-ifndef(HAVE_erlang__atom_to_binary_1).
atom_to_binary(A) -> atom_to_binary(A, utf8).
-endif.
-ifndef(HAVE_erlang__binary_to_atom_1).
binary_to_atom(B) -> binary_to_atom(B, utf8).
-endif.
-ifndef(HAVE_erlang__binary_to_existing_atom_1).
binary_to_existing_atom(B) -> binary_to_existing_atom(B, utf8).
-endif.
