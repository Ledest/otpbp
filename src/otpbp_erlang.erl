-module(otpbp_erlang).

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
-ifndef(HAVE_erlang__term_to_iovec_1).
% OTP 23.0
-export([term_to_iovec/1]).
-endif.
-ifndef(HAVE_erlang__term_to_iovec_2).
% OTP 23.0
-export([term_to_iovec/2]).
-endif.
-ifndef(HAVE_erlang__get_stacktrace_0).
% OTP 24.0
-export([get_stacktrace/0]).
-endif.
-ifndef(HAVE_erlang__error_3).
% OTP 24.0
-export([error/3]).
-endif.
-ifndef(HAVE_erlang__get_cookie_1).
% OTP 24.1
-export([get_cookie/1]).
-endif.
-ifndef(HAVE_erlang__set_cookie_1).
% OTP 24.1
-export([set_cookie/1]).
-endif.
-ifndef(HAVE_erlang__processes_iterator_0).
% OTP 28.0
-export([processes_iterator/0]).
-endif.
-ifndef(HAVE_erlang__processes_next_1).
% OTP 28.0
-export([processes_next/1]).
-endif.
-ifndef(HAVE_erlang__is_integer_3).
% OTP 29.0
-export([is_integer/3]).
-endif.
-ifndef(HAVE_erlang__exit_3).
% OTP 29.0
-export([exit/3]).
-endif.
-ifndef(HAVE_erlang__exit_signal_3).
% OTP 29.0
-export([exit_signal/3]).
-endif.
-ifndef(HAVE_erlang__pid_to_binary_1).
% OTP 30.0
-export([pid_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__port_to_binary_1).
% OTP 30.0
-export([port_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__ref_to_binary_1).
% OTP 30.0
-export([ref_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__integer_set_bits_1).
% OTP 30.0
-export([integer_set_bits/1]).
-endif.
-ifndef(HAVE_erlang__integer_signed_width_1).
% OTP 30.0
-export([integer_signed_width/1]).
-endif.
-ifndef(HAVE_erlang__integer_trailing_zeroes_1).
% OTP 30.0
-export([integer_trailing_zeroes/1]).
-endif.
-ifndef(HAVE_erlang__integer_unsigned_width_1).
% OTP 30.0
-export([integer_unsigned_width/1]).
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

-ifndef(HAVE_erlang__term_to_iovec_1).
term_to_iovec(T) -> [term_to_binary(T)].
-endif.
-ifndef(HAVE_erlang__term_to_iovec_2).
term_to_iovec(T, O) -> [term_to_binary(T, O)].
-endif.

-ifndef(HAVE_erlang__get_stacktrace_0).
get_stacktrace() -> [].
-endif.

-ifndef(HAVE_erlang__error_3).
error(Reason, Args, _Options) -> error(Reason, Args).
-endif.

-ifndef(HAVE_erlang__get_cookie_1).
get_cookie(Node) when is_atom(Node) -> auth:get_cookie(Node);
get_cookie(Node) -> error(badarg, [Node]).
-endif.

-ifndef(HAVE_erlang__set_cookie_1).
set_cookie(C) when is_atom(C) -> auth:set_cookie(C);
set_cookie(C) -> error(badarg, [C]).
-endif.

-ifndef(HAVE_erlang__processes_iterator_0).
processes_iterator() -> {0, processes()}.
-endif.

-ifndef(HAVE_erlang__is_integer_3).
is_integer(T, L, U) ->
    is_integer(L) andalso is_integer(U) orelse error(badarg, [T, L, U]),
    is_integer(T) andalso T >= L andalso T =< U.
-endif.

-ifndef(HAVE_erlang__exit_3).
exit(Dest, Reason, OptList) when OptList =:= []; OptList =:= [priority] -> exit(Dest, Reason).
-endif.

-ifndef(HAVE_erlang__exit_signal_3).
-ifdef(HAVE_erlang__exit_signal_3).
exit_signal(Dest, Reason, OptList) -> exit(Dest, Reason, OptList).
-else.
exit_signal(Dest, Reason, OptList) when OptList =:= []; OptList =:= [priority] -> exit(Dest, Reason).
-endif.
-endif.

-ifndef(HAVE_erlang__processes_next_1).
-ifdef(HAVE_erts_internal__processes_next_1).
processes_next({I, [Pid|Pids]}) -> {Pid, {I, Pids}};
processes_next({I0, []} = A) ->
    try erts_internal:processes_next(I0) of
        none -> none;
        {I, [Pid|Pids]} -> {Pid, {I, Pids}};
        {_, []} = I -> processes_next(I)
    catch
        error:badarg -> error(badarg, [A])
    end;
processes_next(A) -> error(badarg, [A]).
-else.
processes_next({I, [Pid|Pids]}) -> {Pid, {I, Pids}};
processes_next({_, []}) -> none;
processes_next(A) -> error(badarg, [A]).
-endif.
-endif.

-ifndef(HAVE_erlang__pid_to_binary_1).
pid_to_binary(Pid) -> list_to_binary(pid_to_list(Pid)).
-endif.

-ifndef(HAVE_erlang__port_to_binary_1).
port_to_binary(Port) -> list_to_binary(port_to_list(Port)).
-endif.

-ifndef(HAVE_erlang__ref_to_binary_1).
ref_to_binary(Ref) -> list_to_binary(ref_to_list(Ref)).
-endif.

-ifndef(HAVE_erlang__integer_set_bits_1).
integer_set_bits(I) when is_integer(I), I >= 0 -> integer_set_bits(I, 0);
integer_set_bits(I) -> error(badarg, [I]).

integer_set_bits(0, A) -> A;
integer_set_bits(I, A) -> integer_set_bits(I div 2, A + I rem 2).
-endif.

-ifndef(HAVE_erlang__integer_signed_width_1).
integer_signed_width(I) when is_integer(I) ->
    integer_unsigned_width(if
                               I >= 0 -> I;
                               true -> -I - 1
                           end,
                           1);
integer_signed_width(I)  -> error(badarg, [I]).

-ifndef(NEED_integer_unsigned_width_2).
-define(NEED_integer_unsigned_width_2, true).
-endif.
-endif.

-ifndef(HAVE_erlang__integer_trailing_zeroes_1).
integer_trailing_zeroes(I) when is_integer(I), I =/= 0 -> integer_trailing_zeroes(I, 0);
integer_trailing_zeroes(I) -> error(badarg, [I]).

integer_trailing_zeroes(I, A) when I =/= 0, I rem 2 =:= 0 -> integer_trailing_zeroes(I div 2, A + 1);
integer_trailing_zeroes(_, A) -> A.
-endif.

-ifndef(HAVE_erlang__integer_unsigned_width_1).
integer_unsigned_width(I) when is_integer(I), I >= 0 -> integer_unsigned_width(I, 0);
integer_unsigned_width(I)  -> error(badarg, [I]).

-ifndef(NEED_integer_unsigned_width_2).
-define(NEED_integer_unsigned_width_2, true).
-endif.
-endif.

-ifdef(NEED_integer_unsigned_width_2).
integer_unsigned_width(0, A) -> A;
integer_unsigned_width(I, A) -> integer_unsigned_width(I div 2, A + 1).
-endif.
