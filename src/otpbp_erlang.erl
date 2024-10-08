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
