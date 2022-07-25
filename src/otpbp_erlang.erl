-module(otpbp_erlang).

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
-ifndef(HAVE_erlang__get_stacktrace_0).
% OTP 24.0
-export([get_stacktrace/0]).
-endif.
-ifndef(HAVE_erlang__get_cookie_1).
% OTP 24.1
-export([get_cookie/1]).
-endif.
-ifndef(HAVE_erlang__set_cookie_1).
% OTP 24.1
-export([set_cookie/1]).
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

-ifndef(HAVE_erlang__get_stacktrace_0).
get_stacktrace() -> [].
-endif.

-ifndef(HAVE_erlang__get_cookie_1).
get_cookie(Node) when is_atom(Node) -> auth:get_cookie(Node);
get_cookie(Node) -> error(badarg, [Node]).
-endif.

-ifndef(HAVE_erlang__set_cookie_1).
set_cookie(C) when is_atom(C) -> auth:set_cookie(C);
set_cookie(C) -> error(badarg, [C]).
-endif.
