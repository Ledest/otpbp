-module(otpbp_c).

-ifndef(HAVE_c__h_1).
% OTP 23.0
-export([h/1]).
-endif.
-ifndef(HAVE_c__h_2).
% OTP 23.0
-export([h/2]).
-endif.
-ifndef(HAVE_c__h_3).
% OTP 23.0
-export([h/3]).
-endif.
-ifndef(HAVE_c__hcb_1).
% OTP 23.0
-export([hcb/1]).
-endif.
-ifndef(HAVE_c__hcb_2).
% OTP 23.0
-export([hcb/2]).
-endif.
-ifndef(HAVE_c__hcb_3).
% OTP 23.0
-export([hcb/3]).
-endif.
-ifndef(HAVE_c__ht_1).
% OTP 23.0
-export([ht/1]).
-endif.
-ifndef(HAVE_c__ht_2).
% OTP 23.0
-export([ht/2]).
-endif.
-ifndef(HAVE_c__ht_3).
% OTP 23.0
-export([ht/3]).
-endif.

-ifndef(HAVE_c__h_1).
h(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__h_2).
h(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__h_3).
h(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_1).
hcb(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_2).
hcb(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__hcb_3).
hcb(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_1).
ht(M) when is_atom(M) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_2).
ht(M, F) when is_atom(M), is_atom(F) -> {error, missing}.
-endif.

-ifndef(HAVE_c__ht_3).
ht(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> {error, missing}.
-endif.
