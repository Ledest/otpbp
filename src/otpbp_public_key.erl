-module(otpbp_public_key).

-compile({parse_transform, otpbp_pt}).

-ifndef(HAVE_public_key__cacerts_load_1).
% OTP 25.0
-export([cacerts_load/1]).
-endif.

-ifndef(HAVE_public_key__cacerts_load_1).
cacerts_load(File) -> pubkey_os_cacerts:load([File]).
-endif.
