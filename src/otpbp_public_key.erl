-module(otpbp_public_key).

-compile({parse_transform, otpbp_pt}).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-ifndef(HAVE_public_key__cacerts_clear_0).
% OTP 25.0
-export([cacerts_clear/0]).
-endif.
-ifndef(HAVE_public_key__cacerts_get_0).
% OTP 25.0
-export([cacerts_get/0]).
-endif.
-ifndef(HAVE_public_key__cacerts_load_0).
% OTP 25.0
-export([cacerts_load/0]).
-endif.
-ifndef(HAVE_public_key__cacerts_load_1).
% OTP 25.0
-export([cacerts_load/1]).
-endif.
-ifndef(HAVE_public_key__encrypt_private_3).
% OTP 26.0
-export([encrypt_private/3]).
-endif.
-ifndef(HAVE_public_key__encrypt_public_3).
% OTP 26.0
-export([encrypt_public/3]).
-endif.
-ifndef(HAVE_public_key__pkix_hash_type_1).
% OTP 23.0
-export([pkix_hash_type/1]).
-endif.
-ifndef(HAVE_public_key__pkix_subject_id_1).
% OTP 23.1
-export([pkix_subject_id/1]).
-endif.

-ifndef(HAVE_public_key__cacerts_clear_0).
cacerts_clear() -> pubkey_os_cacerts:clear().
-endif.

-ifndef(HAVE_public_key__cacerts_get_0).
cacerts_get() -> pubkey_os_cacerts:get().
-endif.

-ifndef(HAVE_public_key__cacerts_load_0).
cacerts_load() -> pubkey_os_cacerts:load().
-endif.

-ifndef(HAVE_public_key__cacerts_load_1).
cacerts_load(File) -> pubkey_os_cacerts:load([File]).
-endif.

-ifndef(HAVE_public_key__encrypt_private_3).
encrypt_private(PlainText, #'RSAPrivateKey'{modulus = N, publicExponent = E, privateExponent = D} = Key, Options)
  when is_binary(PlainText), is_integer(N), is_integer(E), is_integer(D), is_list(Options) ->
    crypto:private_encrypt(rsa, PlainText, format_rsa_private_key(Key), default_options(Options)).

-compile({inline, format_rsa_private_key/1}).
format_rsa_private_key(#'RSAPrivateKey'{modulus = N, publicExponent = E, privateExponent = D, prime1 = P1, prime2 = P2,
                                        exponent1 = E1, exponent2 = E2, coefficient = C})
  when is_integer(N), is_integer(E), is_integer(D) ->
    if
        is_integer(P1), is_integer(P2), is_integer(E1), is_integer(E2), is_integer(C) -> [E, N, D, P1, P2, E1, E2, C];
        true -> [E, N, D]
    end.

-ifndef(NEED_default_options_1).
-define(NEED_default_options_1, true).
-endif.
-endif.

-ifndef(HAVE_public_key__encrypt_public_3).
encrypt_public(PlainText, #'RSAPublicKey'{modulus = N, publicExponent = E}, Options)
  when is_binary(PlainText), is_list(Options) ->
    crypto:public_encrypt(rsa, PlainText, [E, N], default_options(Options)).

-ifndef(NEED_default_options_1).
-define(NEED_default_options_1, true).
-endif.
-endif.

-ifndef(HAVE_public_key__pkix_hash_type_1).
pkix_hash_type(?'id-sha1') -> sha;
pkix_hash_type(?'id-sha512') -> sha512;
pkix_hash_type(?'id-sha384') -> sha384;
pkix_hash_type(?'id-sha256') -> sha256;
pkix_hash_type('id-sha224') -> sha224;
pkix_hash_type('id-md5') -> md5.
-endif.

-ifndef(HAVE_public_key__pkix_subject_id_1).
pkix_subject_id(#'OTPCertificate'{} = OtpCert) -> pubkey_cert:subject_id(OtpCert);
pkix_subject_id(Cert) when is_binary(Cert) -> pubkey_cert:subject_id(public_key:pkix_decode_cert(Cert, otp)).
-endif.

-ifdef(NEED_default_options_1).
default_options([]) -> [{rsa_padding, rsa_pkcs1_padding}];
default_options(Opts) ->
    set_padding(case proplists:get_value(rsa_pad, Opts) of
                    undefined ->
                        case proplists:get_value(rsa_padding, Opts) of
                            undefined ->
                                case lists:dropwhile(fun erlang:is_tuple/1, Opts) of
                                    [Pad|_] -> Pad;
                                    [] -> rsa_pkcs1_padding
                                end;
                            Pad -> Pad
                        end;
                    Pad -> Pad
                end,
                Opts).

set_padding(Pad, Opts) -> [{rsa_padding, Pad}|[V || {T, _} = V <- Opts, T =/= rsa_padding, T =/= rsa_pad]].
-endif.
