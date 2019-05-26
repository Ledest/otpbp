-module(otpbp_crypto).

-ifndef(HAVE_crypto__dss_sign_2).
-export([dss_sign/2]).
-endif.
-ifndef(HAVE_crypto__dss_sign_3).
-export([dss_sign/3]).
-endif.
-ifndef(HAVE_crypto__dss_verify_3).
-export([dss_verify/3]).
-endif.
-ifndef(HAVE_crypto__dss_verify_4).
-export([dss_verify/4]).
-endif.
-ifndef(HAVE_crypto__rsa_sign_2).
-export([rsa_sign/2]).
-endif.
-ifndef(HAVE_crypto__rsa_sign_3).
-export([rsa_sign/3]).
-endif.
-ifndef(HAVE_crypto__rsa_verify_3).
-export([rsa_verify/3]).
-endif.
-ifndef(HAVE_crypto__rsa_verify_4).
-export([rsa_verify/4]).
-endif.

-ifndef(HAVE_crypto__sha224_1).
-export([sha224/1]).
-endif.
-ifndef(HAVE_crypto__sha256_1).
-export([sha256/1]).
-endif.
-ifndef(HAVE_crypto__sha384_1).
-export([sha384/1]).
-endif.
-ifndef(HAVE_crypto__sha512_1).
-export([sha512/1]).
-endif.

-ifndef(HAVE_crypto__sha224_init_0).
-export([sha224_init/0]).
-endif.
-ifndef(HAVE_crypto__sha256_init_0).
-export([sha256_init/0]).
-endif.
-ifndef(HAVE_crypto__sha384_init_0).
-export([sha384_init/0]).
-endif.
-ifndef(HAVE_crypto__sha512_init_0).
-export([sha512_init/0]).
-endif.

-ifndef(HAVE_crypto__sha224_mac_2).
-export([sha224_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha224_mac_3).
-export([sha224_mac/3]).
-endif.
-ifndef(HAVE_crypto__sha256_mac_2).
-export([sha256_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha256_mac_3).
-export([sha256_mac/3]).
-endif.
-ifndef(HAVE_crypto__sha384_mac_2).
-export([sha384_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha384_mac_3).
-export([sha384_mac/3]).
-endif.
-ifndef(HAVE_crypto__sha512_mac_2).
-export([sha512_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha512_mac_3).
-export([sha512_mac/3]).
-endif.

-ifndef(HAVE_crypto__supports_1).
-export([supports/1]).
-endif.

-ifndef(HAVE_crypto__dss_sign_2).
dss_sign(Data, Key) -> crypto:sign(dss, sha, Data, Key).
-endif.
-ifndef(HAVE_crypto__dss_sign_3).
dss_sign(Type, Data, Key) -> crypto:sign(dss, Type, Data, Key).
-endif.
-ifndef(HAVE_crypto__dss_verify_3).
dss_verify(Data, Signature, Key) -> crypto:verify(dss, sha, Data, Signature, Key).
-endif.
-ifndef(HAVE_crypto__dss_verify_4).
dss_verify(Type, Data, Signature, Key) -> crypto:verify(dss, Type, Data, Signature, Key).
-endif.
-ifndef(HAVE_crypto__rsa_sign_2).
rsa_sign(Data, Key) -> crypto:sign(rsa, sha, Data, Key).
-endif.
-ifndef(HAVE_crypto__rsa_sign_3).
rsa_sign(Type, Data, Key) -> crypto:sign(rsa, Type, Data, Key).
-endif.
-ifndef(HAVE_crypto__rsa_verify_3).
rsa_verify(Data, Signature, Key) -> crypto:verify(rsa, sha, Data, Signature, Key).
-endif.
-ifndef(HAVE_crypto__rsa_verify_4).
rsa_verify(Type, Data, Signature, Key) -> crypto:verify(rsa, Type, Data, Signature, Key).
-endif.

-ifndef(HAVE_crypto__sha224_1).
sha224(Data) -> crypto:hash(sha224, Data).
-endif.
-ifndef(HAVE_crypto__sha256_1).
sha256(Data) -> crypto:hash(sha256, Data).
-endif.
-ifndef(HAVE_crypto__sha384_1).
sha384(Data) -> crypto:hash(sha384, Data).
-endif.
-ifndef(HAVE_crypto__sha512_1).
sha512(Data) -> crypto:hash(sha512, Data).
-endif.

-ifndef(HAVE_crypto__sha224_mac_2).
sha224_mac(Key, Data) -> crypto:hmac(sha224, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha224_mac_3).
sha224_mac(Key, Data, Size) -> crypto:hmac(sha224, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha256_mac_2).
sha256_mac(Key, Data) -> crypto:hmac(sha256, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha256_mac_3).
sha256_mac(Key, Data, Size) -> crypto:hmac(sha256, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha384_mac_2).
sha384_mac(Key, Data) -> crypto:hmac(sha384, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha384_mac_3).
sha384_mac(Key, Data, Size) -> crypto:hmac(sha384, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha512_mac_2).
sha512_mac(Key, Data) -> crypto:hmac(sha512, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha512_mac_3).
sha512_mac(Key, Data, Size) -> crypto:hmac(sha512, Key, Data, Size).
-endif.

-ifndef(HAVE_crypto__sha224_init_0).
sha224_init() -> crypto:hash_init(sha224).
-endif.
-ifndef(HAVE_crypto__sha256_init_0).
sha256_init() -> crypto:hash_init(sha256).
-endif.
-ifndef(HAVE_crypto__sha384_init_0).
sha384_init() -> crypto:hash_init(sha384).
-endif.
-ifndef(HAVE_crypto__sha512_init_0).
sha512_init() -> crypto:hash_init(sha512).
-endif.

-ifndef(HAVE_crypto__supports_1).
supports(T) when T =:= hashs; T =:= ciphers; T =:= public_keys; T =:= macs; T =:= curves; T =:= rsa_opts ->
    case lists:keyfind(T, 1, crypto:supports()) of
        {_, Support} -> Support;
        _ -> []
    end.
-endif.
