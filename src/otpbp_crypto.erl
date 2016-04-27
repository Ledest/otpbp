-module(otpbp_crypto).

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
-ifndef(HAVE_crypto__sha256_mac_2).
-export([sha256_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha384_mac_2).
-export([sha384_mac/2]).
-endif.
-ifndef(HAVE_crypto__sha512_mac_2).
-export([sha512_mac/2]).
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
sha224_mac(Key, Data) -> crypto:hash_mac(sha224, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha256_mac_2).
sha256_mac(Key, Data) -> crypto:hash_mac(sha256, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha384_mac_2).
sha384_mac(Key, Data) -> crypto:hash_mac(sha384, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha512_mac_2).
sha512_mac(Key, Data) -> crypto:hash_mac(sha512, Key, Data).
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
