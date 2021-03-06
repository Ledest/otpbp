-module(otpbp_crypto).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-compile({nowarn_deprecated_function, [{crypto, hmac, 3}, {crypto, hmac, 4}]}).
-endif.
-endif.

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
% OTP 22.0
-export([supports/1]).
-endif.

-ifndef(HAVE_crypto__mac_3).
% OTP 22.1
-export([mac/3]).
-endif.
-ifndef(HAVE_crypto__mac_4).
% OTP 22.1
-export([mac/4]).
-endif.

-ifndef(HAVE_crypto__hash_equals_2).
% OTP 25.0
-export([hash_equals/2]).
-endif.

-ifndef(HAVE_crypto__mac_3).
-ifdef(HAVE_crypto__mac_4).
-import(crypto, [mac/4]).
-endif.
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

-ifndef(HAVE_crypto__mac_3).
mac(Type, Key, Data) -> mac(Type, undefined, Key, Data).
-endif.

-ifndef(HAVE_crypto__mac_4).
-ifdef(HAVE_crypto__poly1305_2).
mac(poly1305, T, Key, Data) ->
    try
        crypto:poly1305(Key, Data)
    catch
        error:badarg ->
            error(case is_binary(Data) orelse is_list(Data) andalso lists:all(fun is_iodata/1, Data) of
                      true ->
                          case is_binary(Key) orelse is_list(Key) andalso lists:all(fun is_iodata/1, Key) of
                              true -> {badarg,{"mac.c",231},"Bad key length"};
                              _false -> {badarg, {"mac.c", 216}, "Bad key"}
                          end;
                      _false -> {badarg, {"mac.c", 179}, "Bad text"}
                  end,
                  [poly1305, T, Key, Data]);
        C:R -> erlang:C(R, [poly1305, T, Key, Data])
    end;
mac(hmac, T, Key, Data) -> hmac(T, Key, Data);
mac(cmac, T, Key, Data) -> cmac(T, Key, Data);
mac(T, SubType, Key, Data) -> mac_unknown(T, SubType, Key, Data).
-else.
mac(hmac, T, Key, Data) -> hmac(T, Key, Data);
mac(cmac, T, Key, Data) -> cmac(T, Key, Data);
mac(T, SubType, Key, Data) -> mac_unknown(T, SubType, Key, Data).
-endif.
is_iodata(E) ->
    is_binary(E) orelse try iolist_size(E) of
                            _ -> true
                        catch
                            _:_ -> false
                        end.

hmac(T, Key, Data) ->
    try
        crypto:hmac(T, Key, Data)
    catch
        error:badarg ->
            error(case is_iodata(Data) of
                      true ->
                          case is_iodata(Key) of
                              true -> {badarg, {"mac.c", 259}, "Bad digest algorithm for HMAC"};
                              _false -> {badarg, {"mac.c", 216}, "Bad key"}
                          end;
                      _false -> {badarg, {"mac.c", 179}, "Bad text"}
                  end,
                  [hmac, T, Key, Data]);
        C:R -> erlang:C(R, [hmac, T, Key, Data])
    end.

-ifdef(HAVE_crypto__cmac_3).
cmac(T, Key, Data) ->
    try
        crypto:cmac(T, Key, Data)
    catch
        error:badarg ->
            error(case is_iodata(Data) of
                      true ->
                          case is_iodata(Key) of
                              true ->
                                  case lists:member(T, ciphers()) of
                                      true -> {badarg, {"mac.c", 306}, "Bad key size"};
                                      _false -> {badarg, {"mac.c", 303}, "Unknown cipher"}
                                  end;
                              _false -> {badarg, {"mac.c", 216}, "Bad key"}
                          end;
                      _false -> {badarg, {"mac.c", 179}, "Bad text"}
                  end,
                  [cmac, T, Key, Data]);
        C:R -> erlang:C(R, [cmac, T, Key, Data])
    end.

-compile({inline, ciphers/0}).
ciphers() ->
    {_, Ciphers} = lists:keyfind(ciphers, 1, crypto:supports()),
    Ciphers.
-else.
cmac(T, Key, Data) -> mac_unknown(cmac, T, Key, Data).
-endif.

mac_unknown(T, SubType, Key, Data) -> error({badarg, {"mac.c", 229}, "Unknown mac algorithm"}, [T, SubType, Key, Data]).

-compile({inline, [hmac/3, cmac/3, mac_unknown/4]}).
-endif.

-ifndef(HAVE_crypto__hash_equals_2).
hash_equals(A, B) when is_binary(A), is_binary(B) -> A =:= B;
hash_equals(A, B) -> error(badarg, [A, B]).
-endif.
