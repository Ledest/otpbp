-module(otpbp_crypto).

-if(?OTP_RELEASE =:= 23).
-compile({nowarn_deprecated_function, [{crypto, hmac, 3}, {crypto, hmac, 4}]}).
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

-ifndef(HAVE_crypto__md4_1).
-export([md4/1]).
-endif.
-ifndef(HAVE_crypto__md4_init_0).
-export([md4_init/0]).
-endif.
-ifndef(HAVE_crypto__md5_1).
-export([md5/1]).
-endif.
-ifndef(HAVE_crypto__md5_init_0).
-export([md5_init/0]).
-endif.
-ifndef(HAVE_crypto__sha_1).
-export([sha/1]).
-endif.
-ifndef(HAVE_crypto__sha_init_0).
-export([sha_init/0]).
-endif.

-ifndef(HAVE_crypto__cipher_info_1).
% OTP 22.0
-export([cipher_info/1]).
-endif.
-ifndef(HAVE_crypto__crypto_one_time_4).
% OTP 22.0
-export([crypto_one_time/4]).
-endif.
-ifndef(HAVE_crypto__crypto_one_time_5).
% OTP 22.0
-export([crypto_one_time/5]).
-endif.
-ifndef(HAVE_crypto__hash_info_1).
% OTP 22.0
-export([hash_info/1]).
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
-ifndef(HAVE_crypto__macN_5).
% OTP 22.1
-export([macN/5]).
-endif.
-ifndef(HAVE_crypto__macN_4).
% OTP 22.1
-export([macN/4]).
-endif.

-ifndef(HAVE_crypto__hmac_3).
% OTP < 24
-export([hmac/3]).
-endif.
-ifndef(HAVE_crypto__hmac_4).
% OTP < 24
-export([hmac/4]).
-endif.
-ifndef(HAVE_crypto__hmac_init_2).
% OTP < 24
-export([hmac_init/2]).
-endif.
-ifndef(HAVE_crypto__hmac_update_2).
% OTP < 24
-export([hmac_update/2]).
-endif.
-ifndef(HAVE_crypto__hmac_final_1).
% OTP < 24
-export([hmac_final/1]).
-endif.
-ifndef(HAVE_crypto__hmac_final_n_2).
% OTP < 24
-export([hmac_final_n/2]).
-endif.
-ifndef(HAVE_crypto__cmac_3).
-ifdef(HAVE_crypto__mac_4).
% OTP < 24
-export([cmac/3]).
-endif.
-endif.
-ifndef(HAVE_crypto__cmac_4).
-ifdef(HAVE_crypto__macN_5).
% OTP < 24
-export([cmac/4]).
-endif.
-endif.
-ifndef(HAVE_crypto__poly1305_2).
-ifdef(HAVE_crypto__mac_3).
% OTP < 24
-export([poly1305/2]).
-endif.
-endif.
-ifndef(HAVE_crypto__block_decrypt_3).
% OTP < 24
-export([block_decrypt/3]).
-endif.
-ifndef(HAVE_crypto__block_decrypt_4).
% OTP < 24
-export([block_decrypt/4]).
-endif.
-ifndef(HAVE_crypto__block_encrypt_3).
% OTP < 24
-export([block_encrypt/3]).
-endif.
-ifndef(HAVE_crypto__block_encrypt_4).
% OTP < 24
-export([block_encrypt/4]).
-endif.

-ifndef(HAVE_crypto__stream_init_2).
% OTP < 24
-export([stream_init/2]).
-endif.
-ifndef(HAVE_crypto__stream_init_3).
% OTP < 24
-export([stream_init/3]).
-endif.
-ifndef(HAVE_crypto__stream_decrypt_2).
% OTP < 24
-export([stream_decrypt/2]).
-endif.
-ifndef(HAVE_crypto__stream_encrypt_2).
% OTP < 24
-export([stream_encrypt/2]).
-endif.

-ifndef(HAVE_crypto__hash_equals_2).
% OTP 25.0
-export([hash_equals/2]).
-endif.

-ifndef(HAVE_crypto__start_0).
-export([start/0]).
-endif.
-ifndef(HAVE_crypto__stop_0).
-export([stop/0]).
-endif.

-ifndef(HAVE_crypto__mac_3).
-ifdef(HAVE_crypto__mac_4).
-import(crypto, [mac/4]).
-endif.
-endif.
-ifndef(HAVE_crypto__macN_5).
-ifdef(HAVE_crypto__mac_4).
-import(crypto, [mac/4]).
-endif.
-endif.
-ifndef(HAVE_crypto__macN_4).
-ifdef(HAVE_crypto__macN_5).
-import(crypto, [macN/5]).
-endif.
-endif.

-ifdef(HAVE_crypto__hmac_3).
-import(crypto, [hmac/3]).
-endif.
-ifdef(HAVE_crypto__hmac_4).
-import(crypto, [hmac/4]).
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
sha224_mac(Key, Data) -> hmac(sha224, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha224_mac_3).
sha224_mac(Key, Data, Size) -> hmac(sha224, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha256_mac_2).
sha256_mac(Key, Data) -> hmac(sha256, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha256_mac_3).
sha256_mac(Key, Data, Size) -> hmac(sha256, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha384_mac_2).
sha384_mac(Key, Data) -> hmac(sha384, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha384_mac_3).
sha384_mac(Key, Data, Size) -> hmac(sha384, Key, Data, Size).
-endif.
-ifndef(HAVE_crypto__sha512_mac_2).
sha512_mac(Key, Data) -> hmac(sha512, Key, Data).
-endif.
-ifndef(HAVE_crypto__sha512_mac_3).
sha512_mac(Key, Data, Size) -> hmac(sha512, Key, Data, Size).
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

-ifndef(HAVE_crypto__md4_1).
md4(Data) -> crypto:hash(md4, Data).
-endif.
-ifndef(HAVE_crypto__md4_init_0).
md4_init() -> crypto:hash_init(md4).
-endif.
-ifndef(HAVE_crypto__md5_1).
md5(Data) -> crypto:hash(md5, Data).
-endif.
-ifndef(HAVE_crypto__md5_init_0).
md5_init() -> crypto:hash_init(md5).
-endif.
-ifndef(HAVE_crypto__sha_1).
sha(Data) -> crypto:hash(sha, Data).
-endif.
-ifndef(HAVE_crypto__sha_init_0).
sha_init() -> crypto:hash_init(sha).
-endif.

-ifndef(HAVE_crypto__cipher_info_1).
cipher_info(aes_ctr) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => ctr_mode, type => undefined};
cipher_info(aes_128_cbc) -> #{block_size => 16, iv_length => 16, key_length => 16, mode => cbc_mode, type => 419};
cipher_info(aes_128_ccm) -> #{block_size => 1, iv_length => 12, key_length => 16, mode => ccm_mode, type => 896};
cipher_info(aes_128_cbc) -> #{block_size => 16, iv_length => 16, key_length => 16, mode => cbc_mode, type => 419};
cipher_info(aes_128_ccm) -> #{block_size => 1, iv_length => 12, key_length => 16, mode => ccm_mode, type => 896};
cipher_info(aes_128_cfb128) -> #{block_size => 1, iv_length => 16, key_length => 16, mode => cfb_mode,type => 421};
cipher_info(aes_128_cfb8) -> #{block_size => 1, iv_length => 16, key_length => 16, mode => cfb_mode, type => 421};
cipher_info(aes_128_ctr) -> #{block_size => 1, iv_length => 16, key_length => 16, mode => ctr_mode, type => undefined};
cipher_info(aes_128_ecb) -> #{block_size => 16, iv_length => 0, key_length => 16, mode => ecb_mode, type => 418};
cipher_info(aes_128_gcm) -> #{block_size => 1, iv_length => 12, key_length => 16, mode => gcm_mode, type => 895};
cipher_info(aes_192_cbc) -> #{block_size => 16, iv_length => 16, key_length => 24, mode => cbc_mode, type => 423};
cipher_info(aes_192_ccm) -> #{block_size => 1, iv_length => 12, key_length => 24, mode => ccm_mode, type => 899};
cipher_info(aes_192_cfb128) -> #{block_size => 1, iv_length => 16, key_length => 24, mode => cfb_mode, type => 425};
cipher_info(aes_192_cfb8) -> #{block_size => 1, iv_length => 16, key_length => 24, mode => cfb_mode, type => 425};
cipher_info(aes_192_ctr) -> #{block_size => 1, iv_length => 16, key_length => 24, mode => ctr_mode, type => undefined};
cipher_info(aes_192_ecb) -> #{block_size => 16, iv_length => 0, key_length => 24, mode => ecb_mode, type => 422};
cipher_info(aes_192_gcm) -> #{block_size => 1, iv_length => 12, key_length => 24, mode => gcm_mode, type => 898};
cipher_info(aes_256_cbc) -> #{block_size => 16, iv_length => 16, key_length => 32, mode => cbc_mode, type => 427};
cipher_info(aes_256_ccm) -> #{block_size => 1, iv_length => 12, key_length => 32, mode => ccm_mode, type => 902};
cipher_info(aes_256_cfb128) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => cfb_mode, type => 429};
cipher_info(aes_256_cfb8) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => cfb_mode, type => 429}; 
cipher_info(aes_256_ctr) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => ctr_mode, type => undefined};
cipher_info(aes_256_ecb) -> #{block_size => 16, iv_length => 0, key_length => 32, mode => ecb_mode, type => 426};
cipher_info(aes_256_gcm) -> #{block_size => 1, iv_length => 12, key_length => 32, mode => gcm_mode, type => 901};
cipher_info(aes_ige256) -> #{block_size => 16, iv_length => 32, key_length => 16, mode => ige_mode, type => undefined};
cipher_info(blowfish_cbc) -> #{block_size => 8, iv_length => 8, key_length => 16, mode => cbc_mode, type => 91};
cipher_info(blowfish_cfb64) -> #{block_size => 1, iv_length => 8, key_length => 16, mode => cfb_mode, type => undefined};
cipher_info(blowfish_ecb) -> #{block_size => 8, iv_length => 0, key_length => 16, mode => ecb_mode, type => undefined};
cipher_info(blowfish_ofb64) -> #{block_size => 1, iv_length => 8, key_length => 16, mode => ofb_mode, type => undefined};
cipher_info(chacha20) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => stream_cipher, type => undefined};
cipher_info(chacha20_poly1305) ->
    #{block_size => 1, iv_length => 12, key_length => 32, mode => stream_cipher, type => undefined};
cipher_info(des_cbc) -> #{block_size => 8, iv_length => 8, key_length => 8, mode => cbc_mode, type => 31};
cipher_info(des_cfb) -> #{block_size => 1, iv_length => 8, key_length => 8, mode => cfb_mode, type => 30};
cipher_info(des_ecb) -> #{block_size => 8, iv_length => 0, key_length => 8, mode => ecb_mode, type => 29};
cipher_info(des_ede3_cbc) -> #{block_size => 8, iv_length => 8, key_length => 24, mode => cbc_mode, type => 44};
cipher_info(des_ede3_cfb) -> #{block_size => 1, iv_length => 8, key_length => 24, mode => cfb_mode, type => 30};
cipher_info(rc2_cbc) -> #{block_size => 8, iv_length => 8, key_length => 16, mode => cbc_mode, type => 37};
cipher_info(rc4) -> #{block_size => 1, iv_length => 0, key_length => 16, mode => stream_cipher, type => 5};
%% These ciphers belong to the "old" interface:
cipher_info(aes_cbc) -> #{block_size => 16, iv_length => 16, key_length => 24, mode => cbc_mode, type => 423};
cipher_info(aes_cbc128) -> #{block_size => 16, iv_length => 16, key_length => 16, mode => cbc_mode, type => 419};
cipher_info(aes_cbc256) -> #{block_size => 16, iv_length => 16, key_length => 32, mode => cbc_mode, type => 427};
cipher_info(aes_ccm) -> #{block_size => 1, iv_length => 12, key_length => 24, mode => ccm_mode, type => 899};
cipher_info(aes_cfb128) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => cfb_mode, type => 429};
cipher_info(aes_cfb8) -> #{block_size => 1, iv_length => 16, key_length => 32, mode => cfb_mode, type => 429};
cipher_info(aes_ecb) -> #{block_size => 16, iv_length => 0, key_length => 24, mode => ecb_mode, type => 422};
cipher_info(aes_gcm) -> #{block_size => 1, iv_length => 12, key_length => 24, mode => gcm_mode, type => 898};
cipher_info(des3_cbc) -> #{block_size => 8, iv_length => 8, key_length => 24, mode => cbc_mode, type => 44};
cipher_info(des3_cbf) -> #{block_size => 1, iv_length => 8, key_length => 24, mode => cfb_mode, type => 30};
cipher_info(des3_cfb) -> #{block_size => 1, iv_length => 8, key_length => 24, mode => cfb_mode, type => 30};
cipher_info(des_ede3) -> #{block_size => 8, iv_length => 8, key_length => 24, mode => cbc_mode, type => 44};
cipher_info(des_ede3_cbf) -> #{block_size => 1, iv_length => 8, key_length => 24, mode => cfb_mode, type => 30};
cipher_info(_Type) -> error(badarg).
-endif.

-ifndef(HAVE_crypto__crypto_one_time_4).
crypto_one_time(Cipher, Key, Data, false) -> crypto:block_decrypt(Cipher, Key, Data);
crypto_one_time(Cipher, Key, Data, true) -> crypto:block_encrypt(Cipher, Key, Data);
crypto_one_time(Cipher, Key, Data, Flag) -> error(badarg, [Cipher, Key, Data, Flag]).
-endif.

-ifndef(HAVE_crypto__crypto_one_time_5).
crypto_one_time(Cipher, Key, IV, Data, false) -> crypto:block_decrypt(Cipher, Key, IV, Data);
crypto_one_time(Cipher, Key, IV, Data, true) -> crypto:block_encrypt(Cipher, Key, IV, Data);
crypto_one_time(Cipher, Key, IV, Data, Flag) -> error(badarg, [Cipher, Key, IV, Data, Flag]).
-endif.

-ifndef(HAVE_crypto__hash_info_1).
hash_info(blake2b) -> #{block_size => 128, size => 64, type => 1056};
hash_info(blake2s) -> #{block_size => 64, size => 32, type => 1057};
hash_info(md4) -> #{block_size => 64, size => 16, type => 257};
hash_info(md5) -> #{block_size => 64, size => 16, type => 4};
hash_info(ripemd160) -> #{block_size => 64, size => 20, type => 117};
hash_info(sha) -> #{block_size => 64, size => 20, type => 64};
hash_info(sha224) -> #{block_size => 64, size => 28, type => 675};
hash_info(sha256) -> #{block_size => 64, size => 32, type => 672};
hash_info(sha384) -> #{block_size => 128, size => 48, type => 673};
hash_info(sha3_224) -> #{block_size => 144, size => 28, type => 1096};
hash_info(sha3_256) -> #{block_size => 136, size => 32, type => 1097};
hash_info(sha3_384) -> #{block_size => 104, size => 48, type => 1098};
hash_info(sha3_512) -> #{block_size => 72, size => 64, type => 1099};
hash_info(sha512) -> #{block_size => 128, size => 64, type => 674};
hash_info(_Type) -> error(badarg).
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
mac(hmac, T, Key, Data) -> mac_hmac(T, Key, Data);
mac(cmac, T, Key, Data) -> mac_cmac(T, Key, Data);
mac(T, SubType, Key, Data) -> mac_unknown(T, SubType, Key, Data).
-else.
mac(hmac, T, Key, Data) -> mac_hmac(T, Key, Data);
mac(cmac, T, Key, Data) -> mac_cmac(T, Key, Data);
mac(T, SubType, Key, Data) -> mac_unknown(T, SubType, Key, Data).
-endif.

is_iodata(E) ->
    is_binary(E) orelse try iolist_size(E) of
                            _ -> true
                        catch
                            _:_ -> false
                        end.

mac_hmac(T, Key, Data) ->
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
mac_cmac(T, Key, Data) ->
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
mac_cmac(T, Key, Data) -> mac_unknown(cmac, T, Key, Data).
-endif.

mac_unknown(T, SubType, Key, Data) -> error({badarg, {"mac.c", 229}, "Unknown mac algorithm"}, [T, SubType, Key, Data]).

-compile({inline, [mac_hmac/3, mac_cmac/3, mac_unknown/4]}).
-endif.

-ifndef(HAVE_crypto__macN_5).
macN(Type, SubType, Key, Data, MacLength) -> binary:part(mac(Type, SubType, Key, Data), 0, MacLength).
-endif.

-ifndef(HAVE_crypto__macN_4).
macN(Type, Key, Data, MacLength) -> macN(Type, undefined, Key, Data, MacLength).
-endif.

-ifndef(HAVE_crypto__hmac_3).
hmac(Type, Key, Data) ->
    try
        crypto:mac(hmac, Type, Key, Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__hmac_4).
hmac(Type, Key, Data, MacLength) ->
    try
        crypto:macN(hmac, Type, Key, Data, MacLength)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__hmac_init_2).
hmac_init(Type, Key) ->
    try
        crypto:mac_init(hmac, Type, Key)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__hmac_update_2).
hmac_update(State, HashLen) ->
    try
        crypto:mac_update(State, HashLen)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__hmac_final_1).
hmac_final(Context) ->
    try
        crypto:mac_final(Context)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__hmac_final_n_2).
hmac_final_n(Context, HashLen) ->
    try
        crypto:mac_finalN(Context, HashLen)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__cmac_3).
-ifdef(HAVE_crypto__mac_4).
cmac(Type, Key, Data) ->
    Alias = alias(Type),
    try
        crypto:mac(cmac, Alias, Key, Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_1).
-define(NEED_ALIAS_1, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_crypto__cmac_4).
-ifdef(HAVE_crypto__macN_5).
cmac(Type, Key, Data, MacLength) ->
    Alias = alias(Type),
    try
        crypto:macN(cmac, Alias, Key, Data, MacLength)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_1).
-define(NEED_ALIAS_1, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_crypto__poly1305_2).
-ifdef(HAVE_crypto__mac_3).
poly1305(Key, Data) ->
    try
        crypto:mac(poly1305, Key, Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.
-endif.

-ifdef(NEED_ALIAS_1).
-compile({no_auto_import, [alias/1]}).

alias(aes_cbc128) -> aes_128_cbc;
alias(aes_cbc256) -> aes_256_cbc;
alias(A) when A =:= des3_cbc; A =:= des_ede3 -> des_ede3_cbc;
alias(A) when A =:= des_ede3_cbf; A =:= des3_cbf; A =:= des3_cfb -> des_ede3_cfb;
alias(A) -> A.
-endif.

-ifndef(HAVE_crypto__hash_equals_2).
hash_equals(A, B) when is_binary(A), is_binary(B) -> A =:= B;
hash_equals(A, B) -> error(badarg, [A, B]).
-endif.

-ifndef(HAVE_crypto__block_decrypt_4).
block_decrypt(aes_ige256, _Key, _IV, _Data) -> error(notsup);
block_decrypt(Type, Key, IV, {AAD, Data, TagLength}) ->
    try
        crypto:crypto_one_time_aead(alias(Type, Key), Key, IV, Data, AAD, TagLength, false)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end;
block_decrypt(Type, Key, IV, Data) ->
    try
        crypto:crypto_update(crypto:crypto_init(alias(Type, Key), Key, IV, false), Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_2).
-define(NEED_ALIAS_2, true).
-endif.
-endif.

-ifndef(HAVE_crypto__block_decrypt_3).
-ifdef(HAVE_crypto__block_decrypt_4).
block_decrypt(Type, Key, Data) -> crypto:block_decrypt(Type, Key, <<>>, Data).
-else.
block_decrypt(Type, Key, Data) ->
    try
        crypto:crypto_update(crypto:crypto_init(alias(Type, Key), Key, false), Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_2).
-define(NEED_ALIAS_2, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_crypto__block_encrypt_4).
block_encrypt(aes_ige256, _Key, _IV, _Data) -> error(notsup);
block_encrypt(Type, Key, IV, {AAD, Data, TagLength}) ->
    try
        crypto:crypto_one_time_aead(alias(Type, Key), Key, IV, Data, AAD, TagLength, true)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end;
block_encrypt(Type, Key, IV, {AAD, Data}) ->
    try
        crypto:crypto_one_time_aead(alias(Type, Key), Key, IV, Data, AAD, true)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end;
block_encrypt(Type, Key, IV, Data) ->
    try
        crypto:crypto_update(crypto:crypto_init(alias(Type, Key), Key, IV, true), Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_2).
-define(NEED_ALIAS_2, true).
-endif.
-endif.

-ifndef(HAVE_crypto__block_encrypt_3).
-ifdef(HAVE_crypto__block_encrypt_4).
block_encrypt(Type, Key, Data) -> crypto:block_encrypt(Type, Key, <<>>, Data).
-else.
block_encrypt(Type, Key, Data) ->
    try
        crypto:crypto_update(crypto:crypto_init(alias(Type, Key), Key, true), Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_2).
-define(NEED_ALIAS_2, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_crypto__stream_init_2).
stream_init(rc4, Key) ->
    try crypto:crypto_init(rc4, Key, [{encrypt, undefined}]) of
        Ref -> {rc4, {Ref, flg_undefined}}
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__stream_init_3).
stream_init(Type, Key, IVec) when is_binary(IVec) ->
    Cypher = alias(Type, Key),
    try crypto:crypto_init(Cypher, Key, IVec, [{encrypt, undefined}]) of
        Ref -> {Cypher, {Ref, flg_undefined}}
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-ifndef(NEED_ALIAS_2).
-define(NEED_ALIAS_2, true).
-endif.
-endif.

-ifndef(HAVE_crypto__stream_decrypt_2).
stream_decrypt({Cipher, {Ref, flg_undefined}}, Data) ->
    try crypto:crypto_init(Cipher, <<>>, [{encrypt, false}]) of
        Ref -> stream_decrypt({Cipher, Ref}, Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end;
stream_decrypt({_Cipher, Ref} = State, Data) ->
    try crypto:crypto_update(Ref, Data) of
        Text -> {State, Text}
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__stream_encrypt_2).
stream_encrypt({Cipher, {Ref0, flg_undefined}}, Data) ->
    try crypto:crypto_init(Ref0, <<>>, [{encrypt, true}]) of
        Ref -> stream_encrypt({Cipher, Ref}, Data)
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end;
stream_encrypt({_Cipher, Ref} = State, Data) ->
    try crypto:crypto_update(Ref, Data) of
        Text -> {State, Text}
    catch
        error:{error, {_File, _Line}, _Reason} -> error(badarg);
        error:{E, {_File, _Line}, _Reason} when E =:= notsup; E =:= badarg -> error(E)
    end.
-endif.

-ifndef(HAVE_crypto__start_0).
start() -> application:start(crypto).
-endif.

-ifndef(HAVE_crypto__stop_0).
stop() -> application:stop(crypto).
-endif.

-ifdef(NEED_ALIAS_2).
alias(aes_cbc128, _) -> aes_128_cbc;
alias(aes_cbc256, _) -> aes_256_cbc;
alias(aes_cbc, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_cbc;
        24 -> aes_192_cbc;
        32 -> aes_256_cbc
    end;
alias(aes_cfb8, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_cfb8;
        24 -> aes_192_cfb8;
        32 -> aes_256_cfb8
    end;
alias(aes_cfb128, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_cfb128;
        24 -> aes_192_cfb128;
        32 -> aes_256_cfb128
    end;
alias(aes_ctr, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_ctr;
        24 -> aes_192_ctr;
        32 -> aes_256_ctr
    end;
alias(aes_ebc, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_ebc;
        24 -> aes_192_ebc;
        32 -> aes_256_ebc
    end;
alias(aes_gcm, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_gcm;
        24 -> aes_192_gcm;
        32 -> aes_256_gcm
    end;
alias(aes_ccm, Key) ->
    case iolist_size(Key) of
        16 -> aes_128_ccm;
        24 -> aes_192_ccm;
        32 -> aes_256_ccm
    end;
alias(A, _) when A =:= des3_cbc; A =:= des_ede3 -> des_ede3_cbc;
alias(A, _) when A =:= des_ede3_cbf; A =:= des3_cbf; A =:= des3_cfb -> des_ede3_cfb;
alias(A, _) -> A.
-endif.
