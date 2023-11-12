-module(otpbp_pubkey_ssh).

-ifndef(HAVE_pubkey_ssh__decode_2).
% OTP 25.0
-export([decode/2]).
-endif.
-ifndef(HAVE_pubkey_ssh__encode_2).
% OTP 25.0
-export([encode/2]).
-endif.
-ifndef(HAVE_pubkey_ssh__new_openssh_decode_1).
% OTP 25.0
-export([new_openssh_decode/1]).
-endif.
-ifndef(HAVE_pubkey_ssh__new_openssh_encode_1).
% OTP 25.0
-export([new_openssh_encode/1]).
-endif.
-ifndef(HAVE_pubkey_pad_2).
% OTP 25.0
-export([pad/2]).
-endif.

-ifndef(HAVE_pubkey_ssh__decode_2).
decode(Bin, rfc4716_public_key) -> ssh_file:decode(Bin, rfc4716_key);
decode(Bin, new_openssh) -> ssh_file:decode(Bin, openssh_key_v1);
decode(Bin, openssh_public_key) -> ssh_file:decode(Bin, openssh_key);
decode(Bin, Type) -> ssh_file:decode(Bin, Type).
-endif.

-ifndef(HAVE_pubkey_ssh__encode_2).
encode(Bin, rfc4716_public_key) -> ssh_file:encode(Bin, rfc4716_key);
encode(Bin, new_openssh) -> ssh_file:encode(Bin, openssh_key_v1);
encode(Bin, openssh_public_key) -> ssh_file:encode(Bin, openssh_key);
encode(Bin, Type) -> ssh_file:encode(Bin, Type).
-endif.

-ifndef(HAVE_pubkey_ssh__new_openssh_decode_1).
-ifdef(HAVE_ssh_file__decode_2).
new_openssh_decode(Bin) -> ssh_file:decode(Bin, openssh_key_v1).
-else.
new_openssh_decode(<<"openssh-key-v1", 0, 4:32, "none", 4:32, "none", 0:32, 1:32, L4:32, _:L4, _:32,
                     CheckInt:32, CheckInt:32, Lt:32, Type:Lt, Lpu:32, PubKey:Lpu, Lpripub:32, PrivPubKey:Lpripub,
                     C1:32, _Comment:C1, _Pad/binary>>) ->
    new_openssh_decode(Type, PubKey, PrivPubKey).

-compile({inline, new_openssh_decode/3}).
new_openssh_decode(<<"ssh-ed25519">>, PubKey, <<PrivKey:32/binary, PubKey:32/binary>>) ->
    {ed_pri, ed25519, PubKey, PrivKey};
new_openssh_decode(<<"ssh-ed448">>, PubKey, <<PrivKey:57/binary, PubKey/binary>>) -> {ed_pri, ed448, PubKey, PrivKey}.
-endif.
-endif.

-ifndef(HAVE_pubkey_ssh__new_openssh_encode_1).
-ifdef(HAVE_ssh_file__encode_2).
new_openssh_encode(Bin) -> ssh_file:encode(Bin, openssh_key_v1).
-else.
-define(STRING(X), (byte_size(X)):32, X/binary).
-define(CHECK_INT, (17 * 256 + 17)). % crypto:strong_rand_bytes(4),

new_openssh_encode({ed_pri, ed25519, PubKey, PrivKey}) -> new_openssh_encode(<<"ssh-ed25519">>, PubKey, PrivKey);
new_openssh_encode({ed_pri, ed448, PubKey, PrivKey}) -> new_openssh_encode(<<"ssh-ed448">>, PubKey, PrivKey).

new_openssh_encode(Type, PubKey, PrivKey) ->
    PublicKey = <<?STRING(Type), ?STRING(PubKey)>>,
    Keys = <<PrivKey/binary, PubKey/binary>>,
    Encrypted0 = <<?CHECK_INT:32, ?CHECK_INT:32, ?STRING(Type), ?STRING(PubKey), ?STRING(Keys), 0:32>>,
    Encrypted = <<Encrypted0/binary, (pad(byte_size(Encrypted0), 8))/binary>>,
    <<"openssh-key-v1", 0, 4:32, "none", 4:32, "none", 0:32, 1:32, ?STRING(PublicKey), ?STRING(Encrypted)>>.
-endif.
-endif.

-ifndef(HAVE_pubkey_pad_2).
pad(N, BlockSize) when N > BlockSize -> pad(N rem BlockSize, BlockSize);
pad(N, BlockSize) -> list_to_binary(lists:seq(1, BlockSize - N)).
-endif.
