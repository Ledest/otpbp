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
-ifndef(HAVE_public_key__pkix_test_data_1).
% OTP 20.1
-export([pkix_test_data/1]).
-endif.
-ifndef(HAVE_public_key__pkix_test_root_cert_2).
% OTP 20.2
-export([pkix_test_root_cert/2]).
-endif.
-ifndef(HAVE_public_key__pkix_verify_hostname_match_fun_1).
% OTP 21.0
-export([pkix_verify_hostname_match_fun/1]).
-endif.
-ifndef(HAVE_public_key__sign_4).
% OTP 20.1
-export([sign/4]).
-endif.
-ifndef(HAVE_public_key__verify_5).
% OTP 20.1
-export([verify/5]).
-endif.

-ifndef('id-Ed448').
-define('id-Ed448', {1, 3, 101, 113}).
-endif.
-ifndef('id-Ed25519').
-define('id-Ed25519', {1, 3, 101, 112}).
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

-ifndef(NEED_format_rsa_private_key_1).
-define(NEED_format_rsa_private_key_1, true).
-endif.
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

-ifndef(HAVE_public_key__pkix_test_data_1).
pkix_test_data(#{client_chain := ClientChain, server_chain := ServerChain}) ->
    Default = #{intermediates => []},
    pubkey_cert:gen_test_certs(#{client_chain => maps:merge(Default, ClientChain),
                                 server_chain => maps:merge(Default, ServerChain)});
pkix_test_data(Chain) when is_map(Chain) -> pubkey_cert:gen_test_certs(maps:merge(#{intermediates => []}, Chain)).
-endif.

-ifndef(HAVE_public_key__pkix_test_root_cert_2).
pkix_test_root_cert(Name, Opts) -> pubkey_cert:root_cert(Name, Opts).
-endif.

-ifndef(HAVE_public_key__pkix_verify_hostname_match_fun_1).
pkix_verify_hostname_match_fun(https) ->
    fun({dns_id, [_|_] = FQDN}, {dNSName, [_|_] = Name}) -> verify_hostname_match_wildcard(FQDN, Name);
       (_, _) -> default
    end.

-compile({inline, verify_hostname_match_wildcard/2}).
verify_hostname_match_wildcard(FQDN, Name) ->
    [[F1|Fs], [N1|Ns]] = [string:tokens(to_lower_ascii(S), ".") || S <- [FQDN, Name]],
    match_wild(F1, N1) andalso Fs =:= Ns.

to_lower_ascii(S) when is_list(S) -> lists:map(fun to_lower_ascii/1, S);
to_lower_ascii({T, _} = X) when T =:= ip; T =:= iPAddress -> X;
to_lower_ascii({T, S}) -> {T, to_lower_ascii(S)};
to_lower_ascii(C) when C >= $A, C =< $Z -> C + ($a - $A);
to_lower_ascii(C) -> C.

match_wild(A, [$*|B]) -> match_wild_sfx(lists:reverse(A), lists:reverse(B));
match_wild([C|A], [C|B]) -> match_wild(A, B);
match_wild(A, B) -> A =:= [] andalso B =:= [].

match_wild_sfx([$*|_], _) -> false; % Bad name (no wildcards allowed)
match_wild_sfx(_, [$*|_]) -> false; % Bad pattern (no more wildcards allowed)
match_wild_sfx([A|Ar], [A|Br]) -> match_wild_sfx(Ar, Br);
match_wild_sfx(Ar, []) -> not lists:member($*, Ar); % Chk for bad name (= wildcards)
match_wild_sfx(_, _) -> false.
-endif.

-ifndef(HAVE_public_key__sign_4).
sign(Digest, none, Key = #'DSAPrivateKey'{}, []) when is_binary(Digest) ->
    sign({digest, Digest}, sha, Key, []);
sign(DigestOrPlainText, DigestType, Key, []) ->
    case format_sign_key(Key) of
        badarg -> error(badarg, [DigestOrPlainText, DigestType, Key, []]);
        {Algorithm, CryptoKey} ->
            try
                crypto:sign(Algorithm, DigestType, DigestOrPlainText, CryptoKey)
            catch
                error:{R, _, _} when R =:= notsup; R =:= error; R =:= badarg -> error(R)
            end
    end.

format_sign_key(#'RSAPrivateKey'{} = Key) -> {rsa, format_rsa_private_key(Key)};
format_sign_key(#'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) -> {dss, [P, Q, G, X]};
format_sign_key(#'ECPrivateKey'{privateKey = PrivKey, parameters = {namedCurve, Curve} = Param})
  when Curve =:= ?'id-Ed25519'; Curve =:= ?'id-Ed448' ->
    {eddsa, [PrivKey, ec_curve_spec(Param)]};
format_sign_key(#'ECPrivateKey'{privateKey = PrivKey, parameters = Param}) -> {ecdsa, [PrivKey, ec_curve_spec(Param)]};
format_sign_key({ed_pri, Curve, _Pub, Priv}) -> {eddsa, [Priv, Curve]};
format_sign_key(_) -> badarg.

format_field(characteristic_two_field = Type, Params0) ->
    #'Characteristic-two'{m = M, basis = BasisOid, parameters = Params} = public_key:der_decode('Characteristic-two',
                                                                                                Params0),
    {Type, M, field_param_decode(BasisOid, Params)};
format_field(prime_field, Params) -> {prime_field, public_key:der_decode('Prime-p', Params)}.

field_param_decode(?ppBasis, Params) ->
    #'Pentanomial'{k1 = K1, k2 = K2, k3 = K3} = public_key:der_decode('Pentanomial', Params),
    {ppbasis, K1, K2, K3};
field_param_decode(?tpBasis, Params) -> {tpbasis, public_key:der_decode('Trinomial', Params)};
field_param_decode(?gnBasis, _) -> onbasis.

-ifndef(NEED_format_rsa_private_key_1).
-define(NEED_format_rsa_private_key_1, true).
-endif.
-ifndef(NEED_format_ec_curve_spec_1).
-define(NEED_format_ec_curve_spec_1, true).
-endif.
-endif.

-ifndef(HAVE_public_key__verify_5).
verify(Digest, none, Signature, {_, #'Dss-Parms'{}} = Key, []) when is_binary(Digest) ->
    verify({digest, Digest}, sha, Signature, Key, []);
verify(DigestOrPlainText, DigestType, Signature, Key, []) when is_binary(Signature) ->
    is_binary(Signature) andalso
        case format_verify_key(Key) of
            badarg -> error(badarg, [DigestOrPlainText, DigestType, Signature, Key, []]);
            {Algorithm, CryptoKey} ->
                try
                    crypto:verify(Algorithm, DigestType, DigestOrPlainText, Signature, CryptoKey)
                catch
                    error:{R, _, _} when R =:= notsup; R =:= error; R =:= badarg -> error(R)
                end
        end.

-record('ECPoint', {point}).

format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) -> {rsa, [Exp, Mod]};
format_verify_key({#'ECPoint'{point = Point}, {namedCurve, Curve} = Param})
  when Curve =:= ?'id-Ed25519'; Curve =:= ?'id-Ed448' ->
    {eddsa, [Point, ec_curve_spec(Param)]};
format_verify_key({#'ECPoint'{point = Point}, Param}) -> {ecdsa, [Point, ec_curve_spec(Param)]};
format_verify_key({Key,  #'Dss-Parms'{p = P, q = Q, g = G}}) -> {dss, [P, Q, G, Key]};
format_verify_key({ed_pub, Curve, Key}) -> {eddsa, [Key, Curve]};
%% Convert private keys to public keys
format_verify_key(#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp}) ->
    format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = {_, Point}}) ->
    format_verify_key({#'ECPoint'{point = Point}, Param});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = Point}) ->
    format_verify_key({#'ECPoint'{point = Point}, Param});
format_verify_key(#'DSAPrivateKey'{y = Y, p = P, q = Q, g = G}) ->
    format_verify_key({Y, #'Dss-Parms'{p = P, q = Q, g = G}});
format_verify_key(_) -> badarg.

-ifndef(NEED_format_ec_curve_spec_1).
-define(NEED_format_ec_curve_spec_1, true).
-endif.
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

-ifdef(NEED_format_rsa_private_key_1).
format_rsa_private_key(#'RSAPrivateKey'{modulus = N, publicExponent = E, privateExponent = D, prime1 = P1, prime2 = P2,
                                        exponent1 = E1, exponent2 = E2, coefficient = C})
  when is_integer(N), is_integer(E), is_integer(D) ->
    if
        is_integer(P1), is_integer(P2), is_integer(E1), is_integer(E2), is_integer(C) -> [E, N, D, P1, P2, E1, E2, C];
        true -> [E, N, D]
    end.
-endif.

-ifdef(NEED_format_ec_curve_spec_1).
ec_curve_spec( #'ECParameters'{fieldID = #'FieldID'{fieldType = Type, parameters = Params},
                               curve = #'Curve'{a = A, b = B}, base = Base, order = Order, cofactor = CoFactor}) ->
    {format_field(pubkey_cert_records:supportedCurvesTypes(Type), Params), {A, B, none}, Base, Order, CoFactor};
ec_curve_spec({ecParameters, ECParams}) -> ec_curve_spec(ECParams);
ec_curve_spec({namedCurve, OID}) when is_tuple(OID), is_integer(element(1, OID)) ->
    ec_curve_spec({namedCurve,  pubkey_cert_records:namedCurves(OID)});
ec_curve_spec({namedCurve, Name}) when Name =:= x25519; Name =:= x448; Name =:= ed25519; Name =:= ed448 -> Name;
ec_curve_spec({namedCurve, Name}) when is_atom(Name) -> Name.
-endif.
