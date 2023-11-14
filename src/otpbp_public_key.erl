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
-ifndef(HAVE_public_key__pkix_verify_hostname_2).
% OTP 19.3
-export([pkix_verify_hostname/2]).
-endif.
-ifndef(HAVE_public_key__pkix_verify_hostname_3).
% OTP 19.3
-export([pkix_verify_hostname/3]).
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

-ifndef(HAVE_public_key__pkix_verify_hostname_2).
-ifdef(HAVE_public_key__pkix_verify_hostname_3).
-import(public_key, [pkix_verify_hostname/3]).
-endif.
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

-ifndef(HAVE_public_key__pkix_verify_hostname_2).
pkix_verify_hostname(Cert, ReferenceIDs) -> pkix_verify_hostname(Cert, ReferenceIDs, []).
-endif.

-ifndef(HAVE_public_key__pkix_verify_hostname_3).
pkix_verify_hostname(BinCert, ReferenceIDs, Options) when is_binary(BinCert) ->
    pkix_verify_hostname(public_key:pkix_decode_cert(BinCert, otp), ReferenceIDs, Options);

pkix_verify_hostname(Cert = #'OTPCertificate'{tbsCertificate = TbsCert}, ReferenceIDs0, Opts) ->
    ExtVals = try lists:keyfind(?'id-ce-subjectAltName', #'Extension'.extnID, TbsCert#'OTPTBSCertificate'.extensions) of
                  #'Extension'{extnValue = EV} -> EV;
                  false -> []
              catch
                  _:_ -> []
              end,
    case [{T, to_string(V)} || {T, V} <- ExtVals] of
        [] ->
            case TbsCert#'OTPTBSCertificate'.subject of
                {rdnSequence, RDNseq} ->
                    PresentedCNs = [{cn, to_string(V)} || ATVs <- RDNseq,
                                                          #'AttributeTypeAndValue'{type = ?'id-at-commonName',
                                                                                   value = {_T, V}} <- ATVs],
                    verify_hostname_match_loop(verify_hostname_fqnds(reference_ids(ReferenceIDs0), fqdn_fun(Opts)),
                                               PresentedCNs, match_fun(Opts), fail_callback(Opts), Cert);
            _ -> false
            end;
        PresentedIDs ->
            MatchFun = match_fun(Opts),
            FailCB = fail_callback(Opts),
            ReferenceIDs = reference_ids(ReferenceIDs0),
            case verify_hostname_match_loop(ReferenceIDs, PresentedIDs, MatchFun, FailCB, Cert) of
                false ->
                    verify_hostname_match_loop([{dns_id, X} || X <- verify_hostname_fqnds(ReferenceIDs, fqdn_fun(Opts))],
                                               PresentedIDs, MatchFun, FailCB, Cert);
                true -> true
            end
    end.

match_fun(Opts) -> proplists:get_value(match_fun, Opts, undefined).

fail_callback(Opts) -> proplists:get_value(fail_callback, Opts, fun(_Cert) -> false end).

fqdn_fun(Opts) -> proplists:get_value(fqdn_fun, Opts, fun verify_hostname_extract_fqdn_default/1).

reference_ids(ReferenceIDs) -> [{T, to_string(V)} || {T, V} <- ReferenceIDs].

verify_hostname_fqnds(L, FqdnFun) ->
    lists:filtermap(fun(E0) ->
                        try verify_hostname_fqnds_(E0, FqdnFun) of
                            [_|_] = E -> inet:parse_address(E) =:= {error, einval} andalso {true, E};
                            _ -> false
                        catch
                            _:_ -> false
                        end
                    end, L).

-compile({inline, verify_hostname_fqnds_/2}).
verify_hostname_fqnds_(E, FqdnFun) ->
    case FqdnFun(E) of
        default -> verify_hostname_extract_fqdn_default(E);
        Other -> Other
    end.

verify_hostname_extract_fqdn_default({dns_id, S}) -> S;
verify_hostname_extract_fqdn_default({uri_id, URI}) ->
    #{scheme := "https", host := Host} = uri_string:normalize(URI, [return_map]),
    Host.

to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(X) -> X.

verify_hostname_match_loop(Refs, Pres0, undefined, FailCB, Cert) ->
    Pres = lists:map(fun to_lower_ascii/1, Pres0),
    lists:any(fun(R) -> lists:any(fun(P) -> verify_hostname_match_default(R, P) orelse FailCB(Cert) end, Pres) end,
              lists:map(fun to_lower_ascii/1, Refs));
verify_hostname_match_loop(Refs, Pres, MatchFun, FailCB, Cert) ->
    lists:any(fun(R) ->
                  lists:any(fun(P) ->
                                case MatchFun(R, P) of
                                    default -> verify_hostname_match_default(R, P);
                                    Bool -> Bool
                                end orelse FailCB(Cert)
                            end,
                            Pres)
              end,
              Refs).

verify_hostname_match_default(Ref, Pres) -> verify_hostname_match_default_(to_lower_ascii(Ref), to_lower_ascii(Pres)).

-ifndef(srvName_OID).
-define(srvName_OID, {1, 3, 6, 1, 4, 1, 434, 2, 2, 1, 37, 0}).
-endif.

-compile({inline, verify_hostname_match_default_/2}).
verify_hostname_match_default_([_|_] = FQDN, {cn, FQDN}) -> not lists:member($*, FQDN);
verify_hostname_match_default_([_|_] = FQDN, {cn, [_|_] = Name}) -> verify_hostname_match_wildcard(FQDN, Name);
verify_hostname_match_default_({dns_id, R}, {dNSName, P}) -> R =:= P;
verify_hostname_match_default_({uri_id, R}, {uniformResourceIdentifier, P}) -> R =:= P;
verify_hostname_match_default_({ip, {A, B, C, D}}, {iPAddress, [A, B, C, D]}) -> true;
verify_hostname_match_default_({ip, R}, {iPAddress, [A, B, C, D] = P}) when is_list(R) ->
    case inet:parse_ipv4strict_address(R) of
        {ok, {A, B, C, D}} -> true;
        _ -> false
    end;
verify_hostname_match_default_({ip, R}, {iPAddress, P}) when length(P) =:= 16 ->
    try l16_to_tup(P) of
        Pt when tuple_size(R) =:= 8 -> Pt =:= R;
        Pt -> inet:parse_ipv6strict_address(R) =:= {ok, Pt}
    catch
        _:_ -> false
    end;
verify_hostname_match_default_({srv_id, R}, {T, P}) when T =:= srvName; T =:= ?srvName_OID -> R =:= P;
verify_hostname_match_default_(_, _) -> false.

-compile({inline, l16_to_tup/1}).
l16_to_tup(L) -> list_to_tuple(l16_to_tup(L, [])).

l16_to_tup([A, B|T], Acc) -> l16_to_tup(T, [(A bsl 8) bor B|Acc]);
l16_to_tup([], Acc) -> lists:reverse(Acc).

-ifndef(NEED_verify_hostname_match_wildcard_2).
-define(NEED_verify_hostname_match_wildcard_2, true).
-endif.
-ifndef(NEED_to_lower_ascii_1).
-define(NEED_to_lower_ascii_1, true).
-endif.
-endif.

-ifndef(HAVE_public_key__pkix_verify_hostname_match_fun_1).
pkix_verify_hostname_match_fun(https) ->
    fun({dns_id, [_|_] = FQDN}, {dNSName, [_|_] = Name}) -> verify_hostname_match_wildcard(FQDN, Name);
       (_, _) -> default
    end.

-ifndef(NEED_verify_hostname_match_wildcard_2).
-define(NEED_verify_hostname_match_wildcard_2, true).
-endif.
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

format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) -> {rsa, [Exp, Mod]};
format_verify_key({{'ECPoint', Point}, Param}) -> {ecdsa, [Point, ec_curve_spec(Param)]};
format_verify_key({Key,  #'Dss-Parms'{p = P, q = Q, g = G}}) -> {dss, [P, Q, G, Key]};
format_verify_key({ed_pub, Curve, Key}) -> {eddsa, [Key, Curve]};
%% Convert private keys to public keys
format_verify_key(#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp}) ->
    format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = {_, Point}}) ->
    format_verify_key({{'ECPoint', Point}, Param});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = Point}) ->
    format_verify_key({{'ECPoint', Point}, Param});
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
ec_curve_spec({namedCurve, Name}) when is_atom(Name) -> Name.
-endif.

-ifdef(NEED_verify_hostname_match_wildcard_2).
verify_hostname_match_wildcard(FQDN, Name) ->
    [[F1|Fs], [N1|Ns]] = [string:tokens(to_lower_ascii(S), ".") || S <- [FQDN, Name]],
    match_wild(F1, N1) andalso Fs =:= Ns.

match_wild(A, [$*|B]) -> match_wild_sfx(lists:reverse(A), lists:reverse(B));
match_wild([C|A], [C|B]) -> match_wild(A, B);
match_wild(A, B) -> A =:= [] andalso B =:= [].

match_wild_sfx([$*|_], _) -> false; % Bad name (no wildcards allowed)
match_wild_sfx(_, [$*|_]) -> false; % Bad pattern (no more wildcards allowed)
match_wild_sfx([A|Ar], [A|Br]) -> match_wild_sfx(Ar, Br);
match_wild_sfx(Ar, []) -> not lists:member($*, Ar); % Chk for bad name (= wildcards)
match_wild_sfx(_, _) -> false.

-ifndef(NEED_to_lower_ascii_1).
-define(NEED_to_lower_ascii_1, true).
-endif.
-endif.

-ifdef(NEED_to_lower_ascii_1).
to_lower_ascii(S) when is_list(S) -> lists:map(fun to_lower_ascii/1, S);
to_lower_ascii({T, _} = X) when T =:= ip; T =:= iPAddress -> X;
to_lower_ascii({T, S}) -> {T, to_lower_ascii(S)};
to_lower_ascii(C) when C >= $A, C =< $Z -> C + ($a - $A);
to_lower_ascii(C) -> C.
-endif.
