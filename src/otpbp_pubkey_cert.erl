-module(otpbp_pubkey_cert).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-ifndef(HAVE_pubkey_cert__root_cert_2).
% OTP 20.2
-export([root_cert/2]).
-endif.
-ifndef(HAVE_pubkey_cert__subject_id_1).
% OTP 23.1
-export([subject_id/1]).
-endif.

-ifndef(HAVE_pubkey_cert__root_cert_2).
root_cert(Name, Opts) ->
    PrivKey = gen_key(proplists:get_value(key, Opts, default_key_gen())),
    TBS = cert_template(),
    Issuer = subject("root", Name),
    SignatureId = sign_algorithm(PrivKey, Opts),
    #{cert => public_key:pkix_sign(TBS#'OTPTBSCertificate'{signature = SignatureId,
                                                           issuer = Issuer,
                                                           validity = validity(Opts),
                                                           subject = Issuer,
                                                           subjectPublicKeyInfo = public_key(PrivKey, SignatureId),
                                                           extensions = extensions(undefined, ca, Opts)},
                                   PrivKey),
      key => PrivKey}.

-compile({inline, default_key_gen/0}).
default_key_gen() ->
    case crypto:ec_curves() of
        [] -> {rsa, 2048, 17};
        [Curve|_] -> {namedCurve, pubkey_cert_records:namedCurves(Curve)}
    end.

-compile({inline, gen_key/1}).
gen_key(KeyGen) ->
     case is_key(KeyGen) of
         true -> KeyGen;
         false -> public_key:generate_key(KeyGen)
     end.

-compile({inline, is_key/1}).
is_key(#'DSAPrivateKey'{}) -> true;
is_key(#'RSAPrivateKey'{}) -> true;
is_key({#'RSAPrivateKey'{}, _}) -> true;
is_key(#'ECPrivateKey'{}) -> true;
is_key(_) -> false.

-compile({inline, cert_template/0}).
cert_template() ->
    #'OTPTBSCertificate'{version = v3,
                         serialNumber = erlang:unique_integer([positive, monotonic]),
                         issuerUniqueID = asn1_NOVALUE,
                         subjectUniqueID = asn1_NOVALUE}.

-compile({inline, subject/2}).
subject(Contact, Name) ->
    subject([{email, Contact ++ "@example.org"},
             {name,  Name},
             {city, "Stockholm"},
             {country, "SE"},
             {org, "erlang"},
             {org_unit, "automated testing"}]).

-compile({inline, subject/1}).
subject(SubjectOpts) when is_list(SubjectOpts) ->
    {rdnSequence,
     lists:map(fun(Opt) ->
                   {Type, Value} = subject_enc(Opt),
                   [#'AttributeTypeAndValue'{type = Type, value = Value}]
               end,
               SubjectOpts)}.

-compile({inline, subject_enc/1}).
subject_enc({name,  Name}) -> {?'id-at-commonName', {printableString, Name}};
subject_enc({email, Email}) -> {?'id-emailAddress', Email};
subject_enc({city,  City}) -> {?'id-at-localityName', {printableString, City}};
subject_enc({org, Org}) -> {?'id-at-organizationName', {printableString, Org}};
subject_enc({org_unit, OrgUnit}) -> {?'id-at-organizationalUnitName', {printableString, OrgUnit}};
subject_enc({country, Country}) -> {?'id-at-countryName', Country}.

-compile({inline, sign_algorithm/2}).
sign_algorithm(#'RSAPrivateKey'{} = Key , Opts) ->
    rsa_sign_algo(Key,
                  rsa_digest_oid(proplists:get_value(digest, Opts,
                                                     case proplists:get_value(rsa_padding, Opts,
                                                                              rsa_pkcs1_pss_padding) of
                                                         rsa_pkcs1_pss_padding -> sha1;
                                                         rsa_pss_rsae -> sha256
                                                     end)),
                  'NULL');
sign_algorithm({#'RSAPrivateKey'{} = Key, #'RSASSA-PSS-params'{} = Params}, _Opts) ->
    rsa_sign_algo(Key, ?'id-RSASSA-PSS', Params);
sign_algorithm(#'DSAPrivateKey'{p = P, q = Q, g = G}, _Opts) ->
    #'SignatureAlgorithm'{algorithm  = ?'id-dsa-with-sha1', parameters = {params, #'Dss-Parms'{p = P, q = Q, g = G}}};
sign_algorithm(#'ECPrivateKey'{parameters = Parms}, Opts) ->
    #'SignatureAlgorithm'{algorithm  = ecdsa_digest_oid(proplists:get_value(digest, Opts, sha1)), parameters = Parms}.

rsa_sign_algo(#'RSAPrivateKey'{}, ?'id-RSASSA-PSS',  #'RSASSA-PSS-params'{} = Params) ->
    #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS', parameters = Params};
rsa_sign_algo(#'RSAPrivateKey'{}, Type, Parms) -> #'SignatureAlgorithm'{algorithm  = Type, parameters = Parms}.

-compile({inline, rsa_digest_oid/1}).
rsa_digest_oid(Oid) when is_tuple(Oid) -> Oid;
rsa_digest_oid(Oid) when Oid =:= sha1; Oid =:= sha -> ?'sha1WithRSAEncryption';
rsa_digest_oid(sha512) -> ?'sha512WithRSAEncryption';
rsa_digest_oid(sha384) -> ?'sha384WithRSAEncryption';
rsa_digest_oid(sha256) -> ?'sha256WithRSAEncryption';
rsa_digest_oid(md5) -> ?'md5WithRSAEncryption'.

-compile({inline, ecdsa_digest_oid/1}).
ecdsa_digest_oid(Oid) when is_tuple(Oid) -> Oid;
ecdsa_digest_oid(Oid) when Oid =:= sha1; Oid =:= sha -> ?'ecdsa-with-SHA1';
ecdsa_digest_oid(sha512) -> ?'ecdsa-with-SHA512';
ecdsa_digest_oid(sha384) -> ?'ecdsa-with-SHA384';
ecdsa_digest_oid(sha256) -> ?'ecdsa-with-SHA256'.


-compile({inline, validity/1}).
validity(Opts) ->
    {DefFrom, DefTo} = proplists:get_value(validity, Opts,
                                           {calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date()) - 1),
                                            calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date()) + 7)}),
    GenFormat = fun({Y, M, D}) -> lists:flatten(io_lib:format("~4..0w~2..0w~2..0w130000Z", [Y, M, D])) end,
    UTCFormat = fun({Y, M, D}) ->
                    [_, _, Y3, Y4] = integer_to_list(Y),
                    lists:flatten(io_lib:format("~s~2..0w~2..0w130000Z", [[Y3, Y4], M, D]))
                end,
    #'Validity'{notBefore = validity_format(DefFrom, GenFormat, UTCFormat),
                notAfter = validity_format(DefTo, GenFormat, UTCFormat)}.

validity_format({Year, _, _} = Validity, GenFormat, _UTCFormat) when Year >= 2049 ->
    {generalTime, GenFormat(Validity)};
validity_format(Validity, _GenFormat, UTCFormat) -> {utcTime, UTCFormat(Validity)}.

-ifndef('id-Ed448').
-define('id-Ed448', {1, 3, 101, 113}).
-endif.
-ifndef('id-Ed25519').
-define('id-Ed25519', {1, 3, 101, 112}).
-endif.

-record('ECPoint', {point}).

-compile({inline, public_key/2}).
public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E},
           #'SignatureAlgorithm'{algorithm  = ?rsaEncryption, parameters = #'RSASSA-PSS-params'{} = Params}) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?rsaEncryption, parameters = Params},
                               subjectPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}};
public_key({#'RSAPrivateKey'{modulus = N, publicExponent = E}, #'RSASSA-PSS-params'{} = Params},
           #'SignatureAlgorithm'{algorithm  = ?'id-RSASSA-PSS', parameters = #'RSASSA-PSS-params'{} = Params}) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-RSASSA-PSS', parameters = Params},
                               subjectPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}};
public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E}, _) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?rsaEncryption, parameters = 'NULL'},
                               subjectPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}};
public_key(#'DSAPrivateKey'{p = P, q = Q, g = G, y = Y}, _) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-dsa',
                                                                 parameters = {params,
                                                                               #'Dss-Parms'{p = P, q = Q, g = G}}},
                               subjectPublicKey = Y};
public_key(#'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed25519' = ID}, publicKey = PubKey}, _) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ID, parameters = asn1_NOVALUE},
                               subjectPublicKey = #'ECPoint'{point = PubKey}};
public_key(#'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed448' = ID}, publicKey = PubKey}, _) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ID, parameters = asn1_NOVALUE},
                               subjectPublicKey = #'ECPoint'{point = PubKey}};
public_key(#'ECPrivateKey'{parameters = Params, publicKey = PubKey}, _) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ecPublicKey', parameters = Params},
                               subjectPublicKey = #'ECPoint'{point = PubKey}}.

-compile({inline, extensions/3}).
extensions(Role, Type, Opts) -> add_default_extensions(Role, Type, proplists:get_value(extensions, Opts, [])).

-compile({inline, add_default_extensions/3}).
add_default_extensions(_, ca, Exts) ->
    add_default_extensions([#'Extension'{extnID = ?'id-ce-keyUsage', extnValue = [keyCertSign, cRLSign],
                                         critical = false},
                            #'Extension'{extnID = ?'id-ce-basicConstraints', extnValue = #'BasicConstraints'{cA = true},
                                         critical = true}],
                           Exts);
add_default_extensions(server, peer, Exts) ->
    add_default_extensions([#'Extension'{extnID = ?'id-ce-keyUsage', extnValue = [digitalSignature, keyAgreement],
                                         critical = false},
                            #'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = [{dNSName, net_adm:localhost()}],
                                         critical = false}],
                           Exts);
add_default_extensions(client, peer, Exts) -> Exts.

add_default_extensions(Defaults, Exts) ->
    Exts ++ [Ext || #'Extension'{extnID = ID} = Ext <- Defaults, not lists:keymember(ID, 2, Exts)].
-endif.

-ifndef(HAVE_pubkey_cert__subject_id_1).
subject_id(#'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Subject, serialNumber = SerialNr}}) ->
    {SerialNr, pubkey_cert:normalize_general_name(Subject)}.
-endif.
