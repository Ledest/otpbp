-module(otpbp_pubkey_ocsp).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-ifndef(HAVE_pubkey_ocsp__otp_cert_1).
% OTP < 26.0
-export([otp_cert/1]).
-endif.

-ifndef(HAVE_pubkey_ocsp__otp_cert_1).
otp_cert(#'OTPCertificate'{} = Cert) -> Cert;
otp_cert(#'Certificate'{} = Cert) -> public_key:pkix_decode_cert(public_key:der_encode('Certificate', Cert), otp);
otp_cert(CertDer) when is_binary(CertDer) -> public_key:pkix_decode_cert(CertDer, otp).
-endif.
