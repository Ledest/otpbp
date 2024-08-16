-module(otpbp_pubkey_cert).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-ifndef(HAVE_pubkey_cert__subject_id_1).
% OTP 23.1
-export([subject_id/1]).
-endif.

-ifndef(HAVE_pubkey_cert__subject_id_1).
subject_id(#'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Subject, serialNumber = SerialNr}}) ->
    {SerialNr, pubkey_cert:normalize_general_name(Subject)}.
-endif.
