-module(otpbp_httpc).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_httpc__ssl_verify_host_options).
% OTP 25.1
-export([ssl_verify_host_options/1]).
-endif.

-ifndef(HAVE_httpc__ssl_verify_host_options).
ssl_verify_host_options(WildcardHostName) -> ssl_verify_host_options(WildcardHostName, public_key:cacerts_get()).

-compile({inline, ssl_verify_host_options/2}).
ssl_verify_host_options(false, CACerts) -> [{verify, verify_peer}, {cacerts, CACerts}];
ssl_verify_host_options(true, CACerts) ->
    [{verify, verify_peer}, {cacerts, CACerts},
     {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}].
-endif.
