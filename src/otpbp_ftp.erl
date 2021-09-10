-module(otpbp_ftp).

-ifndef(HAVE_ftp__start_service_1).
% OTP < 26.0
-export([start_service/1]).
-endif.

-ifndef(HAVE_ftp__start_service_1).
start_service(Options) ->
    case lists:keytake(host, 1, Options) of
        {value, {_host, Host}, O} -> ftp:open(Host, O);
        _false -> {error, eclosed}
    end.
-endif.
