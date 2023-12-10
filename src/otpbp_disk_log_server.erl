-module(otpbp_disk_log_server).

-ifndef(HAVE_disk_log_server__all_0).
% OTP 24.0
-export([all/0]).
-endif.

-ifndef(HAVE_disk_log_server__all_0).
all() ->
    disk_log_server:start(),
    lists:sort(ets:select(disk_log_names, [{{'$1', '_'}, [], ['$1']}])).
-endif.
