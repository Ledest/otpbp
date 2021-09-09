-module(otpbp_disk_log).

-ifndef(HAVE_disk_log__accessible_logs_0).
% OTP < 26.0
-export([accessible_logs/0]).
-endif.
-ifndef(HAVE_disk_log__lclose_2).
% OTP < 26.0
-export([lclose/2]).
-endif.

-ifndef(HAVE_disk_log__accessible_logs_0).
accessible_logs() -> {disk_log:all(), []}.
-endif.

-ifndef(HAVE_disk_log__lclose_2).
lclose(Log, Node) when Node =:= node() -> disk_log:close(Log);
lclose(_Log, _Node) -> {error, no_such_log}.
-endif.
