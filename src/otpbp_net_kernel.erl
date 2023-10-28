-module(otpbp_net_kernel).

-ifndef(HAVE_net_kernel__start_2).
% OTP 24.3
-export([start/2]).
-endif.

-ifndef(HAVE_net_kernel__start_2).
start(Name, #{name_domain := NameDomain, net_ticktime := NetTickTime}) ->
    net_kernel:start([Name, NameDomain, NetTickTime]);
start(Name, #{name_domain := NameDomain}) -> net_kernel:start([Name, NameDomain]);
start(Name, #{}) -> net_kernel:start([Name]).
-endif.
