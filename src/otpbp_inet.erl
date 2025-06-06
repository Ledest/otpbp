-module(otpbp_inet).

-ifndef(HAVE_inet__gen_tcp_module_1).
% OTP 23.0
-export([gen_tcp_module/1]).
-endif.
-ifndef(HAVE_inet__ensure_sockaddr_1).
% OTP 24.0
-export([ensure_sockaddr/1]).
-compile({parse_transform, otpbp_pt}).
-endif.
-ifndef(HAVE_inet__info_1).
% OTP 24.0
-export([info/1]).
-endif.
-ifndef(HAVE_inet__gen_udp_module_1).
% OTP 24.1
-export([gen_udp_module/1]).
-endif.
-ifndef(HAVE_inet__is_ip_address_1).
% OTP 25.0
-export([is_ip_address/1]).
-endif.
-ifndef(HAVE_inet__is_ipv4_address_1).
% OTP 25.0
-export([is_ipv4_address/1]).
-endif.
-ifndef(HAVE_inet__is_ipv6_address_1).
% OTP 25.0
-export([is_ipv6_address/1]).
-endif.

-ifndef(HAVE_inet__is_ip_address_1).
-ifdef(HAVE_inet__is_ipv4_address_1).
-import(inet, [is_ipv4_address/1]).
-endif.
-ifdef(HAVE_inet__is_ipv6_address_1).
-import(inet, [is_ipv6_address/1]).
-endif.
-endif.

-ifndef(HAVE_inet__gen_tcp_module_1).
gen_tcp_module(Opts) -> {gen_tcp, Opts}.
-endif.

-ifndef(HAVE_inet__ensure_sockaddr_1).
ensure_sockaddr(SockAddr) ->
    try
        prim_socket:enc_sockaddr(SockAddr)
    catch
        throw:{invalid, _} = Invalid:Stacktrace -> erlang:raise(error, Invalid, Stacktrace)
    end.
-endif.

-ifndef(HAVE_inet__info_1).
info({'$inet', GenSocketMod, _} = Socket) when is_atom(GenSocketMod) -> GenSocketMod:info(Socket);
info(Socket) ->
    maps:merge(port_info(Socket),
               #{counters => case inet:getstat(Socket) of
                                 {ok, Stats} -> maps:from_list(Stats);
                                 _ -> []
                             end,
                 states => case prim_inet:getstatus(Socket) of
                               {ok, State} -> State;
                               _ -> #{}
                           end}).

port_info(P) when is_port(P) ->
    maps:from_list([erlang:port_info(P, memory), erlang:port_info(P, monitors)
                    |port_info(erlang:port_info(P), [connected, links, input, output])]);
port_info(_) -> #{}.

port_info(PI, Items) -> port_info(PI, Items, []).

port_info(_PI, [], Acc) -> Acc;
port_info(PI, [Item | Items], Acc) -> port_info(PI, Items, [{Item, proplists:get_value(Item, PI)}|Acc]).
-endif.

-ifndef(HAVE_inet__gen_udp_module_1).
gen_udp_module(Opts) -> {gen_udp, Opts}.
-endif.

-ifndef(HAVE_inet__is_ip_address_1).
is_ip_address(Address) -> is_ipv4_address(Address) orelse is_ipv6_address(Address).
-endif.

-ifndef(HAVE_inet__is_ipv4_address_1).
-define(ip(A, B, C, D), (A bor B bor C bor D) band (bnot 16#ff) =:= 0).

is_ipv4_address({A, B, C, D}) when ?ip(A, B, C, D) -> true;
is_ipv4_address(_) -> false.
-endif.

-ifndef(HAVE_inet__is_ipv6_address_1).
-define(ip6(A, B, C, D, E, F, G, H), (A bor B bor C bor D bor E bor F bor G bor H) band (bnot 16#ffff) =:= 0).

is_ipv6_address({A, B, C, D, E, F, G, H}) when ?ip6(A, B, C, D, E, F, G, H) -> true;
is_ipv6_address(_) -> false.
-endif.
