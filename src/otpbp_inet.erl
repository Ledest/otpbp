-module(otpbp_inet).

-ifndef(HAVE_inet__ipv4_mapped_ipv6_address_1).
% OTP 21.0
-export([ipv4_mapped_ipv6_address/1]).
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

-ifndef(HAVE_inet__ipv4_mapped_ipv6_address_1).
ipv4_mapped_ipv6_address({D1, D2, D3, D4}) when (D1 bor D2 bor D3 bor D4) =< 16#FF ->
    {0, 0, 0, 0, 0, 16#FFFF, (D1 bsl 8) bor D2, (D3 bsl 8) bor D4};
ipv4_mapped_ipv6_address({D1, D2, D3, D4, D5, D6, D7, D8})
  when (D1 bor D2 bor D3 bor D4 bor D5 bor D6 bor D7 bor D8) =< 16#FFFF ->
    {D7 bsr 8, D7 band 16#FF, D8 bsr 8, D8 band 16#FF}.
-endif.

-ifndef(HAVE_inet__ensure_sockaddr_1).
ensure_sockaddr(SockAddr) ->
    try
        prim_socket:enc_sockaddr(SockAddr)
    catch
        throw:{invalid, Invalid}:Stacktrace -> erlang:raise(error, {invalid, Invalid}, Stacktrace)
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
