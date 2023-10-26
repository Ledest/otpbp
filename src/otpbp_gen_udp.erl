-module(otpbp_gen_udp).

-compile({parse_transform, otpbp_pt}).

-ifndef(HAVE_gen_udp__connect_2).
% OTP 24.3
-export([connect/2]).
-endif.
-ifndef(HAVE_gen_udp__send_3).
% OTP 24.3
-export([send/3]).
-endif.
-ifndef(HAVE_gen_udp__send_5).
% OTP 24.3
-export([send/5]).
-endif.

-ifndef(HAVE_gen_udp__send_3).
-ifdef(HAVE_gen_udp__send_5).
-import(gen_udp, [send/5]).
-endif.
-endif.

-ifndef(HAVE_gen_udp__connect_2).
connect(S, SockAddr) when is_port(S), is_map(SockAddr) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:connect(S, inet:ensure_sockaddr(SockAddr));
        Error -> Error
    end.
-endif.

-define(module_socket(Handler, Handle), {'$inet', Handler, Handle}).

-ifndef(HAVE_gen_udp__send_3).
send(?module_socket(GenUdpMod, _) = S, Destination, Packet) when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Destination, Packet);
send(Socket, Destination, Packet) -> send(Socket, Destination, [], Packet).

send(S, #{family := Fam} = Destination, AncData, Packet)
  when is_port(S), Fam =:= inet orelse Fam =:= inet6, is_list(AncData) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:send(S, inet:ensure_sockaddr(Destination), AncData, Packet);
        Error -> Error
    end;
send(S, {_, _} = Destination, 0, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:send(S, Destination, [], Packet);
        Error -> Error
    end;
send(S, {_, _}, PortZero, _Packet) when is_port(S), is_integer(PortZero) -> {error, einval};
send(S, {_, _} = Destination, AncData, Packet) when is_port(S), is_list(AncData) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:send(S, Destination, AncData, Packet);
        Error -> Error
    end;
send(S, Host, Port, Packet) when is_port(S) -> send(S, Host, Port, [], Packet).
-endif.

-ifndef(HAVE_gen_udp__send_5).
send(?module_socket(GenUdpMod, _) = S, Host, Port, AncData, Packet) when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Host, Port, AncData, Packet);
send(S, Host, Port, AncData, Packet) when is_port(S), is_list(AncData) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} ->
            case Mod:getaddr(Host) of
                {ok, IP} ->
                    case Mod:getserv(Port) of
                        {ok, P} -> Mod:send(S, {IP, P}, AncData, Packet);
                        {error, einval} -> exit(badarg);
                        Error -> Error
                    end;
                {error, einval} -> exit(badarg);
                Error -> Error
            end;
        Error -> Error
    end.
-endif.
