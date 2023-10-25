-module(otpbp_erl_epmd).

-ifndef(HAVE_erl_epmd__listen_port_please_2).
% OTP 21.0
-export([listen_port_please/2]).
-endif.
-ifndef(HAVE_erl_epmd__port_please_2).
% OTP 21.0
-export([port_please/2]).
-endif.
-ifndef(HAVE_erl_epmd__port_please_3).
% OTP 21.0
-export([port_please/3]).
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/inet.hrl").
-endif.

-ifndef(HAVE_erl_epmd__port_please_2).
-ifdef(HAVE_erl_epmd__port_please_3).
-import(erl_epmd, [port_please/3]).
-endif.
-endif.
-ifndef(HAVE_erl_epmd__port_please_3).
-ifdef(HAVE_erl_epmd__listen_port_please_2).
-import(erl_epmd, [listen_port_please/2]).
-endif.
-endif.

-ifndef(HAVE_erl_epmd__listen_port_please_2).
listen_port_please(_Name, _Host) ->
    try
        {ok, [[StringPort]]} = init:get_argument(erl_epmd_port),
        {ok, list_to_integer(StringPort)}
    catch
        error:_ -> {ok, 0}
    end.
-endif.

-ifndef(HAVE_erl_epmd__port_please_2).
port_please(Node, HostName) -> port_please(Node, HostName, infinity).
-endif.

-ifndef(HAVE_erl_epmd__port_please_3).
-define(epmd_dist_high, ?ERL_DIST_VER_HIGH).
-define(epmd_dist_low, ?ERL_DIST_VER_LOW).

port_please(Node, HostName, Timeout) ->
    case listen_port_please(Node, HostName) of
        {ok, 0} ->
            case getepmdbyname(HostName, Timeout) of
                {ok, EpmdAddr} -> get_port(Node, EpmdAddr, Timeout);
                _Error -> noport
            end;
        {ok, Prt} -> {port, Prt, ?epmd_dist_low}
    end.

getepmdbyname(HostName, Timeout) when is_list(HostName) ->
    case inet:gethostbyname(HostName,
                            case inet_db:res_option(inet6) of
                                true -> inet6;
                                false -> inet
                            end,
                            Timeout) of
        {ok, #hostent{h_addr_list = [EpmdAddr|_]}} -> {ok, EpmdAddr};
        Else -> Else
    end;
getepmdbyname(HostName, Timeout) when is_atom(HostName) -> getepmdbyname(atom_to_list(HostName), Timeout);
getepmdbyname(HostName, _Timeout) -> {ok, HostName}.

-define(int16(X), [((X) bsr 8) band 16#FF, (X) band 16#FF]).
-define(EPMD_PORT_PLEASE2_REQ, $z).

get_port(Node, EpmdAddress, Timeout) ->
    case erl_epmd:open(EpmdAddress, Timeout) of
        {ok, Socket} ->
            Name = to_string(Node),
            case gen_tcp:send(Socket, [?int16(length(Name) + 1), ?EPMD_PORT_PLEASE2_REQ, Name]) of
                ok -> wait_for_port_reply(Socket, []);
                _Error -> noport
            end;
        _Error -> noport
    end.

to_string(S) when is_list(S) -> S;
to_string(S) when is_atom(S) -> atom_to_list(S).

-define(u16(X1, X0), (((X1) bsl 8) bor (X0))).

wait_for_port_reply(Socket, SoFar) ->
    receive
        {tcp, Socket, Data0} ->
            case SoFar ++ Data0 of
                [$w, 0|Rest] -> wait_for_port_reply_cont(Socket, Rest);
                [$w, _|_] -> wait_for_close(Socket, noport);
                [_, _|_] = Garbage -> {error, {garbage_from_epmd, Garbage}};
                Data -> wait_for_port_reply(Socket, Data)
            end;
        {tcp_closed, Socket} -> closed
    after 10000 ->
            gen_tcp:close(Socket),
            noport
    end.

wait_for_port_reply_cont(Socket, SoFar) when length(SoFar) >= 10 -> wait_for_port_reply_cont2(Socket, SoFar);
wait_for_port_reply_cont(Socket, SoFar) ->
    receive
        {tcp, Socket, Data0} ->
            case SoFar ++ Data0 of
                Data when length(Data) >= 10 -> wait_for_port_reply_cont2(Socket, Data);
                Data -> wait_for_port_reply_cont(Socket, Data)
            end;
        {tcp_closed, Socket} -> noport
    after 10000 ->
        gen_tcp:close(Socket),
        noport
    end.

wait_for_port_reply_cont2(Socket, [A, B, _Type, _Proto, HighA, HighB, LowA, LowB, NLenA, NLenB|Rest]) ->
    wait_for_port_reply_name(Socket, ?u16(NLenA, NLenB), Rest),
    {port, ?u16(A, B), best_version(?u16(LowA, LowB), ?u16(HighA, HighB))}.

wait_for_port_reply_name(Socket, Len, Sofar) ->
    receive
        {tcp, Socket, _Data} -> wait_for_port_reply_name(Socket, Len, Sofar);
        {tcp_closed, Socket} -> ok
    end.

best_version(Low, High) -> select_best_version(epmd_dist_low(), epmd_dist_high(), Low, High).

select_best_version(L1, H1, L2, H2) when L1 > H2; L2 > H1 -> 0;
select_best_version(_L1, H1, _L2, H2) -> min(H1, H2).

wait_for_close(Socket, Reply) ->
    receive
        {tcp_closed, Socket} -> Reply
    after 10000 ->
            gen_tcp:close(Socket),
            Reply
    end.

epmd_dist_high() ->
    case os:getenv("ERL_EPMD_DIST_HIGH") of
        false -> ?epmd_dist_high;
        Version ->
            try list_to_integer(Version) of
                N when N < ?epmd_dist_high -> N;
                _ -> ?epmd_dist_high
            catch
                _:_ -> ?epmd_dist_high
            end
    end.

epmd_dist_low() ->
    case os:getenv("ERL_EPMD_DIST_LOW") of
        false -> ?epmd_dist_low;
        Version ->
            try list_to_integer(Version) of
                N when N > ?epmd_dist_low -> N;
                _ -> ?epmd_dist_low
            catch
                _:_ -> ?epmd_dist_low
            end
    end.
-endif.
