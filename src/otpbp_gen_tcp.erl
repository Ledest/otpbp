-module(otpbp_gen_tcp).

-compile({parse_transform, otpbp_pt}).

-ifndef(HAVE_gen_tcp__connect_2).
% OTP 24.3
-export([connect/2]).
-endif.

-ifndef(HAVE_gen_tcp__connect_2).
connect(#{family := Fam} = SockAddr, Opts) when Fam =:= inet; Fam =:= inet6 ->
    SockAddr2 = inet:ensure_sockaddr(SockAddr),
    case inet:gen_tcp_module(Opts) of
        {gen_tcp, Opts2} ->
            try connect_(SockAddr2, Opts2) of
                {ok, _} = R -> R;
                {error, einval} -> exit(badarg);
                Error -> Error
            catch
                _:Reason -> exit(Reason)
            end;
        {GenTcpMod, Opts2} -> GenTcpMod:connect(SockAddr2, Opts2, infinity)
    end.

-compile({inline, connect_/2}).
connect_(SockAddr, Opts0) ->
    {Mod, Opts} = inet:tcp_module(Opts0, SockAddr),
    Mod:connect(SockAddr, Opts, infinity).
-endif.
