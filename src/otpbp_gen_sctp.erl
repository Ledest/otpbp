-module(otpbp_gen_sctp).

-compile({parse_transform, otpbp_pt}).

-ifndef(HAVE_gen_sctp__connect_3).
% OTP 24.3
-export([connect/3]).
-endif.
-ifndef(HAVE_gen_sctp__connect_init_3).
% OTP 24.3
-export([connect_init/3]).
-endif.

-ifndef(HAVE_gen_sctp__connect_3).
connect(S, SockAddr, Opts) when is_port(S), is_map(SockAddr), is_list(Opts) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:connect(S, inet:ensure_sockaddr(SockAddr), Opts, false);
        Error -> Error
    end;
connect(S, SockAddr, Opts) -> error(badarg, [S, SockAddr, Opts, infinity]).
-endif.

-ifndef(HAVE_gen_sctp__connect_init_3).
connect_init(S, SockAddr, Opts) when is_port(S), is_map(SockAddr), is_list(Opts) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} -> Mod:connect(S, inet:ensure_sockaddr(SockAddr), Opts, nowait);
        Error -> Error
    end;
connect_init(S, SockAddr, Opts) -> error(badarg, [S, SockAddr, Opts, infinity]).
-endif.

