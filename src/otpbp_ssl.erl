-module(otpbp_ssl).

-ifndef(HAVE_ssl__ssl_accept_1).
% OTP < 24.0
-export([ssl_accept/1]).
-endif.
-ifndef(HAVE_ssl__ssl_accept_2).
% OTP < 24.0
-export([ssl_accept/2]).
-endif.
-ifndef(HAVE_ssl__ssl_accept_3).
% OTP < 24.0
-export([ssl_accept/3]).
-endif.

-ifndef(HAVE_ssl__ssl_accept_1).
ssl_accept(Socket) when is_port(Socket) -> ssl:handshake(Socket);
ssl_accept(Socket) ->
     case ssl:handshake(Socket) of
        {ok, _} -> ok;
        Error -> Error
     end.
-endif.

-ifndef(HAVE_ssl__ssl_accept_2).
ssl_accept(Socket, SslOptionsOrTimeout) when is_port(Socket) -> ssl:handshake(Socket, SslOptionsOrTimeout);
ssl_accept(Socket, SslOptionsOrTimeout) ->
     case ssl:handshake(Socket, SslOptionsOrTimeout) of
        {ok, _} -> ok;
        Error -> Error
     end.
-endif.

-ifndef(HAVE_ssl__ssl_accept_3).
ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) -> ssl:handshake(Socket, SslOptions, Timeout);
ssl_accept(Socket, SslOptions, Timeout) ->
     case ssl:handshake(Socket, SslOptions, Timeout) of
        {ok, _} -> ok;
        Error -> Error
     end.
-endif.
