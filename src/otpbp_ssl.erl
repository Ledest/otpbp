-module(otpbp_ssl).

-ifndef(HAVE_ssl__handshake_1).
% OTP 21.0
-export([handshake/1]).
-endif.
-ifndef(HAVE_ssl__handshake_2).
% OTP 21.0
-export([handshake/2]).
-endif.
-ifndef(HAVE_ssl__handshake_3).
% OTP 21.0
-export([handshake/3]).
-endif.

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
-ifndef(HAVE_ssl__cipher_suites_0).
% OTP < 24.0
-export([cipher_suites/0]).
-endif.
-ifndef(HAVE_ssl__cipher_suites_1).
% OTP < 24.0
-export([cipher_suites/1]).
-endif.

-ifndef(HAVE_ssl__cipher_suites_0).
-ifdef(HAVE_ssl__cipher_suites_1).
-import(ssl, [cipher_suites/1]).
-endif.
-endif.

-ifndef(HAVE_ssl__handshake_1).
handshake(Socket) ->
    case ssl:ssl_accept(Socket) of
        ok -> {ok, Socket};
        R -> R
    end.
-endif.

-ifndef(HAVE_ssl__handshake_2).
handshake(Socket, SslOptionsOrTimeout) ->
    case ssl:ssl_accept(Socket, SslOptionsOrTimeout) of
        ok -> {ok, Socket};
        R -> R
    end.
-endif.

-ifndef(HAVE_ssl__handshake_3).
handshake(Socket, SslOptions, Timeout) ->
    case ssl:ssl_accept(Socket, SslOptions, Timeout) of
        ok -> {ok, Socket};
        R -> R
    end.
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

-ifndef(HAVE_ssl__cipher_suites_0).
cipher_suites() -> cipher_suites(erlang).
-endif.

-ifndef(HAVE_ssl__cipher_suites_1).
cipher_suites(erlang) -> lists:map(fun ssl_cipher_format:suite_legacy/1, available_suites_default());
cipher_suites(openssl) ->
    lists:map(fun(Suite) -> ssl_cipher_format:suite_map_to_openssl_str(ssl_cipher_format:suite_bin_to_map(Suite)) end,
              available_suites_default());
cipher_suites(all) ->
    lists:map(fun ssl_cipher_format:suite_legacy/1,
              ssl_cipher:filter_suites(ssl_cipher:all_suites(tls_record:highest_protocol_version([])))).

available_suites_default() -> ssl_cipher:filter_suites(ssl_cipher:suites(tls_record:highest_protocol_version([]))).
-endif.
