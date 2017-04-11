-module(otpbp_ssl).

-ifdef(HAVE_ssl__connection_information_1).
-import(ssl, [connection_information/1]).
-endif.

-ifndef(HAVE_ssl__connection_information_1).
-export([connection_information/1]).
-endif.

-ifndef(HAVE_ssl__connection_information_2).
-export([connection_information/2]).
-endif.

-ifndef(HAVE_ssl__connection_information_1).
connection_information(SslSocket) ->
    case ssl:connection_info(SslSocket) of
        {ok, {ProtocolVersion, CipherSuite}} -> {ok, [{protocol, ProtocolVersion}, {cipher_suite, CipherSuite}]};
        {error, _} = E -> E
    end.
-endif.

-ifndef(HAVE_ssl__connection_information_2).
connection_information(SslSocket, Items) ->
    case connection_information(SslSocket) of
        {ok, L} -> {ok, [Item || {K, _} = Item <- L, lists:member(K, Items)]};
        {error, _} = E -> E
    end.
-endif.
