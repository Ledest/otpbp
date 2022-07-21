-module(otpbp_http_uri).

-ifndef(HAVE_http_uri__encode_1).
% OTP < 25.0
-export([encode/1]).
-endif.
-ifndef(HAVE_http_uri__decode_1).
% OTP < 25.0
-export([decode/1]).
-endif.
-ifndef(HAVE_http_uri__scheme_defaults_0).
% OTP < 25.0
-export([scheme_defaults/0]).
-endif.
-ifndef(HAVE_http_uri__parse_2).
% OTP < 25.0
-export([parse/2]).
-endif.
-ifndef(HAVE_http_uri__parse_1).
% OTP < 25.0
-export([parse/1]).
-ifdef(HAVE_http_uri__parse_2).
-import(http_uri, [parse/2]).
-endif.
-endif.

-ifndef(HAVE_http_uri__parse_1).
-ifdef(HAVE_http_uri__parse_2).
-import(http_uri, [parse/2]).
-endif.
-endif.

-ifndef(HAVE_http_uri__parse_2).
-ifdef(HAVE_http_uri__scheme_defaults_0).
-import(http_uri, [scheme_defaults/0]).
-endif.
-endif.

-ifndef(HAVE_http_uri__encode_1).
encode(URI) when is_list(URI) ->
    R = reserved(),
    lists:foldr(fun(C, A) ->
                    case sets:is_element(C, R) of
                        true -> [$%|http_util:integer_to_hexlist(C) ++ A];
                        false -> [C|A]
                    end
                end, [], URI);
encode(URI) when is_binary(URI) -> list_to_binary(encode(binary_to_list(URI))).

-compile({inline, reserved/0}).
reserved() ->
    sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?, $#, $[, $], $<, $>, ${, $}, $|, $", $\\, $', $^, $%, $\s]).
-endif.

-ifndef(HAVE_http_uri__decode_1).
decode(URI) when is_list(URI); is_binary(URI) ->
    case uri_string:percent_decode(URI) of
        {error, _, _} -> error(function_clause);
        R -> R
    end.
-endif.

-ifndef(HAVE_http_uri__parse_2).
parse(URI, Options) ->
    case uri_string:parse(URI) of
        #{scheme := S, host := H, port := P} = M -> parse(M, Options, scheme(S), H, P, S);
        #{scheme := S, host := H} = M ->
            Scheme = scheme(S),
            case lists:keyfind(Scheme, 1, scheme_defaults(Options)) of
                {_Scheme, P} -> parse(M, Options, Scheme, H, P, S);
                false -> {error, {no_default_port, Scheme, URI}}
            end;
        #{scheme := S} -> {error, {malformed_url, scheme(S), URI}};
        #{} -> {error, no_scheme};
        {error, invalid_uri, _} -> {error, {malformed_url, scheme(protocol(URI)), URI}};
        {error, _, _} -> {error, no_scheme}
    end.

parse(M, Options, Scheme, H, P, S) ->
    case lists:keyfind(scheme_validation_fun, 1, Options) of
        {_scheme_validation_fun, Fun} when is_function(Fun) ->
            case Fun(S) of
                valid -> parse(M, Options, Scheme, H, P);
                {error, _} = E -> E
            end;
        _ -> parse(M, Options, Scheme, H, P)
    end.

parse(M, Options, Scheme, H, Port) ->
    E = empty(H),
    Host = host(H, Options),
    #{path := P, query := Q, userinfo := U} = maps:merge(#{path => E, query => E, userinfo => E}, M),
    Path = path(P),
    Query = prefix(Q, $?),
    {ok,
     case lists:member({fragment, true}, Options) of
         false -> {Scheme, U, Host, Port, Path, Query};
         true ->
             {Scheme, U, Host, Port, Path, Query,
              case M of
                  #{fragment := F} -> fragment(F);
                  _ -> E
              end}
     end}.

prefix([_|_] = S, C) -> [C|S];
prefix(S, C) when byte_size(S) =/= 0 -> <<C, S/binary>>;
prefix(S, _) -> S.

scheme(S) when is_list(S) -> list_to_atom(S);
scheme(S) when is_binary(S) -> binary_to_atom(S, latin1).

path("") -> "/";
path(<<>>) -> <<$/>>;
path(S) -> S.

fragment(F) when is_list(F) -> [$#|F];
fragment(F) when is_binary(F) -> <<$#, F/binary>>.

host(H, Options) ->
    case lists:member({ipv6_host_with_brackets, true}, Options) of
        false -> H;
        true -> ipv6(H)
    end.

ipv6(H) when is_list(H) ->
    case string:chr(H, $:) of
        0 -> H;
        _ -> [$[|H ++ "]"]
    end;
ipv6(H) when is_binary(H) ->
    case binary:match(H, <<$:>>) of
        nomatch -> H;
        _ -> <<$[, H/binary, $]>>
    end.

protocol(URI) when is_list(URI) -> string:sub_word(URI, 1, $:);
protocol(URI) when is_binary(URI) ->
    {I, _} = binary:match(URI, <<$:>>),
    binary:part(URI, 0, I).

scheme_defaults(Options) ->
    case lists:keyfind(scheme_defaults, 1, Options) of
        false -> scheme_defaults();
        {_scheme_defaults, SD} -> SD
    end.

empty(URI) when is_list(URI) -> "";
empty(URI) when is_binary(URI) -> <<>>.

-compile({inline, [empty/1, fragment/1, host/2, ipv6/1, protocol/1, scheme_defaults/1]}).
-endif.

-ifndef(HAVE_http_uri__parse_2).
parse(URI) -> parse(URI, []).
-endif.

-ifndef(HAVE_http_uri__scheme_defaults_0).
scheme_defaults() -> [{http,  80}, {https, 443}, {ftp,   21}, {ssh,   22}, {sftp,  22}, {tftp,  69}].
-endif.
