-module(otpbp_uri_string).

-ifndef(HAVE_uri_string__parse_1).
-ifdef(HAVE_uri_string__is_host_1).
-import(uri_string, [is_host/1]).
-endif.
-ifdef(HAVE_uri_string__is_path_1).
-import(uri_string, [is_path/1]).
-endif.
-export([parse/1]).
-endif.

-ifndef(HAVE_uri_string__is_host_1).
-export([is_host/1]).
-endif.
-ifndef(HAVE_uri_string__is_path_1).
-export([is_path/1]).
-endif.

-define(SUB_DELIM, "!$&'()*+,;=").

-ifndef(HAVE_uri_string__parse_1).
-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
-define(CHAR(Char), <<Char/utf8>>).
-define(STRING_EMPTY, <<>>).
-define(STRING_REST(MatchStr, Rest), <<MatchStr/utf8, (Rest)/binary>>).
-ifdef(HAVE_erlang__is_map_1).
-define(map(K, V), #{K => V}).
-else.
-define(map(K, V), maps:from_list([{K, V}])).
-endif.

parse(URIString) when is_binary(URIString) ->
    try
        parse_uri_reference(URIString, #{})
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
parse(URIString) when is_list(URIString) ->
    Binary = unicode:characters_to_binary(URIString),
    try
        convert_mapfields_to_list(parse_uri_reference(Binary, #{}))
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.

parse_uri_reference(<<>>, _) -> ?map(path, <<>>);
parse_uri_reference(URIString, URI) ->
    try
        parse_scheme_start(URIString, URI)
    catch
        throw:{_, _, _} -> parse_relative_part(URIString, URI)
    end.

parse_scheme_start(?STRING_REST(Char, Rest), URI) ->
    case is_alpha(Char) of
        true ->
            {T, URI1} = parse_scheme(Rest, URI),
            maps:put(scheme, ?STRING_REST(Char, calculate_parsed_scheme(Rest, T)), maybe_add_path(URI1));
        false -> throw({error, invalid_uri, [Char]})
    end.

parse_scheme(?STRING_REST($:, Rest), URI) ->
    {_, URI1} = parse_hier(Rest, URI),
    {Rest, URI1};
parse_scheme(?STRING_REST(Char, Rest), URI) ->
    case is_scheme(Char) of
        true -> parse_scheme(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_scheme(?STRING_EMPTY, _URI) -> throw({error, invalid_uri, <<>>}).

parse_hier(?STRING_REST("//", Rest), URI) ->
    % Parse userinfo - "//" is NOT part of authority
    {Rest, try parse_userinfo(Rest, URI) of
               {T, URI1} -> maps:put(userinfo, decode_userinfo(calculate_parsed_userinfo(Rest, T)), URI1)
           catch
               throw:{_, _, _} ->
                   {T, URI1} = parse_host(Rest, URI),
                   maps:put(host, decode_host(remove_brackets(calculate_parsed_host_port(Rest, T))), URI1)
           end};
parse_hier(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_hier(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_hier(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_hier(?STRING_REST(Char, Rest), URI) ->  % path-rootless
    case is_pchar(Char) of
        true ->  % segment_nz
            {T, URI1} = parse_segment(Rest, URI),
            {Rest, maps:put(path, decode_path(?STRING_REST(Char, calculate_parsed_part(Rest, T))), URI1)};
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_hier(?STRING_EMPTY, URI) -> {<<>>, URI}.

parse_segment(?STRING_REST($/, Rest), URI) -> parse_segment(Rest, URI);  % segment
parse_segment(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_segment(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_segment(?STRING_REST(Char, Rest), URI) ->
    case is_pchar(Char) of
        true -> parse_segment(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_segment(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

segment(Rest, URI) -> {Rest, segment0(Rest, URI)}.

segment0(Rest, URI) ->
    {T, URI1} = parse_segment(Rest, URI),
    maps:put(path, decode_path(?STRING_REST($/, calculate_parsed_part(Rest, T))), URI1).

parse_query(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_query(?STRING_REST(Char, Rest), URI) ->
    case is_query(Char) of
        true -> parse_query(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_query(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

query(Rest, URI) ->
    {T, URI1} = parse_query(Rest, URI),
    {Rest, maps:put(query, decode_query(calculate_parsed_query_fragment(Rest, T)), URI1)}.

parse_fragment(?STRING_REST(Char, Rest), URI) ->
    case is_fragment(Char) of
        true -> parse_fragment(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_fragment(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

decode_fragment(Cs) -> check_utf8(decode(Cs, fun is_fragment/1, <<>>)).

check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {T, _, _} when T =:= incomplete; T =:= error -> throw({error, invalid_utf8, Cs});
        _ -> Cs
    end.

decode(<<$%, C0, C1, Cs/binary>>, Fun, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true -> decode(Cs, Fun, <<Acc/binary, C0:4, C1:4>>);
        false -> throw({error, invalid_percent_encoding, <<$%, C0, C1>>})
    end;
decode(<<C, Cs/binary>>, Fun, Acc) ->
    case Fun(C) of
        true -> decode(Cs, Fun, <<Acc/binary, C>>);
        false -> throw({error, invalid_percent_encoding, <<C, Cs/binary>>})
    end;
decode(<<>>, _Fun, Acc) -> Acc.

calculate_parsed_query_fragment(Input, <<>>) -> strip_last_char(Input, "#");
calculate_parsed_query_fragment(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

strip_last_char(<<>>, _) -> <<>>;
strip_last_char(Input, L) ->
    case lists:member(binary:last(Input), L) of
        true -> init_binary(Input);
        _false -> Input
    end.

init_binary(B) -> binary:part(B, 0, byte_size(B) - 1).

convert_mapfields_to_list(Map) ->
    maps:map(fun(_, V) when is_binary(V) -> unicode:characters_to_list(V);
                (_, V) -> V
             end, Map).

parse_relative_part(?STRING_REST("//", Rest), URI) ->
    %% Parse userinfo - "//" is NOT part of authority
    try parse_userinfo(Rest, URI) of
        {T, URI1} -> maps:put(userinfo, decode_userinfo(calculate_parsed_userinfo(Rest, T)), maybe_add_path(URI1))
    catch
        throw:{_, _, _} ->
            {T, URI1} = parse_host(Rest, URI),
            maps:put(host, decode_host(remove_brackets(calculate_parsed_host_port(Rest, T))), maybe_add_path(URI1))
    end;
parse_relative_part(?STRING_REST($/, Rest), URI) -> segment0(Rest, URI);
parse_relative_part(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    maps:put(query, decode_query(calculate_parsed_query_fragment(Rest, T)), maybe_add_path(URI1));
parse_relative_part(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    maps:put(fragment, decode_fragment(calculate_parsed_query_fragment(Rest, T)), maybe_add_path(URI1));
parse_relative_part(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true ->
            {T, URI1} = parse_segment_nz_nc(Rest, URI),  % path-noscheme
            maps:put(path, decode_path(?STRING_REST(Char, calculate_parsed_part(Rest, T))), URI1);
        false -> throw({error, invalid_uri, [Char]})
    end.

calculate_parsed_scheme(Input, <<>>) -> strip_last_char(Input, ":");
calculate_parsed_scheme(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

maybe_add_path(Map) ->
    case maps:is_key(path, Map) of
        true -> Map;
        _false -> maps:put(path, <<>>, Map)
    end.

parse_userinfo(?CHAR($@), URI) -> {?STRING_EMPTY, maps:put(host, <<>>, URI)};
parse_userinfo(?STRING_REST($@, Rest), URI) ->
    {T, URI1} = parse_host(Rest, URI),
    {Rest, maps:put(host, decode_host(remove_brackets(calculate_parsed_host_port(Rest, T))), URI1)};
parse_userinfo(?STRING_REST(Char, Rest), URI) ->
    case is_userinfo(Char) of
        true -> parse_userinfo(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_userinfo(?STRING_EMPTY, _URI) -> throw({error, invalid_uri, <<>>}). % URI cannot end in userinfo state

calculate_parsed_userinfo(Input, <<>>) -> strip_last_char(Input, "?#@");
calculate_parsed_userinfo(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

decode_userinfo(Cs) -> check_utf8(decode(Cs, fun is_userinfo/1, <<>>)).

parse_host(?STRING_REST($:, Rest), URI) -> port(Rest, URI);
parse_host(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_host(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_host(?STRING_REST($[, Rest), URI) -> parse_ipv6_bin(Rest, [], URI);
parse_host(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_host(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_ipv4_bin(Rest, [Char], URI);
        false -> parse_reg_name(?STRING_REST(Char, Rest), URI)
    end;
parse_host(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

calculate_parsed_host_port(Input, <<>>) -> strip_last_char(Input, ":?#/");
calculate_parsed_host_port(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

decode_host(Cs) -> check_utf8(decode(Cs, fun is_host/1, <<>>)).

remove_brackets(<<$[, Rest/binary>>) ->
    case binary:last(Rest) of
        $] -> binary:part(Rest, 0, byte_size(Rest) - 1);
        _ -> Rest
    end;
remove_brackets(Addr) -> Addr.

calculate_parsed_part(Input, <<>>) -> strip_last_char(Input, "?#");
calculate_parsed_part(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

decode_path(Cs) -> check_utf8(decode(Cs, fun is_path/1, <<>>)).

decode_query(Cs) -> check_utf8(decode(Cs, fun is_query/1, <<>>)).

get_parsed_binary(Input, Unparsed) -> binary:part(Input, 0, byte_size(Input) - byte_size_exl_head(Unparsed)).

parse_segment_nz_nc(?STRING_REST($/, Rest), URI) -> parse_segment(Rest, URI);  % segment
parse_segment_nz_nc(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_segment_nz_nc(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_segment_nz_nc(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_segment_nz_nc(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

parse_port(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_port(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_port(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_port(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_port(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_port(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

port(Rest, URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {Rest, maps:put(port, get_port(calculate_parsed_host_port(Rest, T)), URI1)}.

get_port(<<>>) -> undefined;
get_port(B) ->
    try
        binary_to_integer(B)
    catch
        error:badarg -> throw({error, invalid_uri, B})
    end.

parse_ipv6_bin(?STRING_REST($], Rest), Acc, URI) ->
    _ = validate_ipv6_address(lists:reverse(Acc)),
    parse_ipv6_bin_end(Rest, URI);
parse_ipv6_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin(Rest, [Char|Acc], URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_ipv6_bin(?STRING_EMPTY, _Acc, _URI) -> throw({error, invalid_uri, <<>>}).

validate_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error, invalid_uri, Addr})
    end.

parse_ipv6_bin_end(?STRING_REST($:, Rest), URI) -> port(Rest, URI);
parse_ipv6_bin_end(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_ipv6_bin_end(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_ipv6_bin_end(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_ipv6_bin_end(?STRING_REST(Char, Rest), URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin_end(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_ipv6_bin_end(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

parse_ipv4_bin(?STRING_REST($:, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    port(Rest, URI);
parse_ipv4_bin(?STRING_REST($/, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    segment(Rest, URI);
parse_ipv4_bin(?STRING_REST($?, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    query(Rest, URI);
parse_ipv4_bin(?STRING_REST($#, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    fragment(Rest, URI);
parse_ipv4_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv4(Char) of
        true -> parse_ipv4_bin(Rest, [Char|Acc], URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_ipv4_bin(?STRING_EMPTY, Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {?STRING_EMPTY, URI}.

validate_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error, invalid_uri, Addr})
    end.

parse_reg_name(?STRING_REST($:, Rest), URI) -> port(Rest, URI);
parse_reg_name(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_reg_name(?STRING_REST($?, Rest), URI) -> query(Rest, URI);
parse_reg_name(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_reg_name(?STRING_REST(Char, Rest), URI) ->
    case is_reg_name(Char) of
        true -> parse_reg_name(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_reg_name(?STRING_EMPTY, URI) -> {?STRING_EMPTY, URI}.

fragment(Rest, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Rest, maps:put(fragment, decode_fragment(calculate_parsed_query_fragment(Rest, T)), URI1)}.

byte_size_exl_head(<<>>) -> 0;
byte_size_exl_head(Binary) -> byte_size(Binary) + 1.

is_hex_digit(C) -> C >= $a andalso C =< $f orelse C >= $A andalso C =< $F orelse is_digit(C).

is_fragment(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

is_query(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

is_userinfo(C) -> lists:member(C, "%:" ?SUB_DELIM) orelse is_unreserved(C).

is_scheme(C) -> lists:member(C, "+-.") orelse is_alpha(C) orelse is_digit(C).

is_segment_nz_nc(C) -> lists:member(C, "%@" ?SUB_DELIM) orelse is_unreserved(C).

is_ipv6(C) -> C =:= $: orelse C =:= $. orelse is_hex_digit(C).

is_ipv4(C) -> C =:= $. orelse is_digit(C).

is_reg_name(C) -> lists:member(C, "%" ?SUB_DELIM) orelse is_unreserved(C).
-endif.

-ifndef(HAVE_uri_string__is_host_1).
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
is_host(C) -> lists:member(C, ":" ?SUB_DELIM) orelse is_unreserved(C).
-endif.

-ifndef(HAVE_uri_string__is_path_1).
-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
is_path(C) -> C =:= $/ orelse is_pchar(C).
-endif.

-ifdef(NEED_is_pchar_1).
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
is_pchar(C) -> lists:member(C, "%:@" ?SUB_DELIM) orelse is_unreserved(C).
-endif.

-ifdef(NEED_is_unreserved_1).
-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
is_unreserved(C) -> lists:member(C, "-._~") orelse is_alpha(C) orelse is_digit(C).
-endif.

-ifdef(NEED_is_alpha_1).
is_alpha(C) -> C >= $A andalso C =< $Z orelse C >= $a andalso C =< $z.
-endif.

-ifdef(NEED_is_digit_1).
is_digit(C) -> C >= $0 andalso C =< $9.
-endif.
