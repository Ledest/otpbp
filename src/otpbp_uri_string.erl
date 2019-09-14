-module(otpbp_uri_string).

-compile([{parse_transform, otpbp_pt}]).

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

-ifndef(HAVE_uri_string__compose_query_1).
-export([compose_query/1]).
-ifdef(HAVE_uri_string__compose_query_2).
-import(uri_string, [compose_query/2]).
-else.
-ifndef(NEED_uri_string__compose_query_2).
-define(NEED_uri_string__compose_query_2, true).
-endif.
-endif.
-endif.
-ifndef(HAVE_uri_string__compose_query_2).
-export([compose_query/2]).
-ifndef(NEED_uri_string__compose_query_2).
-define(NEED_uri_string__compose_query_2, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__dissect_query_1).
-export([dissect_query/1]).
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
-define(map(), #{}).
-define(map(K, V), #{K => V}).
-else.
-define(map(), maps:new()).
-define(map(K, V), maps:from_list([{K, V}])).
-endif.

parse(URIString) when is_binary(URIString) ->
    try
        parse_uri_reference(URIString, ?map())
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
parse(URIString) when is_list(URIString) ->
    Binary = unicode:characters_to_binary(URIString),
    try
        convert_mapfields_to_list(parse_uri_reference(Binary, ?map()))
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
parse_hier(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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
parse_segment(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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

'query'(Rest, URI) ->
    {T, URI1} = parse_query(Rest, URI),
    {Rest, maps:put('query', decode_query(calculate_parsed_query_fragment(Rest, T)), URI1)}.

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
    maps:put('query', decode_query(calculate_parsed_query_fragment(Rest, T)), maybe_add_path(URI1));
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
parse_host(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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
parse_segment_nz_nc(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
parse_segment_nz_nc(?STRING_REST($#, Rest), URI) -> fragment(Rest, URI);
parse_segment_nz_nc(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, URI);
        false -> throw({error, invalid_uri, [Char]})
    end;
parse_segment_nz_nc(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

parse_port(?STRING_REST($/, Rest), URI) -> segment(Rest, URI);
parse_port(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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
parse_ipv6_bin_end(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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
    'query'(Rest, URI);
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
parse_reg_name(?STRING_REST($?, Rest), URI) -> 'query'(Rest, URI);
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

-ifndef(HAVE_uri_string__compose_query_1).
compose_query(List) -> compose_query(List, [{encoding, utf8}]).
-endif.

-ifdef(NEED_uri_string__compose_query_2).
compose_query([], _Options) -> [];
compose_query(List, Options) ->
    try
        compose_query(List, Options, false, <<>>)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.

compose_query([{Key, true}|Rest], Options, IsList, Acc) ->
    compose_query(Rest, Options,
                  IsList orelse is_list(Key),
                  <<Acc/binary, (form_urlencode(Key, Options))/binary, (get_separator(Rest))/binary>>);
compose_query([{Key, Value}|Rest], Options, IsList, Acc) ->
    compose_query(Rest, Options,
                  IsList orelse is_list(Key) orelse is_list(Value),
                  <<Acc/binary, (form_urlencode(Key, Options))/binary, $=, (form_urlencode(Value, Options))/binary,
                    (get_separator(Rest))/binary>>);
compose_query([], _Options, true, Acc) -> convert_to_list(Acc, utf8);
compose_query([], _Options, false, Acc) -> Acc.

%% Returns separator to be used between key-value pairs
get_separator([]) -> <<>>;
get_separator(_L) -> <<"&">>.

convert_to_list(Binary, InEncoding) ->
    case unicode:characters_to_list(Binary, InEncoding) of
        {E, _List, RestData} when E =:= error; E =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.

convert_to_binary(Binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(Binary, InEncoding, OutEncoding) of
        {E, _List, RestData} when E =:= error; E =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.

%% For each character in the entry's name and value that cannot be expressed using
%% the selected character encoding, replace the character by a string consisting of
%% a U+0026 AMPERSAND character (&), a "#" (U+0023) character, one or more ASCII
%% digits representing the Unicode code point of the character in base ten, and
%% finally a ";" (U+003B) character.
base10_encode(Cs) -> base10_encode(Cs, <<>>).

base10_encode(<<>>, Acc) -> Acc;
base10_encode(<<H/utf8, T/binary>>, Acc) when H > 255 ->
    base10_encode(T, <<Acc/binary, "&#", (convert_to_binary(integer_to_list(H,10), utf8, utf8))/binary, $;>>);
base10_encode(<<H/utf8, T/binary>>, Acc) -> base10_encode(T, <<Acc/binary, H>>).

%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - encoding (non UTF-8)
form_urlencode(Cs, [{encoding, latin1}]) when is_list(Cs) ->
    html5_byte_encode(base10_encode(convert_to_binary(Cs, utf8, utf8)));
form_urlencode(Cs, [{encoding, latin1}]) when is_binary(Cs) -> html5_byte_encode(base10_encode(Cs));
form_urlencode(Cs, [{encoding, Encoding}]) when is_list(Cs), Encoding =:= utf8 orelse Encoding =:= unicode ->
    html5_byte_encode(convert_to_binary(Cs, utf8, Encoding));
form_urlencode(Cs, [{encoding, Encoding}]) when is_binary(Cs), Encoding =:= utf8 orelse Encoding =:= unicode ->
    html5_byte_encode(Cs);
form_urlencode(Cs, [{encoding, Encoding}]) when is_list(Cs); is_binary(Cs) -> throw({error, invalid_encoding, Encoding});
form_urlencode(Cs, _) -> throw({error, invalid_input, Cs}).

html5_byte_encode(B) -> html5_byte_encode(B, <<>>).

-define(DEC2HEX(X),
        if
            (X) >= 0, (X) =< 9 -> (X) + $0;
            (X) >= 10, (X) =< 15 -> (X) + ($A - 10)
        end).

html5_byte_encode(<<>>, Acc) -> Acc;
html5_byte_encode(<<$ , T/binary>>, Acc) -> html5_byte_encode(T, <<Acc/binary, $+>>);
html5_byte_encode(<<H, T/binary>>, Acc) ->
    case is_url_char(H) of
        true -> html5_byte_encode(T, <<Acc/binary, H>>);
        false ->
            <<A:4, B:4>> = <<H>>,
            html5_byte_encode(T, <<Acc/binary, $%, (?DEC2HEX(A)), (?DEC2HEX(B))>>)
    end;
html5_byte_encode(H, _Acc) -> throw({error, invalid_input, H}).

%% Return true if input char can appear in form-urlencoded string
%% Allowed chararacters: 0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A, 0x5F, 0x61 to 0x7A
is_url_char(C) ->
    C =:= 16#2A orelse C =:= 16#2D orelse C =:= 16#2E orelse C =:= 16#5F orelse
    16#30 =< C andalso C =< 16#39 orelse 16#41 =< C andalso C =< 16#5A orelse 16#61 =< C andalso C =< 16#7A.
-endif.

-ifndef(HAVE_uri_string__dissect_query_1).
dissect_query(QueryString) when QueryString =:= <<>>; QueryString =:= [] -> [];
dissect_query(QueryString) when is_list(QueryString) ->
    try
        dissect_query_key(convert_to_binary(QueryString, utf8, utf8), true, [], <<>>, <<>>)
    catch
        throw:{error, _Atom, _RestData} = R -> R
    end;
dissect_query(QueryString) ->
    try
        dissect_query_key(QueryString, false, [], <<>>, <<>>)
    catch
        throw:{error, _Atom, _RestData} = R -> R
    end.

dissect_query_key(<<$=, T/binary>>, IsList, Acc, Key, Value) -> dissect_query_value(T, IsList, Acc, Key, Value);
dissect_query_key(<<"&#", T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, Acc, <<Key/binary, "&#">>, Value);
dissect_query_key(<<$&, _/binary>> = T, IsList, Acc, Key, <<>>) -> dissect_query_value(T, IsList, Acc, Key, true);
dissect_query_key(<<H, T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, Acc, <<Key/binary, H>>, Value);
dissect_query_key(<<>>, IsList, Acc, Key, <<>>) -> dissect_query_value(<<>>, IsList, Acc, Key, true).

dissect_query_value(<<$&, T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, [{form_urldecode(IsList, Key), form_urldecode(IsList, Value)}|Acc], <<>>, <<>>);
dissect_query_value(<<H, T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_value(T, IsList, Acc, Key, <<Value/binary, H>>);
dissect_query_value(<<>>, IsList, Acc, Key, Value) ->
    lists:reverse([{form_urldecode(IsList, Key), form_urldecode(IsList, Value)}|Acc]).

-define(HEX2DEC(X),
        if
            (X) >= $0, (X) =< $9 -> (X) - $0;
            (X) >= $A, (X) =< $F -> (X) - ($A + 10);
            (X) >= $a, (X) =< $f -> (X) - ($a + 10)
        end).

%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - decoding (non UTF-8)
form_urldecode(_, true) -> true;
form_urldecode(true, B) -> convert_to_list(base10_decode(form_urldecode(B, <<>>)), utf8);
form_urldecode(false, B) -> base10_decode(form_urldecode(B, <<>>));
form_urldecode(<<>>, Acc) -> Acc;
form_urldecode(<<$+, T/binary>>, Acc) -> form_urldecode(T, <<Acc/binary, $\s>>);
form_urldecode(<<$%, C0, C1, T/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true -> form_urldecode(T, <<Acc/binary, (?HEX2DEC(C0) * 16 + ?HEX2DEC(C1))>>);
        false -> throw({error, invalid_percent_encoding, convert_to_list(<<$%, C0, C1, T/binary>>, utf8)})
    end;
form_urldecode(<<H/utf8, T/binary>>, Acc) -> form_urldecode(T, <<Acc/binary, H/utf8>>);
form_urldecode(<<H, _/binary>>, _Acc) -> throw({error, invalid_character, [H]}).

base10_decode(Cs) -> base10_decode(Cs, <<>>).

base10_decode(<<>>, Acc) -> Acc;
base10_decode(<<"&#", T/binary>>, Acc) -> base10_decode_unicode(T, Acc);
base10_decode(<<H/utf8, T/binary>>, Acc) -> base10_decode(T, <<Acc/binary, H/utf8>>);
base10_decode(<<H, _/binary>>, _) -> throw({error, invalid_input, [H]}).

base10_decode_unicode(B, Acc) -> base10_decode_unicode(B, 0, Acc).

base10_decode_unicode(<<H/utf8, T/binary>>, Codepoint, Acc) when H >= $0, H =< $9 ->
    base10_decode_unicode(T, Codepoint * 10 + (H - $0), Acc);
base10_decode_unicode(<<$;, T/binary>>, Codepoint, Acc) -> base10_decode(T, <<Acc/binary, Codepoint/utf8>>);
base10_decode_unicode(<<H, _/binary>>, _, _) -> throw({error, invalid_input, [H]}).
-endif.
