-module(otpbp_uri_string).

-ifndef(OTP_RELEASE).
-compile({parse_transform, otpbp_pt}).
-endif.

-ifndef(HAVE_uri_string__is_host_1).
% OTP 21.0
-export([is_host/1]).
-endif.
-ifndef(HAVE_uri_string__is_path_1).
% OTP 21.0
-export([is_path/1]).
-endif.
-ifndef(HAVE_uri_string__compose_query_1).
% OTP 21.0
-export([compose_query/1]).
-endif.
-ifndef(HAVE_uri_string__compose_query_2).
% OTP 21.0
-export([compose_query/2]).
-endif.
-ifndef(HAVE_uri_string__dissect_query_1).
% OTP 21.0
-export([dissect_query/1]).
-endif.
-ifndef(HAVE_uri_string__normalize_1).
% OTP 21.0
-export([normalize/1]).
-endif.
-ifndef(HAVE_uri_string__normalize_2).
% OTP 21.0
-export([normalize/2]).
-endif.
-ifndef(HAVE_uri_string__parse_1).
% OTP 21.0
-export([parse/1]).
-endif.
-ifndef(HAVE_uri_string__recompose_1).
% OTP 21.0
-export([recompose/1]).
-endif.
-ifndef(HAVE_uri_string__transcode_2).
% OTP 21.0
-export([transcode/2]).
-endif.
-ifndef(HAVE_uri_string__resolve_2).
% OTP 22.3
-export([resolve/2]).
-endif.
-ifndef(HAVE_uri_string__resolve_3).
% OTP 22.3
-export([resolve/3]).
-endif.
-ifndef(HAVE_uri_string__allowed_characters_0).
% OTP 23.2
-export([allowed_characters/0]).
-endif.
-ifndef(HAVE_uri_string__percent_decode_1).
% OTP 23.2
-export([percent_decode/1]).
-endif.
-ifndef(HAVE_uri_string__quote_1).
% OTP 25.0
-export([quote/1]).
-endif.
-ifndef(HAVE_uri_string__quote_2).
% OTP 25.0
-export([quote/2]).
-endif.
-ifndef(HAVE_uri_string__unquote_1).
% OTP 25.0
-export([unquote/1]).
-endif.

-ifndef(HAVE_uri_string__compose_query_1).
-ifdef(HAVE_uri_string__compose_query_2).
-import(uri_string, [compose_query/2]).
-endif.
-endif.
-ifndef(HAVE_uri_string__normalize_1).
-ifdef(HAVE_uri_string__normalize_2).
-import(uri_string, [normalize/2]).
-endif.
-endif.
-ifndef(HAVE_uri_string__normalize_2).
-ifndef(NEED_parse_1).
-define(NEED_parse_1, true).
-endif.
-ifndef(NEED_recompose_1).
-define(NEED_recompose_1, true).
-endif.
-endif.
-ifndef(HAVE_uri_string__resolve_2).
-ifdef(HAVE_uri_string__resolve_3).
-import(uri_string, [resolve/3]).
-endif.
-endif.
-ifndef(HAVE_uri_string__resolve_3).
-ifndef(NEED_parse_1).
-define(NEED_parse_1, true).
-endif.
-ifndef(NEED_recompose_1).
-define(NEED_recompose_1, true).
-endif.
-endif.
-ifndef(HAVE_uri_string__percent_decode_1).
-ifdef(HAVE_uri_string__unquote_1).
-import(uri_string, [unquote/1]).
-endif.
-endif.
-ifdef(NEED_recompose_1).
-ifdef(HAVE_uri_string__recompose_1).
-import(uri_string, [recompose/1]).
-endif.
-endif.
-ifdef(NEED_parse_1).
-ifdef(HAVE_uri_string__parse_1).
-import(uri_string, [parse/1]).
-endif.
-endif.

-ifdef(HAVE_uri_string__is_host_1).
-define(fun_is_host_1, fun uri_string:is_host/1).
-else.
-define(fun_is_host_1, fun is_host/1).
-endif.
-ifdef(HAVE_uri_string__is_path_1).
-define(fun_is_path_1, fun uri_string:is_path/1).
-else.
-define(fun_is_path_1, fun is_path/1).
-endif.

-define(DEC2HEX(X),
        if
            X =< 9 -> X + $0;
            true -> X + ($A - 16#A)
        end).

-define(HEX2DEC(X),
        if
            X =< $9 -> X - $0;
            X =< $F -> X - ($A - 16#A);
            true -> X - ($a - 16#A)
        end).

-type uri_string() :: iodata().
-type error() :: {error, atom(), term()}.
-type uri_map() :: #{fragment => unicode:chardata(),
                     host => unicode:chardata(),
                     path => unicode:chardata(),
                     port => non_neg_integer() | undefined,
                     query => unicode:chardata(),
                     scheme => unicode:chardata(),
                     userinfo => unicode:chardata()}.
-export_type([error/0, uri_map/0, uri_string/0]).

-ifndef(HAVE_uri_string__is_host_1).
is_host(C) -> C =:= $: orelse is_unreserved(C) orelse is_sub_delim(C).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_is_sub_delim_1).
-define(NEED_is_sub_delim_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__is_path_1).
is_path(C) -> C =:= $/ orelse is_pchar(C).

-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__compose_query_1).
compose_query(List) -> compose_query(List, [{encoding, utf8}]).
-endif.

-ifndef(HAVE_uri_string__compose_query_2).
compose_query([], _Options) -> [];
compose_query(List, Options) ->
    try
        compose_query(List, Options, false, <<>>)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end.

compose_query([{Key, true}|Rest], Options, IsList, Acc) ->
    compose_query(Rest, Options, IsList orelse is_list(Key),
                  <<Acc/binary, (form_urlencode(Key, Options))/binary, (get_separator(Rest))/binary>>);
compose_query([{Key, Value}|Rest], Options, IsList, Acc) ->
    compose_query(Rest, Options, IsList orelse is_list(Key) orelse is_list(Value),
                  <<Acc/binary, (form_urlencode(Key, Options))/binary, $=, (form_urlencode(Value, Options))/binary,
                    (get_separator(Rest))/binary>>);
compose_query([], _Options, true, Acc) -> convert_to_list(Acc, utf8);
compose_query([], _Options, false, Acc) -> Acc.

form_urlencode(Cs, [{encoding, latin1}]) when is_list(Cs) ->
    html5_byte_encode(base10_encode(convert_to_binary(Cs, utf8, utf8)));
form_urlencode(Cs, [{encoding, latin1}]) when is_binary(Cs) -> html5_byte_encode(base10_encode(Cs));
form_urlencode(Cs, [{encoding, Enc}]) when is_list(Cs), Enc =:= utf8 orelse Enc =:= unicode ->
    html5_byte_encode(convert_to_binary(Cs, utf8, Enc));
form_urlencode(Cs, [{encoding, Enc}]) when is_binary(Cs), Enc =:= utf8 orelse Enc =:= unicode -> html5_byte_encode(Cs);
form_urlencode(Cs, [{encoding, Enc}]) when is_list(Cs); is_binary(Cs) -> throw({error, invalid_encoding, Enc});
form_urlencode(Cs, _) -> throw({error, invalid_input, Cs}).

-ifndef(NEED_convert_to_list_2).
-define(NEED_convert_to_list_2, true).
-endif.
-ifndef(NEED_convert_to_binary_3).
-define(NEED_convert_to_binary_3, true).
-endif.

base10_encode(Cs) -> base10_encode(Cs, <<>>).

base10_encode(<<>>, Acc) -> Acc;
base10_encode(<<H/utf8, T/binary>>, Acc) when H > 255 ->
    base10_encode(T, <<Acc/binary, "&#", (integer_to_binary(H))/binary, $;>>);
base10_encode(<<H/utf8, T/binary>>, Acc) -> base10_encode(T, <<Acc/binary, H>>).

html5_byte_encode(B) -> html5_byte_encode(B, <<>>).

html5_byte_encode(<<>>, Acc) -> Acc;
html5_byte_encode(<<$\s, T/binary>>, Acc) -> html5_byte_encode(T, <<Acc/binary, $+>>);
html5_byte_encode(<<H, T/binary>>, Acc) ->
    html5_byte_encode(T,
                      case is_url_char(H) of
                          true -> <<Acc/binary, H>>;
                          false -> <<Acc/binary, $%, (?DEC2HEX(H bsr 4)), (?DEC2HEX(H band 2#1111))>>
                      end);
html5_byte_encode(H, _Acc) -> throw({error, invalid_input, H}).

get_separator([]) -> <<>>;
get_separator(_L) -> <<"&">>.

-compile({inline, is_url_char/1}).
is_url_char(C) ->
    C =:= 16#2A orelse C =:= 16#2D orelse C =:= 16#2E orelse C =:= 16#5F orelse
    C >= 16#30 andalso C =< 16#39 orelse C >= 16#41 andalso C =< 16#5A orelse C >= 16#61 andalso C =< 16#7A.
-endif.

-ifndef(HAVE_uri_string__dissect_query_1).
dissect_query(QueryString) when QueryString =:= <<>>; QueryString =:= [] -> [];
dissect_query(QueryString) when is_list(QueryString) ->
    try
        dissect_query_key(convert_to_binary(QueryString, utf8, utf8), true, [], <<>>, <<>>)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end;
dissect_query(QueryString) ->
    try
        dissect_query_key(QueryString, false, [], <<>>, <<>>)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end.

-ifndef(NEED_convert_to_binary_3).
-define(NEED_convert_to_binary_3, true).
-endif.

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

form_urldecode(_, true) -> true;
form_urldecode(true, B) -> convert_to_list(base10_decode(form_urldecode(B, <<>>)), utf8);
form_urldecode(false, B) -> base10_decode(form_urldecode(B, <<>>));
form_urldecode(<<>>, Acc) -> Acc;
form_urldecode(<<$+, T/binary>>, Acc) -> form_urldecode(T, <<Acc/binary, $\s>>);
form_urldecode(<<$%, C0, C1, T/binary>> = B, Acc) ->
    is_hex_digit(C0) andalso is_hex_digit(C1) orelse throw({error, invalid_percent_encoding, convert_to_list(B, utf8)}),
    form_urldecode(T, <<Acc/binary, (?HEX2DEC(C0) * 16 + ?HEX2DEC(C1))>>);
form_urldecode(<<H/utf8, T/binary>>, Acc) -> form_urldecode(T, <<Acc/binary, H/utf8>>);
form_urldecode(<<H, _/binary>>, _Acc) -> throw({error, invalid_character, [H]}).

base10_decode(Cs) -> base10_decode(Cs, <<>>).

base10_decode(<<"&#",T/binary>>, Acc) -> base10_decode_unicode(T, Acc);
base10_decode(<<H/utf8, T/binary>>, Acc) -> base10_decode(T, <<Acc/binary, H/utf8>>);
base10_decode(<<H, _/binary>>, _) -> throw({error, invalid_input, [H]});
base10_decode(<<>>, Acc) -> Acc.

base10_decode_unicode(B, Acc) -> base10_decode_unicode(B, 0, Acc).

base10_decode_unicode(<<H/utf8, T/binary>>, Codepoint, Acc) when H >= $0, H =< $9 ->
    base10_decode_unicode(T, Codepoint * 10 + (H - $0), Acc);
base10_decode_unicode(<<$;, T/binary>>, Codepoint, Acc) -> base10_decode(T, <<Acc/binary, Codepoint/utf8>>);
base10_decode_unicode(<<H, _/binary>>, _, _) -> throw({error, invalid_input, [H]}).

-ifndef(NEED_convert_to_list_2).
-define(NEED_convert_to_list_2, true).
-endif.
-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__normalize_1).
normalize(URIMap) -> normalize(URIMap, []).
-endif.

-ifndef(HAVE_uri_string__normalize_2).
normalize(URIMap, []) when is_map(URIMap) ->
    try
        recompose(normalize_map(URIMap))
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end;
normalize(URIMap, [return_map]) when is_map(URIMap) ->
    try
        normalize_map(URIMap)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end;
normalize(URIString, []) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            try
                recompose(normalize_map(Value))
            catch
                throw:{error, _Atom, _RestData} = E -> E
            end;
        Error -> Error
    end;
normalize(URIString, [return_map]) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            try
                normalize_map(Value)
            catch
                throw:{error, _Atom, _RestData} = E -> E
            end;
        Error -> Error
    end.

normalize_map(URIMap) ->
    lists:foldr(fun(F, A) -> F(A) end, URIMap,
                [fun normalize_path_segment/1, fun normalize_scheme_based/1, fun normalize_undefined_port/1,
                 fun normalize_percent_encoding/1, fun normalize_case/1]).

-compile({inline, normalize_case/1}).
normalize_case(#{scheme := Scheme, host := Host} = Map) -> Map#{scheme => to_lower(Scheme), host => to_lower(Host)};
normalize_case(#{host := Host} = Map) -> Map#{host => to_lower(Host)};
normalize_case(#{scheme := Scheme} = Map) -> Map#{scheme => to_lower(Scheme)};
normalize_case(#{} = Map) -> Map.

-compile({inline, normalize_percent_encoding/1}).
normalize_percent_encoding(Map) ->
    maps:map(fun(K, V) when K =:= userinfo; K =:= host; K =:= path; K =:= query; K =:= fragment -> decode(V);
                (_, V) -> V % Handle port and scheme
             end, Map).

to_lower(Cs) when is_binary(Cs) -> to_lower_(Cs);
to_lower(Cs) when is_list(Cs) -> convert_to_list(to_lower_(convert_to_binary(Cs, utf8, utf8)), utf8).

to_lower_(Cs) ->
    <<<<(if
             C >= $A, C =< $Z -> C + ($a -$A);
             true -> C
         end)>> || <<C>> <= Cs>>.

-compile({inline, normalize_scheme_based/1}).
normalize_scheme_based(Map) ->
    [Scheme, Port, Path] = lists:map(fun(K) -> maps:get(K, Map, undefined) end, [scheme, port, path]),
    normalize_scheme_based(Map, Scheme, Port, Path).

-compile({inline, normalize_scheme_based/4}).
normalize_scheme_based(Map, Scheme, Port, Path) when Scheme =:= "http"; Scheme =:= <<"http">> ->
    normalize_http(Map, Port, Path);
normalize_scheme_based(Map, Scheme, Port, Path) when Scheme =:= "https"; Scheme =:= <<"https">> ->
    normalize_https(Map, Port, Path);
normalize_scheme_based(Map, Scheme, Port, _Path) when Scheme =:= "ftp"; Scheme =:= <<"ftp">> ->
    normalize_ftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path) when Scheme =:= "ssh"; Scheme =:= <<"ssh">> ->
    normalize_ssh_sftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path) when Scheme =:= "sftp"; Scheme =:= <<"sftp">> ->
    normalize_ssh_sftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path) when Scheme =:= "tftp"; Scheme =:= <<"tftp">> ->
    normalize_tftp(Map, Port);
normalize_scheme_based(Map, _, _, _) -> Map.

-compile({inline, normalize_http/3}).
normalize_http(Map, Port, Path) -> normalize_http_path(normalize_default_port(Map, Port, 80), Path).

-compile({inline, normalize_https/3}).
normalize_https(Map, Port, Path) -> normalize_http_path(normalize_default_port(Map, Port, 443), Path).

-compile({inline, normalize_ftp/2}).
normalize_ftp(Map, Port) -> normalize_default_port(Map, Port, 21).

normalize_ssh_sftp(Map, Port) -> normalize_default_port(Map, Port, 22).

-compile({inline, normalize_tftp/2}).
normalize_tftp(Map, Port) -> normalize_default_port(Map, Port, 69).

normalize_default_port(Map, Port, Port) -> maps:remove(port, Map);
normalize_default_port(Map, _Port, _Default) -> Map.

-compile({inline, normalize_undefined_port/1}).
normalize_undefined_port(#{port := undefined} = Map) -> maps:remove(port, Map);
normalize_undefined_port(#{} = Map) -> Map.

normalize_http_path(Map, "") -> Map#{path => "/"};
normalize_http_path(Map, <<>>) -> Map#{path => <<$/>>};
normalize_http_path(Map, _Path) -> Map.

-compile({inline, decode/1}).
decode(Cs) -> decode(Cs, <<>>).

decode(L, Acc) when is_list(L) -> unicode:characters_to_list(decode(unicode:characters_to_binary(L), Acc));
decode(<<$%, C0, C1, Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0) * 16 + ?HEX2DEC(C1),
            decode(Cs,
                   case is_unreserved(B) of
                       false ->
                           %% [2.2] Characters in the reserved set are protected from normalization.
                           %% [2.1] For consistency, URI producers and normalizers should
                           %% use uppercase hexadecimal digits for all percent-encodings.
                           <<Acc/binary, $%, (hex_to_upper(C0)), (hex_to_upper(C1))>>;
                       true -> <<Acc/binary, B>>
                   end);
        false -> throw({error, invalid_percent_encoding, <<$%, C0, C1>>})
    end;
decode(<<C, Cs/binary>>, Acc) -> decode(Cs, <<Acc/binary, C>>);
decode(<<>>, Acc) -> check_utf8(Acc).

-ifndef(NEED_convert_to_binary_3).
-define(NEED_convert_to_binary_3, true).
-endif.
-ifndef(NEED_convert_to_list_2).
-define(NEED_convert_to_list_2, true).
-endif.
-ifndef(NEED_normalize_path_segment_1).
-define(NEED_normalize_path_segment_1, true).
-endif.
-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
-endif.
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_check_utf8_1).
-define(NEED_check_utf8_1, true).
-endif.

hex_to_upper(H) when H >= $a, H =< $f -> H - ($a - $A);
hex_to_upper(H) when H >= $0, H =< $9; H >= $A, H =< $F-> H;
hex_to_upper(H) -> throw({error, invalid_input, H}).
-endif.

-ifndef(HAVE_uri_string__parse_1).
parse(URIString) when is_binary(URIString) ->
    try
        parse_uri_reference(URIString, #{})
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end;
parse(URIString) when is_list(URIString) ->
    try
        convert_mapfields_to_list(parse_uri_reference(unicode:characters_to_binary(URIString), #{}))
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end.

-compile({inline, convert_mapfields_to_list/1}).
convert_mapfields_to_list(Map) ->
    maps:map(fun(_, V) when is_binary(V) -> unicode:characters_to_list(V);
                (_, V) -> V
             end,
             Map).

parse_uri_reference(<<>>, _) -> #{path => <<>>};
parse_uri_reference(URIString, URI) ->
    try
        parse_scheme_start(URIString, URI)
    catch
        throw:{_, _, _} -> parse_relative_part(URIString, URI)
    end.

-compile({inline, parse_relative_part/2}).
parse_relative_part(<<"//", Rest/binary>>, URI) ->
    %% Parse userinfo - "//" is NOT part of authority
    try parse_userinfo(Rest, URI) of
        {T, URI1} ->
            maps:put(userinfo, calculate_parsed_userinfo(Rest, T), maybe_add_path(URI1))
    catch
        throw:{_, _, _} ->
            {T, URI1} = parse_host(Rest, URI),
            maps:put(host, remove_brackets(calculate_parsed_host_port(Rest, T)), maybe_add_path(URI1))
    end;
parse_relative_part(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>};
parse_relative_part(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    maps:put(query, calculate_parsed_query_fragment(Rest, T), maybe_add_path(URI1));
parse_relative_part(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    maps:put(fragment, calculate_parsed_query_fragment(Rest, T), maybe_add_path(URI1));
parse_relative_part(<<Char/utf8, Rest/binary>>, URI) ->
    is_segment_nz_nc(Char) orelse throw({error, invalid_uri, [Char]}),
    {T, URI1} = parse_segment_nz_nc(Rest, URI),  % path-noscheme
    URI1#{path => <<Char/utf8, (calculate_parsed_part(Rest, T))/binary>>}.

parse_segment(<<$/, Rest/binary>>, URI) -> parse_segment(Rest, URI);  % segment
parse_segment(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_segment(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_segment(<<Char/utf8, Rest/binary>>, URI) ->
    is_pchar(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_segment(Rest, URI);
parse_segment(<<>>, URI) -> {<<>>, URI}.

parse_segment_nz_nc(<<$/, Rest/binary>>, URI) -> parse_segment(Rest, URI);  % segment
parse_segment_nz_nc(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_segment_nz_nc(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_segment_nz_nc(<<Char/utf8, Rest/binary>>, URI) ->
    is_segment_nz_nc(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_segment_nz_nc(Rest, URI);
parse_segment_nz_nc(<<>>, URI) -> {<<>>, URI}.

is_segment_nz_nc(C) -> C =:= $% orelse C =:= $@ orelse is_unreserved(C) orelse is_sub_delim(C).

-compile({inline, parse_scheme_start/2}).
parse_scheme_start(<<Char/utf8, Rest/binary>>, URI) ->
    is_alpha(Char) orelse throw({error, invalid_uri, [Char]}),
    {T, URI1} = parse_scheme(Rest, URI),
    maps:put(scheme, <<Char/utf8, (calculate_parsed_scheme(Rest, T))/binary>>, maybe_add_path(URI1)).

maybe_add_path(Map) -> maps:merge(#{path => <<>>}, Map).

parse_scheme(<<$:, Rest/binary>>, URI) ->
    {_, URI1} = parse_hier(Rest, URI),
    {Rest, URI1};
parse_scheme(<<Char/utf8, Rest/binary>>, URI) ->
    is_scheme(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_scheme(Rest, URI);
parse_scheme(<<>>, _URI) -> throw({error,invalid_uri,<<>>}).

-compile({inline, parse_hier/2}).
parse_hier(<<"//", Rest/binary>>, URI) ->
    % Parse userinfo - "//" is NOT part of authority
    try parse_userinfo(Rest, URI) of
        {T, URI1} -> {Rest, URI1#{userinfo => calculate_parsed_userinfo(Rest, T)}}
    catch
        throw:{_, _, _} ->
            {T, URI1} = parse_host(Rest, URI),
            {Rest, URI1#{host => remove_brackets(calculate_parsed_host_port(Rest, T))}}
    end;
parse_hier(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_hier(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_hier(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_hier(<<Char/utf8, Rest/binary>>, URI) ->  % path-rootless
    is_pchar(Char) orelse throw({error, invalid_uri, [Char]}),
    % segment_nz
    {T, URI1} = parse_segment(Rest, URI),
    {Rest, URI1#{path => <<Char/utf8, (calculate_parsed_part(Rest, T))/binary>>}};
parse_hier(<<>>, URI) -> {<<>>, URI}.

parse_userinfo(<<$@>>, URI) -> {<<>>, URI#{host => <<>>}};
parse_userinfo(<<$@, Rest/binary>>, URI) ->
    {T, URI1} = parse_host(Rest, URI),
    {Rest, URI1#{host => remove_brackets(calculate_parsed_host_port(Rest, T))}};
parse_userinfo(<<Char/utf8, Rest/binary>>, URI) ->
    is_userinfo(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_userinfo(Rest, URI);
parse_userinfo(<<>>, _URI) ->
    %% URI cannot end in userinfo state
    throw({error,invalid_uri,<<>>}).

parse_host(<<$:, Rest/binary>>, URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {Rest, URI1#{port => get_port(calculate_parsed_host_port(Rest, T))}};
parse_host(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_host(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_host(<<$[, Rest/binary>>, URI) -> parse_ipv6_bin(Rest, [], URI);
parse_host(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_host(<<Char/utf8, Rest/binary>>, URI) ->
    case is_digit(Char) of
        true ->
            try
                parse_ipv4_bin(Rest, [Char], URI)
            catch
                throw:{_, _, _} -> parse_reg_name(<<Char/utf8, Rest/binary>>, URI)
            end;
        false -> parse_reg_name(<<Char/utf8, Rest/binary>>, URI)
    end;
parse_host(<<>>, URI) -> {<<>>, URI}.

parse_reg_name(<<$:, Rest/binary>>, URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {Rest, URI1#{port => get_port(calculate_parsed_host_port(Rest, T))}};
parse_reg_name(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_reg_name(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_reg_name(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_reg_name(<<Char/utf8, Rest/binary>>, URI) ->
    is_reg_name(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_reg_name(Rest, URI);
parse_reg_name(<<>>, URI) -> {<<>>, URI}.

parse_ipv4_bin(<<$:, Rest/binary>>, Acc, URI) ->
    validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_port(Rest, URI),
    {Rest, URI1#{port => get_port(calculate_parsed_host_port(Rest, T))}};
parse_ipv4_bin(<<$/, Rest/binary>>, Acc, URI) ->
    validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_ipv4_bin(<<$?, Rest/binary>>, Acc, URI) ->
    validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_ipv4_bin(<<$#, Rest/binary>>, Acc, URI) ->
    validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_ipv4_bin(<<Char/utf8, Rest/binary>>, Acc, URI) ->
    is_ipv4(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_ipv4_bin(Rest, [Char|Acc], URI);
parse_ipv4_bin(<<>>, Acc, URI) ->
    validate_ipv4_address(lists:reverse(Acc)),
    {<<>>, URI}.

validate_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error, invalid_uri, Addr})
    end.

parse_ipv6_bin(<<$], Rest/binary>>, Acc, URI) ->
    validate_ipv6_address(lists:reverse(Acc)),
    parse_ipv6_bin_end(Rest, URI);
parse_ipv6_bin(<<Char/utf8, Rest/binary>>, Acc, URI) ->
    is_ipv6(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_ipv6_bin(Rest, [Char|Acc], URI);
parse_ipv6_bin(<<>>, _Acc, _URI) -> throw({error, invalid_uri, <<>>}).

parse_ipv6_bin_end(<<$:, Rest/binary>>, URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {Rest, URI1#{port => get_port(calculate_parsed_host_port(Rest, T))}};
parse_ipv6_bin_end(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_ipv6_bin_end(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_ipv6_bin_end(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_ipv6_bin_end(<<Char/utf8, Rest/binary>>, URI) ->
    is_ipv6(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_ipv6_bin_end(Rest, URI);
parse_ipv6_bin_end(<<>>, URI) -> {<<>>, URI}.

-compile({inline, validate_ipv6_address/1}).
validate_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error, invalid_uri, Addr})
    end.

parse_port(<<$/, Rest/binary>>, URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Rest, URI1#{path => <<$/, (calculate_parsed_part(Rest, T))/binary>>}};
parse_port(<<$?, Rest/binary>>, URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Rest, URI1#{query => calculate_parsed_query_fragment(Rest, T)}};
parse_port(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_port(<<Char/utf8, Rest/binary>>, URI) ->
    is_digit(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_port(Rest, URI);
parse_port(<<>>, URI) -> {<<>>, URI}.

parse_query(<<$#, Rest/binary>>, URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Rest, URI1#{fragment => calculate_parsed_query_fragment(Rest, T)}};
parse_query(<<Char/utf8, Rest/binary>>, URI) ->
    is_query(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_query(Rest, URI);
parse_query(<<>>, URI) -> {<<>>, URI}.

parse_fragment(<<Char/utf8, Rest/binary>>, URI) ->
    is_fragment(Char) orelse throw({error, invalid_uri, [Char]}),
    parse_fragment(Rest, URI);
parse_fragment(<<>>, URI) -> {<<>>, URI}.

-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_is_sub_delim_1).
-define(NEED_is_sub_delim_1, true).
-endif.
-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_scheme_1).
-define(NEED_is_scheme_1, true).
-endif.
-ifndef(NEED_is_userinfo_1).
-define(NEED_is_userinfo_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-ifndef(NEED_is_reg_name_1).
-define(NEED_is_reg_name_1, true).
-endif.
-ifndef(NEED_is_ipv4_1).
-define(NEED_is_ipv4_1, true).
-endif.
-ifndef(NEED_is_ipv6_1).
-define(NEED_is_ipv6_1, true).
-endif.
-ifndef(NEED_is_query_1).
-define(NEED_is_query_1, true).
-endif.
-ifndef(NEED_is_fragment_1).
-define(NEED_is_fragment_1, true).
-endif.

remove_brackets(<<$[, Rest/binary>>) ->
    S = byte_size(Rest) - 1,
    case Rest of
        <<R:S/binary, $]>> -> R;
        _ -> Rest
    end;
remove_brackets(Addr) -> Addr.

-compile({inline, calculate_parsed_scheme/2}).
calculate_parsed_scheme(Input, <<>>) -> strip_last_char(Input, ":");
calculate_parsed_scheme(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

calculate_parsed_part(Input, <<>>) -> strip_last_char(Input, "?#");
calculate_parsed_part(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

calculate_parsed_userinfo(Input, <<>>) -> strip_last_char(Input, "?#@");
calculate_parsed_userinfo(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

calculate_parsed_host_port(Input, <<>>) -> strip_last_char(Input, ":?#/");
calculate_parsed_host_port(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

calculate_parsed_query_fragment(Input, <<>>) -> strip_last_char(Input, "#");
calculate_parsed_query_fragment(Input, Unparsed) -> get_parsed_binary(Input, Unparsed).

get_port(<<>>) -> undefined;
get_port(B) ->
    try
        binary_to_integer(B)
    catch
        error:badarg -> throw({error, invalid_uri, B})
    end.

strip_last_char(<<>>, _) -> <<>>;
strip_last_char(Input, L) ->
    S = byte_size(Input) - 1,
    <<H:S/binary, C>> = Input,
    case lists:member(C, L) of
        true -> H;
        _false -> Input
    end.

get_parsed_binary(Input, Unparsed) -> binary:part(Input, 0, byte_size(Input) - byte_size(Unparsed) - 1).
-endif.

-ifndef(HAVE_uri_string__recompose_1).
recompose(Map) ->
    case is_valid_map(Map) of
        false -> {error, invalid_map, Map};
        true ->
            try
                lists:foldl(fun(F, A) -> F(Map, A) end, empty,
                            [fun update_scheme/2, fun update_userinfo/2, fun update_host/2,
                             fun update_port/2, fun update_path/2, fun update_query/2, fun update_fragment/2])
            catch
                throw:{error, _Atom, _RestData} = E -> E
            end
    end.

-compile({inline, is_valid_map/1}).
is_valid_map(#{path := Path} = Map) ->
    (starts_with_two_slash(Path) orelse maps:is_key(userinfo, Map) orelse maps:is_key(port, Map)) andalso
        is_valid_map_host(Map) orelse
            all_fields_valid(Map);
is_valid_map(#{}) -> false.

-compile({inline, starts_with_two_slash/1}).
starts_with_two_slash("//" ++ _) -> true;
starts_with_two_slash(<<"//", _/binary>>) -> true;
starts_with_two_slash(_) -> false.

-compile({inline, is_valid_map_host/1}).
is_valid_map_host(Map) -> maps:is_key(host, Map) andalso all_fields_valid(Map).

all_fields_valid(Map) -> maps:keys(Map) -- [scheme, userinfo, host, port, path, query, fragment] =:= [].

update_scheme(#{scheme := Scheme}, _) -> add_colon_postfix(encode_scheme(Scheme));
update_scheme(#{}, _) -> empty.

update_userinfo(#{userinfo := Userinfo}, empty) -> add_auth_prefix(encode_userinfo(Userinfo));
update_userinfo(#{userinfo := Userinfo}, URI) -> concat(URI, add_auth_prefix(encode_userinfo(Userinfo)));
update_userinfo(#{}, empty) -> empty;
update_userinfo(#{}, URI) -> URI.

update_host(#{host := Host}, empty) -> add_auth_prefix(encode_host(Host));
update_host(#{host := Host} = Map, URI) -> concat(URI, add_host_prefix(Map, encode_host(Host)));
update_host(#{}, URI) -> URI.

update_port(#{port := undefined}, URI) -> concat(URI, <<$:>>);
update_port(#{port := Port}, URI) -> concat(URI, add_colon(encode_port(Port)));
update_port(#{}, URI) -> URI.

update_path(#{path := Path}, empty) -> encode_path(Path);
update_path(#{host := _, path := Path}, URI) -> concat(URI, encode_path(make_path_absolute(maybe_flatten_list(Path))));
update_path(#{path := Path}, URI) -> concat(URI, encode_path(Path));
update_path(#{}, URI) -> URI.

update_query(#{query := Query}, empty) -> encode_query(Query);
update_query(#{query := Query}, URI) -> concat(URI, add_question_mark(encode_query(Query)));
update_query(#{}, URI) -> URI.

update_fragment(#{fragment := Fragment}, empty) -> add_hashmark(encode_fragment(Fragment));
update_fragment(#{fragment := Fragment}, URI) -> concat(URI, add_hashmark(encode_fragment(Fragment)));
update_fragment(#{}, empty) -> "";
update_fragment(#{}, URI) -> URI.

-compile({inline, add_colon_postfix/1}).
add_colon_postfix(Comp) when is_binary(Comp) -> <<Comp/binary, $:>>;
add_colon_postfix(Comp) when is_list(Comp) -> Comp ++ ":".

add_auth_prefix(Comp) -> add_double_slash(Comp).

-compile({inline, add_host_prefix/2}).
add_host_prefix(#{userinfo := _}, Host) -> add_char(Host, $@);
add_host_prefix(#{}, Host) -> add_double_slash(Host).

-compile({inline, add_colon/1}).
add_colon(Comp) when is_binary(Comp) -> <<$:, Comp/binary>>.

-compile({inline, add_question_mark/1}).
add_question_mark(Comp) -> add_char(Comp, $?).

add_hashmark(Comp) -> add_char(Comp, $#).

add_char(Comp, C) when is_binary(Comp) -> <<C, Comp/binary>>;
add_char(Comp, C) when is_list(Comp) -> [C|Comp].

add_double_slash(Comp) when is_binary(Comp) -> <<"//", Comp/binary>>;
add_double_slash(Comp) when is_list(Comp) -> [$/, $/|Comp].

-compile({inline, encode_scheme/1}).
encode_scheme(Scheme) ->
    Scheme =/= "" andalso Scheme =/= <<>> andalso validate_scheme(Scheme) orelse throw({error, invalid_scheme, Scheme}),
    Scheme.

encode_userinfo(Cs) -> encode(Cs, fun is_userinfo/1).

encode_host(Cs) ->
    case classify_host(Cs) of
        ipv6 -> bracket_ipv6(Cs);
        other -> encode(Cs, fun is_reg_name/1);
        C when C =:= regname; C =:= ipv4 -> Cs
    end.

-compile({inline, encode_port/1}).
encode_port(Port) -> integer_to_binary(Port).

encode_path(Cs) -> encode(Cs, ?fun_is_path_1).

encode_query(Cs) -> encode(Cs, fun is_query/1).

encode_fragment(Cs) -> encode(Cs, fun is_fragment/1).

validate_scheme([H|T]) -> is_scheme(H) andalso validate_scheme(T);
validate_scheme(<<H, Rest/binary>>) -> is_scheme(H) andalso validate_scheme(Rest);
validate_scheme(S) when S =:= []; S =:= <<>> -> true.

-compile({inline, classify_host/1}).
classify_host([]) -> other;
classify_host(Addr) when is_binary(Addr) -> classify_host_ipv6(unicode:characters_to_list(Addr));
classify_host(Addr) -> classify_host_ipv6(Addr).

classify_host_ipv6(Addr) ->
    case is_ipv6_address(Addr) of
        true -> ipv6;
        false -> classify_host_ipv4(Addr)
    end.

-compile({inline, classify_host_ipv4/1}).
classify_host_ipv4(Addr) ->
    case is_ipv4_address(Addr) of
        true -> ipv4;
        false -> classify_host_regname(Addr)
    end.

classify_host_regname([]) -> regname;
classify_host_regname([H|T]) ->
    case is_reg_name(H) of
        true -> classify_host_regname(T);
        false -> other
    end.

-compile({inline, is_ipv4_address/1}).
is_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> true;
        {error, _} -> false
    end.

-compile({inline, is_ipv6_address/1}).
is_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> true;
        {error, _} -> false
    end.

-compile({inline, bracket_ipv6/1}).
bracket_ipv6(Addr) when is_binary(Addr) -> <<$[, Addr/binary, $]>>;
bracket_ipv6(Addr) when is_list(Addr) -> [$[|Addr ++ "]"].

-compile({inline, make_path_absolute/1}).
make_path_absolute(Path) when Path =:= <<>>; Path =:= "" -> Path;
make_path_absolute(<<"/", _/binary>> = Path) -> Path;
make_path_absolute([$/|_] = Path) -> Path;
make_path_absolute(Path) -> add_char(Path, $/).

-compile({inline, maybe_flatten_list/1}).
maybe_flatten_list(Path) when is_binary(Path) -> Path;
maybe_flatten_list(Path) -> unicode:characters_to_list(Path).

concat(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
concat(A, B) when is_binary(A), is_list(B) -> unicode:characters_to_list(A) ++ B;
concat(A, B) when is_list(A), is_binary(B) -> A ++ unicode:characters_to_list(B);
concat(A, B) when is_list(A) -> A ++ B.

-ifndef(NEED_is_reg_name_1).
-define(NEED_is_reg_name_1, true).
-endif.
-ifndef(NEED_encode_2).
-define(NEED_encode_2, true).
-endif.
-ifndef(NEED_is_scheme_1).
-define(NEED_is_scheme_1, true).
-endif.
-ifndef(NEED_is_userinfo_1).
-define(NEED_is_userinfo_1, true).
-endif.
-ifndef(NEED_is_query_1).
-define(NEED_is_query_1, true).
-endif.
-ifndef(NEED_is_fragment_1).
-define(NEED_is_fragment_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__transcode_2).
transcode(URIString, Options) when is_binary(URIString) ->
    InEnc = proplists:get_value(in_encoding, Options, utf8),
    OutEnc = proplists:get_value(out_encoding, Options, utf8),
    try
        convert_to_binary(transcode(convert_to_list(URIString, InEnc), InEnc, OutEnc), utf8, OutEnc)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end;
transcode(URIString, Options) when is_list(URIString) ->
    InEnc = proplists:get_value(in_encoding, Options, utf8),
    OutEnc = proplists:get_value(out_encoding, Options, utf8),
    Flattened = flatten_list(URIString, InEnc),
    try
        transcode(Flattened, InEnc, OutEnc)
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end.

transcode([$%, _C0, _C1|_Rest] = L, InEnc, OutEnc) -> transcode_pct(L, InEnc, OutEnc, [], <<>>);
transcode([_C|_Rest] = L, InEnc, OutEnc) -> transcode(L, InEnc, OutEnc, [], []).

transcode([$%, _C0, _C1|_Rest] = L, InEnc, OutEnc, Acc, List) -> transcode_pct(L, InEnc, OutEnc, List ++ Acc, <<>>);
transcode([C|Rest], InEnc, OutEnc, Acc, List) -> transcode(Rest, InEnc, OutEnc, Acc, [C|List]);
transcode([], _InEnc, _OutEnc, Acc, List) -> lists:reverse(List ++ Acc).

transcode_pct([$%, C0, C1|Rest] = L, InEnc, OutEnc, Acc, B) ->
    is_hex_digit(C0) andalso is_hex_digit(C1) orelse throw({error, invalid_percent_encoding, L}),
    transcode_pct(Rest, InEnc, OutEnc, Acc, <<B/binary, (?HEX2DEC(C0) * 16 + ?HEX2DEC(C1))>>);
transcode_pct([_C|_Rest] = L, InEnc, OutEnc, Acc, B) ->
    transcode(L, InEnc, OutEnc,
              lists:reverse(convert_to_list(percent_encode_segment(convert_to_binary(B, InEnc, OutEnc)), utf8), Acc),
              []);
transcode_pct([], InEnc, OutEnc, Acc, B) ->
    lists:reverse(Acc, convert_to_list(percent_encode_segment(convert_to_binary(B, InEnc, OutEnc)), utf8)).

percent_encode_segment(Segment) -> percent_encode_binary(Segment, <<>>).

-compile({inline, flatten_list/2}).
flatten_list([], _) -> [];
flatten_list(L, InEnc) -> flatten_list(L, InEnc, []).

flatten_list([H|T], InEnc, Acc) when is_binary(H) ->
    flatten_list(T, InEnc, lists:reverse(convert_to_list(H, InEnc), Acc));
flatten_list([H|T], InEnc, Acc) when is_list(H) -> flatten_list(H ++ T, InEnc, Acc);
flatten_list([H|T], InEnc, Acc) -> flatten_list(T, InEnc, [H|Acc]);
flatten_list([], _InEnc, Acc) -> lists:reverse(Acc);
flatten_list(Arg, _, _) -> throw({error, invalid_input, Arg}).

-ifndef(NEED_percent_encode_binary_2).
-define(NEED_percent_encode_binary_2, true).
-endif.
-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
-endif.
-ifndef(NEED_convert_to_binary_3).
-define(NEED_convert_to_binary_3, true).
-endif.
-ifndef(NEED_convert_to_list_2).
-define(NEED_convert_to_list_2, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__allowed_characters_0).
allowed_characters() ->
    Input = lists:seq(0, 127),
    lists:keymap(fun(F) -> lists:filter(F, Input) end, 1,
                 [{scheme, fun is_scheme/1},
                  {userinfo, fun is_userinfo/1},
                  {host, ?fun_is_host_1},
                  {ipv4, fun is_ipv4/1},
                  {ipv6, fun is_ipv6/1},
                  {regname, fun is_reg_name/1},
                  {path, ?fun_is_path_1},
                  {query, fun is_query/1},
                  {fragment, fun is_fragment/1},
                  {reserved, fun is_reserved/1},
                  {unreserved, fun is_unreserved/1}]).

-ifndef(NEED_is_scheme_1).
-define(NEED_is_scheme_1, true).
-endif.
-ifndef(NEED_is_userinfo_1).
-define(NEED_is_userinfo_1, true).
-endif.
-ifndef(NEED_is_ipv4_1).
-define(NEED_is_ipv4_1, true).
-endif.
-ifndef(NEED_is_ipv6_1).
-define(NEED_is_ipv6_1, true).
-endif.
-ifndef(NEED_is_reg_name_1).
-define(NEED_is_reg_name_1, true).
-endif.
-ifndef(NEED_is_query_1).
-define(NEED_is_query_1, true).
-endif.
-ifndef(NEED_is_fragment_1).
-define(NEED_is_fragment_1, true).
-endif.
-ifndef(NEED_is_reserved_1).
-define(NEED_is_reserved_1, true).
-endif.
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__percent_decode_1).
percent_decode(URIMap) when is_map(URIMap)->
    Fun = fun(K, V) when K =:= userinfo; K =:= host; K =:= path; K =:= query; K =:= fragment ->
              case unquote(V) of
                  {error, Reason, Input} -> throw({error, {invalid, {K, {Reason, Input}}}});
                  Else -> Else
              end;
              %% Handle port and scheme
             (_, V) -> V
          end,
    try
        maps:map(Fun, URIMap)
    catch
        throw:Return -> Return
    end;
percent_decode(URI) when is_list(URI); is_binary(URI) -> unquote(URI).
-endif.

-ifndef(HAVE_uri_string__quote_1).
quote(D) -> encode(D, fun is_unreserved/1).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_encode_2).
-define(NEED_encode_2, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__quote_2).
quote(D, Safe) -> encode(D, fun(C) -> is_unreserved(C) orelse lists:member(C, Safe) end).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_encode_2).
-define(NEED_encode_2, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__unquote_1).
unquote(D) -> raw_decode(D, <<>>).

raw_decode(<<$%, C0, C1, Cs/binary>>, Acc) ->
    is_hex_digit(C0) andalso is_hex_digit(C1) orelse throw({error, invalid_percent_encoding, <<$%, C0, C1>>}),
    raw_decode(Cs, <<Acc/binary, (?HEX2DEC(C0) * 16 + ?HEX2DEC(C1))>>);
raw_decode(<<C, Cs/binary>>, Acc) -> raw_decode(Cs, <<Acc/binary, C>>);
raw_decode(<<>>, Acc) -> check_utf8(Acc);
raw_decode(L, Acc) when is_list(L) ->
    try
        unicode:characters_to_list(raw_decode(unicode:characters_to_binary(L), Acc))
    catch
        throw:{error, _Atom, _RestData} = E -> E
    end.

-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
-endif.
-ifndef(NEED_check_utf8_1).
-define(NEED_check_utf8_1, true).
-endif.
-endif.

-ifndef(HAVE_uri_string__resolve_2).
resolve(URIMap, BaseURIMap) -> resolve(URIMap, BaseURIMap, []).
-endif.

-ifndef(HAVE_uri_string__resolve_3).
resolve(URIMap, BaseURIMap, Options) when is_map(URIMap) ->
    case resolve_map(URIMap, BaseURIMap) of
        TargetURIMap when is_map(TargetURIMap) ->
            case Options of
                [return_map] -> TargetURIMap;
                [] -> recompose(TargetURIMap)
            end;
        Error -> Error
    end;
resolve(URIString, BaseURIMap, Options) ->
    case parse(URIString) of
        URIMap when is_map(URIMap) -> resolve(URIMap, BaseURIMap, Options);
        Error -> Error
    end.

-compile({inline, resolve_map/2}).
resolve_map(#{scheme := _} = URIMap, _) -> normalize_path_segment(URIMap);
resolve_map(URIMap, #{scheme := _} = BaseURIMap) -> resolve_map(URIMap, BaseURIMap, resolve_path_type(URIMap));
resolve_map(_URIMap, BaseURIMap) when is_map(BaseURIMap) -> {error, invalid_scheme, ""};
resolve_map(URIMap, BaseURIString) ->
    case parse(BaseURIString) of
        #{scheme := _} = BaseURIMap-> resolve_map(URIMap, BaseURIMap, resolve_path_type(URIMap));
        BaseURIMap when is_map(BaseURIMap) -> {error, invalid_scheme, ""};
        Error -> Error
    end.

resolve_map(#{host := _} = URI, #{scheme := Scheme}, _) -> normalize_path_segment(URI#{scheme => Scheme});
resolve_map(#{query := _} = URI, BaseURI, empty_path) ->
    maps:merge(URI, maps:with([scheme, userinfo, host, port, path],  BaseURI));
resolve_map(URI, BaseURI, empty_path) ->
    maps:merge(URI, maps:with([scheme, userinfo, host, port, path, query], BaseURI));
resolve_map(URI, BaseURI, absolute_path) ->
    normalize_path_segment(maps:merge(URI, maps:with([scheme, userinfo, host, port], BaseURI)));
resolve_map(#{path := Path} = URI, BaseURI, relative_path) ->
    normalize_path_segment(maps:merge(URI#{path => merge_paths(Path, BaseURI)},
                                      maps:with([scheme, userinfo, host, port], BaseURI))).

resolve_path_type(URIMap) ->
    case iolist_to_binary(maps:get(path, URIMap, <<>>)) of
        <<>> -> empty_path;
        <<$/, _/bits>> -> absolute_path;
        _ -> relative_path
    end.

-compile({inline, merge_paths/2}).
merge_paths(Path, #{path := BasePath0} = BaseURI) ->
    case {BaseURI, iolist_size(BasePath0)} of
        {#{host := _}, 0} -> merge_paths_absolute(Path);
        _ ->
            case string:split(BasePath0, <<$/>>, trailing) of
                [BasePath, _] when is_binary(Path) -> unicode:characters_to_binary([BasePath, $/, Path]);
                [BasePath, _] when is_list(Path) -> unicode:characters_to_list([BasePath, $/, Path]);
                [_] -> Path
            end
    end.

-compile({inline, merge_paths_absolute/1}).
merge_paths_absolute(Path) when is_binary(Path) -> <<$/, Path/binary>>;
merge_paths_absolute(Path) when is_list(Path) -> unicode:characters_to_list([$/, Path]).

-ifndef(NEED_normalize_path_segment_1).
-define(NEED_normalize_path_segment_1, true).
-endif.
-endif.

-ifdef(NEED_check_utf8_1).
check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {E, _, _} when E =:= incomplete; E =:= error -> throw({error, invalid_utf8, Cs});
        _ -> Cs
    end.
-endif.

-ifdef(NEED_is_userinfo_1).
is_userinfo(C) -> C =:= $% orelse C =:= $: orelse is_unreserved(C) orelse is_sub_delim(C).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_is_sub_delim_1).
-define(NEED_is_sub_delim_1, true).
-endif.
-endif.

-ifdef(NEED_is_reg_name_1).
is_reg_name(C) -> C =:= $% orelse is_unreserved(C) orelse is_sub_delim(C).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_is_sub_delim_1).
-define(NEED_is_sub_delim_1, true).
-endif.
-endif.

-ifdef(NEED_is_query_1).
is_query(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
-endif.

-ifdef(NEED_is_fragment_1).
is_fragment(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
-endif.

-ifdef(NEED_is_pchar_1).
is_pchar(C) ->  C =:= $% orelse C =:= $: orelse C =:= $@ orelse is_unreserved(C) orelse is_sub_delim(C).

-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
-ifndef(NEED_is_sub_delim_1).
-define(NEED_is_sub_delim_1, true).
-endif.
-endif.

-ifdef(NEED_is_unreserved_1).
is_unreserved(C) -> C =:= $- orelse C =:= $. orelse C =:= $_ orelse C =:= $~ orelse is_alpha(C) orelse is_digit(C).

-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-endif.

-ifdef(NEED_is_scheme_1).
is_scheme(C) -> C =:= $+ orelse C =:= $- orelse C =:= $. orelse is_alpha(C) orelse is_digit(C).

-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-endif.

-ifdef(NEED_is_ipv4_1).
is_ipv4(C) -> C =:= $. orelse is_digit(C).

-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-endif.

-ifdef(NEED_is_ipv6_1).
is_ipv6(C) -> C =:= $: orelse C =:= $. orelse is_hex_digit(C).

-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
-endif.
-endif.

-ifdef(NEED_is_alpha_1).
is_alpha(C) -> C >= $A andalso C =< $Z orelse C >= $a andalso C =< $z.
-endif.

-ifdef(NEED_is_digit_1).
is_digit(C) -> C >= $0 andalso C =< $9.
-endif.

-ifdef(NEED_is_hex_digit_1).
is_hex_digit(C) -> C >= $0 andalso C =< $9 orelse C >= $a andalso C =< $f orelse C >= $A andalso C =< $F.
-endif.

-ifdef(NEED_is_sub_delim_1).
is_sub_delim(C) ->
    C =:= $! orelse C =:= $$ orelse C =:= $& orelse C =:= $' orelse C =:= $( orelse C =:= $) orelse
    C =:= $* orelse C =:= $+ orelse C =:= $, orelse C =:= $; orelse C =:= $=.
-endif.

-ifdef(NEED_is_reserved_1).
-spec is_reserved(char()) -> boolean().
is_reserved(C) ->
    C =:= $: orelse C =:= $/ orelse C =:= $? orelse C =:= $# orelse C =:= $[ orelse C =:= $] orelse
    C =:= $@ orelse C =:= $! orelse C =:= $$ orelse C =:= $& orelse C =:= $' orelse C =:= $( orelse
    C =:= $) orelse C =:= $* orelse C =:= $+ orelse C =:= $, orelse C =:= $: orelse C =:= $=.
-endif.

-ifdef(NEED_encode_2).
encode(Component, Fun) when is_list(Component) ->
    unicode:characters_to_list(encode(unicode:characters_to_binary(Component), Fun, <<>>));
encode(Component, Fun) when is_binary(Component) -> encode(Component, Fun, <<>>).

encode(<<Char/utf8, Rest/binary>>, Fun, Acc) ->
    encode(Rest, Fun, <<Acc/binary, (encode_codepoint_binary(Char, Fun))/binary>>);
encode(<<>>, _Fun, Acc) -> Acc;
encode(B, _Fun, _Acc) when is_binary(B) -> throw({error, invalid_input, B}).

-compile({inline, encode_codepoint_binary/2}).
encode_codepoint_binary(C, Fun) ->
    case Fun(C) of
        false -> percent_encode_binary(<<C/utf8>>, <<>>);
        true -> <<C>>
    end.

-ifndef(NEED_percent_encode_binary_2).
-define(NEED_percent_encode_binary_2, true).
-endif.
-endif.

-ifdef(NEED_percent_encode_binary_2).
percent_encode_binary(<<H, Rest/binary>>, Acc) ->
    percent_encode_binary(Rest, <<Acc/binary, $%, (?DEC2HEX(H bsr 4)), (?DEC2HEX(H band 2#1111))>>);
percent_encode_binary(<<>>, Acc) -> Acc.
-endif.

-ifdef(NEED_normalize_path_segment_1).
normalize_path_segment(Map) -> maps:update_with(path, fun remove_dot_segments/1, Map).

remove_dot_segments(Path) when is_binary(Path) -> remove_dot_segments(Path, <<>>);
remove_dot_segments(Path) when is_list(Path) ->
    convert_to_list(remove_dot_segments(convert_to_binary(Path, utf8, utf8), <<>>), utf8).

remove_dot_segments(<<>>, Output) -> Output;
remove_dot_segments(<<"../", T/binary>>, Output) -> remove_dot_segments(T, Output);
remove_dot_segments(<<"./", T/binary>>, Output) -> remove_dot_segments(T, Output);
remove_dot_segments(<<"/./", T/binary>>, Output) -> remove_dot_segments(<<$/, T/binary>>, Output);
remove_dot_segments(<<"/.">>, Output) -> remove_dot_segments(<<$/>>, Output);
remove_dot_segments(<<"/../", T/binary>>, Output) -> remove_dot_segments(<<$/, T/binary>>, remove_last_segment(Output));
remove_dot_segments(<<"/..">>, Output) -> remove_dot_segments(<<$/>>, remove_last_segment(Output));
remove_dot_segments(B, Output) when B =:= <<$.>>; B =:= <<"..">> -> remove_dot_segments(<<>>, Output);
remove_dot_segments(<<C, T/binary>> = Input, Output) ->
    {First, Rest} = split_binary(Input, byte_size(first_path_segment_end(T, <<C>>))),
    remove_dot_segments(Rest, <<Output/binary, First/binary>>).

remove_last_segment(<<>>) -> <<>>;
remove_last_segment(B) ->
    S = byte_size(B) - 1,
    case B of
        <<Init:S/binary, $/>> -> Init;
        <<Init:S/binary, _>> -> remove_last_segment(Init)
    end.

first_path_segment_end(<<$/, _/binary>>, Acc) -> Acc;
first_path_segment_end(<<C, T/binary>>, Acc) -> first_path_segment_end(<<T/binary>>, <<Acc/binary, C>>);
first_path_segment_end(<<>>, Acc) -> Acc.

-ifndef(NEED_convert_to_binary_3).
-define(NEED_convert_to_binary_3, true).
-endif.
-ifndef(NEED_convert_to_list_2).
-define(NEED_convert_to_list_2, true).
-endif.
-endif.

-ifdef(NEED_convert_to_binary_3).
convert_to_binary(Binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(Binary, InEncoding, OutEncoding) of
        {T, _List, RestData} when T =:= error; T =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.
-endif.

-ifdef(NEED_convert_to_list_2).
convert_to_list(Binary, InEncoding) ->
    case unicode:characters_to_list(Binary, InEncoding) of
        {T, _List, RestData} when T =:= error; T =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.
-endif.
