-module(otpbp_uri_string).

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

-ifndef(HAVE_uri_string__resolve_2).
-ifdef(HAVE_uri_string__resolve_3).
-import(uri_string, [resolve/3]).
-endif.
-endif.
-ifndef(HAVE_uri_string__percent_decode_1).
-ifdef(HAVE_uri_string__unquote_1).
-import(uri_string, [unquote/1]).
-endif.
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

-ifndef(HAVE_uri_string__allowed_characters_0).
allowed_characters() ->
    Input = lists:seq(0, 127),
    lists:keymap(fun(F) -> lists:filter(F, Input) end, 1,
                 [{scheme, fun is_scheme/1},
                  {userinfo, fun is_userinfo/1},
                  {host, fun uri_string:is_host/1},
                  {ipv4, fun is_ipv4/1},
                  {ipv6, fun is_ipv6/1},
                  {regname, fun is_reg_name/1},
                  {path, fun uri_string:is_path/1},
                  {query, fun is_query/1},
                  {fragment, fun is_fragment/1},
                  {reserved, fun is_reserved/1},
                  {unreserved, fun is_unreserved/1}]).

is_reserved(C) ->
    C =:= $: orelse C =:= $/ orelse C =:= $? orelse C =:= $# orelse C =:= $[ orelse C =:= $] orelse
    C =:= $@ orelse C =:= $! orelse C =:= $$ orelse C =:= $& orelse C =:= $' orelse C =:= $( orelse
    C =:= $) orelse C =:= $* orelse C =:= $+ orelse C =:= $, orelse C =:= $: orelse C =:= $=.

is_ipv6(C) -> C =:= $: orelse C =:= $. orelse is_hex_digit(C).

is_ipv4(C) -> C =:= $. orelse is_digit(C).

is_scheme(C) -> C =:= $+ orelse C =:= $- orelse C =:= $. orelse is_alpha(C) orelse is_digit(C).

is_fragment(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

is_query(C) -> C =:= $/ orelse C =:= $? orelse is_pchar(C).

is_pchar(C) ->  C =:= $% orelse C =:= $: orelse C =:= $@ orelse is_unreserved(C) orelse is_sub_delim(C).

is_reg_name(C) -> C =:= $% orelse is_unreserved(C) orelse is_sub_delim(C).

is_userinfo(C) -> C =:= $% orelse C =:= $: orelse is_unreserved(C) orelse is_sub_delim(C).

is_sub_delim(C) ->
    C =:= $! orelse C =:= $$ orelse C =:= $& orelse C =:= $' orelse C =:= $( orelse C =:= $) orelse
    C =:= $* orelse C =:= $+ orelse C =:= $, orelse C =:= $; orelse C =:= $=.

-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
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

check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {E, _, _} when E =:= incomplete; E =:= error -> throw({error, invalid_utf8, Cs});
        _ -> Cs
    end.

-ifndef(NEED_is_hex_digit_1).
-define(NEED_is_hex_digit_1, true).
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
                [] -> uri_string:recompose(TargetURIMap)
            end;
        Error -> Error
    end;
resolve(URIString, BaseURIMap, Options) ->
    case uri_string:parse(URIString) of
        URIMap when is_map(URIMap) -> resolve(URIMap, BaseURIMap, Options);
        Error -> Error
    end.

-compile({inline, resolve_map/2}).
resolve_map(#{scheme := _} = URIMap, _) -> normalize_path_segment(URIMap);
resolve_map(URIMap, #{scheme := _} = BaseURIMap) -> resolve_map(URIMap, BaseURIMap, resolve_path_type(URIMap));
resolve_map(_URIMap, BaseURIMap) when is_map(BaseURIMap) -> {error, invalid_scheme, ""};
resolve_map(URIMap, BaseURIString) ->
    case uri_string:parse(BaseURIString) of
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

-compile({inline, convert_to_list/2}).
convert_to_list(Binary, InEncoding) ->
    case unicode:characters_to_list(Binary, InEncoding) of
        {T, _List, RestData} when T =:= error; T =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.

-compile({inline, convert_to_binary/3}).
convert_to_binary(Binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(Binary, InEncoding, OutEncoding) of
        {T, _List, RestData} when T =:= error; T =:= incomplete -> throw({error, invalid_input, RestData});
        Result -> Result
    end.
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

-ifdef(NEED_is_alpha_1).
is_alpha(C) -> C >= $A andalso C =< $Z orelse C >= $a andalso C =< $z.
-endif.

-ifdef(NEED_is_digit_1).
is_digit(C) -> C >= $0 andalso C =< $9.
-endif.

-ifdef(NEED_is_hex_digit_1).
is_hex_digit(C) -> C >= $0 andalso C =< $9 orelse C >= $a andalso C =< $f orelse C >= $A andalso C =< $F.
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

-compile({inline, percent_encode_binary/2}).
percent_encode_binary(<<H, Rest/binary>>, Acc) ->
    percent_encode_binary(Rest, <<Acc/binary, $%, (?DEC2HEX(H bsr 4)), (?DEC2HEX(H band 2#1111))>>);
percent_encode_binary(<<>>, Acc) -> Acc.
-endif.
