-module(otpbp_string).

-compile({parse_transform, otpbp_pt}).
-compile({no_auto_import, [length/1]}).

-ifndef(HAVE_string__casefold_1).
% OTP 20.0
-export([casefold/1]).
-endif.
-ifndef(HAVE_string__chomp_1).
% OTP 20.0
-export([chomp/1]).
-endif.
-ifndef(HAVE_string__equal_3).
% OTP 20.0
-export([equal/3]).
-export([equal/2]).
-endif.
-ifndef(HAVE_string__equal_4).
% OTP 20.0
-export([equal/4]).
-endif.
-ifndef(HAVE_string__find_2).
% OTP 20.0
-export([find/2]).
-endif.
-ifndef(HAVE_string__find_3).
% OTP 20.0
-export([find/3]).
-endif.
-ifndef(HAVE_string__is_empty_1).
% OTP 20.0
-export([is_empty/1]).
-endif.
-ifndef(HAVE_string__length_1).
% OTP 20.0
-export([length/1]).
-endif.
-ifndef(HAVE_string__lexemes_2).
% OTP 20.0
-export([lexemes/2]).
-endif.
-ifndef(HAVE_string__lowercase_1).
% OTP 20.0
-export([lowercase/1]).
-endif.
-ifndef(HAVE_string__nth_lexeme_3).
% OTP 20.0
-export([nth_lexeme/3]).
-endif.
-ifndef(HAVE_string__pad_2).
% OTP 20.0
-export([pad/2]).
-endif.
-ifndef(HAVE_string__pad_3).
% OTP 20.0
-export([pad/3]).
-endif.
-ifndef(HAVE_string__pad_4).
% OTP 20.0
-export([pad/4]).
-endif.
-ifndef(HAVE_string__prefix_2).
% OTP 20.0
-export([prefix/2]).
-endif.
-ifndef(HAVE_string__replace_3).
% OTP 20.0
-export([replace/3]).
-endif.
-ifndef(HAVE_string__replace_4).
% OTP 20.0
-export([replace/4]).
-endif.
-ifndef(HAVE_string__reverse_1).
% OTP 20.0
-export([reverse/1]).
-endif.
-ifndef(HAVE_string__slice_2).
% OTP 20.0
-export([slice/2]).
-endif.
-ifndef(HAVE_string__slice_3).
% OTP 20.0
-export([slice/3]).
-endif.
-ifndef(HAVE_string__split_2).
% OTP 20.0
-export([split/2]).
-endif.
-ifndef(HAVE_string__split_3).
% OTP 20.0
-export([split/3]).
-endif.
-ifndef(HAVE_string__take_2).
% OTP 20.0
-export([take/2]).
-endif.
-ifndef(HAVE_string__take_3).
% OTP 20.0
-export([take/3]).
-endif.
-ifndef(HAVE_string__take_4).
% OTP 20.0
-export([take/4]).
-endif.
-ifndef(HAVE_string__titlecase_1).
% OTP 20.0
-export([titlecase/1]).
-endif.
-ifndef(HAVE_string__to_graphemes_1).
% OTP 20.0
-export([to_graphemes/1]).
-endif.
-ifndef(HAVE_string__list_to_float_1).
% OTP 20.0
-export([to_float/1]).
-endif.
-ifndef(HAVE_string__list_to_integer_1).
% OTP 20.0
-export([to_integer/1]).
-endif.
-ifndef(HAVE_string__trim_1).
% OTP 20.0
-export([trim/1]).
-endif.
-ifndef(HAVE_string__trim_2).
% OTP 20.0
-export([trim/2]).
-endif.
-ifndef(HAVE_string__trim_3).
% OTP 20.0
-export([trim/3]).
-endif.
-ifndef(HAVE_string__uppercase_1).
% OTP 20.0
-export([uppercase/1]).
-endif.
-ifndef(HAVE_string__jaro_similarity_2).
% OTP 27.0
-export([jaro_similarity/2]).
-endif.

-ifndef(HAVE_string__chomp_1).
-ifdef(HAVE_string__trim_3).
-import(string, [trim/3]).
-endif.
-endif.
-ifndef(HAVE_string__equal_3).
-ifdef(HAVE_string__is_empty_1).
-import(string, [is_empty/1]).
-endif.
-endif.
-ifndef(HAVE_string__equal_4).
-ifdef(HAVE_string__equal_3).
-import(string, [equal/3]).
-endif.
-endif.
-ifndef(HAVE_string__find_2).
-ifdef(HAVE_string__find_3).
-import(string, [find/3]).
-endif.
-endif.
-ifndef(HAVE_string__pad_2).
-ifdef(HAVE_string__pad_3).
-import(string, [pad/3]).
-endif.
-endif.
-ifndef(HAVE_string__pad_3).
-ifdef(HAVE_string__pad_4).
-import(string, [pad/4]).
-endif.
-endif.
-ifndef(HAVE_string__pad_4).
-ifdef(HAVE_string__length_1).
-import(string, [length/1]).
-endif.
-endif.
-ifndef(HAVE_string__replace_3).
-ifdef(HAVE_string__split_2).
-import(string, [split/2]).
-endif.
-endif.
-ifndef(HAVE_string__replace_4).
-ifdef(HAVE_string__split_3).
-import(string, [split/3]).
-endif.
-endif.
-ifndef(HAVE_string__split_2).
-ifdef(HAVE_string__split_3).
-import(string, [split/3]).
-import(string, [is_empty/1]).
-endif.
-endif.
-ifndef(HAVE_string__take_2).
-ifdef(HAVE_string__take_3).
-import(string, [take/3]).
-endif.
-endif.
-ifndef(HAVE_string__take_3).
-ifdef(HAVE_string__take_4).
-import(string, [take/4]).
-endif.
-endif.
-ifndef(HAVE_string__take_4).
-ifdef(HAVE_string__is_empty_1).
-import(string, [is_empty/1]).
-endif.
-endif.
-ifndef(HAVE_string__trim_1).
-ifdef(HAVE_string__trim_2).
-import(string, [trim/2]).
-endif.
-endif.
-ifndef(HAVE_string__trim_2).
-ifdef(HAVE_string__trim_3).
-import(string, [trim/3]).
-endif.
-endif.
-ifndef(HAVE_string__trim_3).
-ifdef(HAVE_string__is_empty_1).
-import(string, [is_empty/1]).
-endif.
-endif.

-define(ASCII_LIST(CP1, CP2), is_integer(CP1), CP1 >= 0, CP1 < 256, is_integer(CP2), CP2 >= 0, CP2 < 256, CP1 =/= $\r).

-ifndef(HAVE_string__casefold_1).
casefold(CD) when is_list(CD) ->
    try
        casefold_list(CD, false)
    catch
        throw:unchanged -> CD
    end;
casefold(<<CP1/utf8, Rest/binary>> = Orig) ->
    try casefold_bin(CP1, Rest, false) of
        List -> unicode:characters_to_binary(List)
    catch
        throw:unchanged -> Orig
    end;
casefold(<<>>) -> <<>>;
casefold(Bin) -> error({badarg, Bin}).

casefold_list([CP1|[CP2|_] = Cont], _Changed)
  when is_integer(CP1), CP1 >= $A, CP1 =< $Z, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1 + $\s|casefold_list(Cont, true)];
casefold_list([CP1|[CP2|_] = Cont], Changed)
  when is_integer(CP1), CP1 >= 0, CP1 < 128, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1|casefold_list(Cont, Changed)];
casefold_list([], true) -> [];
casefold_list([], false) -> throw(unchanged);
casefold_list(CPs0, Changed) ->
    case unicode_util:casefold(CPs0) of
        [Char|CPs] when Char =:= hd(CPs0) -> [Char|casefold_list(CPs, Changed)];
        [Char|CPs] -> append(Char, casefold_list(CPs, true));
        [] -> casefold_list([], Changed)
    end.

casefold_bin(CP1, <<CP2/utf8, Bin/binary>>, _Changed) when is_integer(CP1), CP1 >= $A, CP1 =< $Z, CP2 < 256 ->
    [CP1 + $\s|casefold_bin(CP2, Bin, true)];
casefold_bin(CP1, <<CP2/utf8, Bin/binary>>, Changed) when is_integer(CP1), CP1 >= 0, CP1 < 128, CP2 < 256 ->
    [CP1|casefold_bin(CP2, Bin, Changed)];
casefold_bin(CP1, Bin, Changed) ->
    case unicode_util:casefold([CP1|Bin]) of
        [CP1|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [CP1|casefold_bin(Next, Rest, Changed)];
                [] when Changed -> [CP1];
                [] -> throw(unchanged);
                {error, Err} -> error({badarg, Err})
            end;
        [Char|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [Char|casefold_bin(Next, Rest, true)];
                [] -> [Char];
                {error, Err} -> error({badarg, Err})
            end
    end.

-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-endif.

-ifndef(HAVE_string__chomp_1).
chomp(Str) -> trim(Str, trailing, ["\r\n", $\n]).
-endif.

-ifndef(HAVE_string__equal_3).
equal(A, B) -> is_binary(A) andalso A =:= B orelse equal_(A, B).

equal(A, B, false) -> equal(A, B);
equal(A, B, true) -> equal_nocase(A, B).

equal_([A|AR], [B|BR]) when is_integer(A), is_integer(B) -> A =:= B andalso equal_(AR, BR);
equal_([], BR) -> is_empty(BR);
equal_(A0, B0) ->
    case {unicode_util:cp(A0), unicode_util:cp(B0)} of
        {[CP|A], [CP|B]} -> equal_(A, B);
        {[], []} -> true;
        {L1, L2} when is_list(L1), is_list(L2) -> false
    end.

equal_nocase(A, A) -> true;
equal_nocase(A0, B0) ->
    case {unicode_util:cp(unicode_util:casefold(A0)), unicode_util:cp(unicode_util:casefold(B0))} of
        {[CP|A], [CP|B]} -> equal_nocase(A, B);
        {[], []} -> true;
        {L1, L2} when is_list(L1), is_list(L2) -> false
    end.
-endif.

-ifndef(HAVE_string__equal_4).
equal(A, B, Case, none) -> equal(A, B, Case);
equal(A, B, false, Norm) -> equal_norm(A, B, Norm);
equal(A, B, true, Norm) -> equal_norm_nocase(A, B, Norm).

equal_norm(A0, B0, Norm) ->
    A0 =:= B0 orelse
        case {unicode_util:cp(unicode_util:Norm(A0)), unicode_util:cp(unicode_util:Norm(B0))} of
            {[CP|A], [CP|B]} -> equal_norm(A, B, Norm);
            {[], []} -> true;
            {L1, L2} when is_list(L1), is_list(L2) -> false
        end.

equal_norm_nocase(A0, B0, Norm) ->
    A0 =:= B0 orelse
        case {unicode_util:cp(unicode_util:casefold(unicode_util:Norm(A0))),
              unicode_util:cp(unicode_util:casefold(unicode_util:Norm(B0)))} of
            {[CP|A], [CP|B]} -> equal_norm_nocase(A, B, Norm);
            {[], []} -> true;
            {L1, L2} when is_list(L1), is_list(L2) -> false
        end.
-endif.

-ifndef(HAVE_string__find_2).
find(String, SearchPattern) -> find(String, SearchPattern, leading).
-endif.

-ifndef(HAVE_string__find_3).
find(String, SearchPattern, _) when SearchPattern =:= ""; SearchPattern =:= <<>> -> String;
find(String, SearchPattern, leading) -> find_l(String, unicode:characters_to_list(SearchPattern));
find(String, SearchPattern, trailing) -> find_r(String, SearchPattern).

find_l([C1|Cs] = Cs0, [C|_] = Needle) when is_integer(C1) ->
    case C1 =/= C orelse prefix_(Cs0, Needle) =:= nomatch of
        true -> find_l(Cs, Needle);
        _false -> Cs0
    end;
find_l([Bin|Cont0], Needle) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch, _, Cont} -> find_l(Cont, Needle);
        {_Before, Cs, _After} -> Cs
    end;
find_l(Cs0, [C|_] = Needle) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_(Cs0, Needle) of
                nomatch -> find_l(Cs, Needle);
                _ -> Cs0
            end;
        [_C|Cs] -> find_l(Cs, Needle);
        [] -> nomatch
    end;
find_l(Bin, Needle) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch, _, _} -> nomatch;
        {_Before, [Cs], _After} -> Cs
    end.

-compile({inline, find_r/2}).
find_r(String, SearchPattern) -> find_r(String, unicode:characters_to_list(SearchPattern), nomatch).

find_r([Cp|Cs] = Cs0, [C|_] = Needle, Res) when is_integer(Cp) ->
    find_r(Cs, Needle,
           case Cp =/= C orelse prefix_(Cs0, Needle) =:= nomatch of
               true -> Res;
                   _false -> Cs0
           end);
find_r([Bin|Cont0], Needle, Res) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch, _, Cont} -> find_r(Cont, Needle, Res);
        {_, Cs, _} -> find_r(tl(unicode_util:gc(Cs)), Needle, Cs)
    end;
find_r(Cs0, [C|_] = Needle, Res) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            find_r(Cs, Needle,
                   case prefix_(Cs0, Needle) of
                       nomatch -> Res;
                       _ -> Cs0
                   end);
        [_C|Cs] -> find_r(Cs, Needle, Res);
        [] -> Res
    end;
find_r(Bin, Needle, Res) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch, _, _} -> Res;
        {_Before, [Cs0], _After} ->
            <<_/utf8, Cs/binary>> = Cs0,
            find_r(Cs, Needle, Cs0)
    end.

-ifndef(NEED_prefix__2).
-define(NEED_prefix__2, true).
-endif.
-ifndef(NEED_bin_search_str_4).
-define(NEED_bin_search_str_4, true).
-endif.
-endif.

-ifndef(HAVE_string__is_empty_1).
is_empty([L|R]) -> is_empty(L) andalso is_empty(R);
is_empty(S) -> S =:= [] orelse S =:= <<>>.
-endif.

-ifndef(HAVE_string__jaro_similarity_2).
jaro_similarity(A0, B0) ->
    case {str_to_gcl_and_length(A0), str_to_indexmap(B0)} of
        {{_, 0}, [_|0]} -> 1.0;
        {{_, ALen}, [_|BLen]} when ALen =:= 0; BLen =:= 0 -> 0.0;
        {{A, ALen}, [B|BLen]} ->
            case jaro_match(A, B, max(1, max(ALen, BLen) div 2)) of
                {[], _} -> 0.0;
                {AM, BM} ->
                    {M, T} = jaro_calc_mt(AM, BM, 0, 0),
                    (M / ALen + M / BLen + (M - T / 2) / M) / 3
            end
    end.

-compile({inline, jaro_match/3}).
jaro_match(A, B, Dist) -> jaro_match(A, B, -Dist, Dist, [], []).

jaro_match([A|As], B, Min, Max, AM, BM) ->
    Min1 = Min + 1,
    Max1 = Max + 1,
    case jaro_detect(maps:get(A, B, []), Min, Max) of
        false -> jaro_match(As, B, Min1, Max1, AM, BM);
        {J, Remain} -> jaro_match(As, B#{A => Remain}, Min1, Max1, [A|AM], add_rsorted({J, A}, BM))
    end;
jaro_match(_A, _B, _Min, _Max, AM, BM) -> {AM, BM}.

jaro_detect([Idx|Rest], Min, Max) when Idx < Max ->
    if
        Min < Idx -> {Idx, Rest};
        true -> jaro_detect(Rest, Min, Max)
    end;
jaro_detect(_, _, _) -> false.

jaro_calc_mt([Char|AM], [{_, Char}|BM], M, T) -> jaro_calc_mt(AM, BM, M + 1, T);
jaro_calc_mt([_|AM], [_|BM], M, T) -> jaro_calc_mt(AM, BM, M + 1, T + 1);
jaro_calc_mt([], [], M, T) -> {M, T}.

add_rsorted(A, [H|_] = BM) when A > H -> [A|BM];
add_rsorted(A, [H|BM]) -> [H|add_rsorted(A, BM)];
add_rsorted(A, []) -> [A].

-compile({inline, str_to_gcl_and_length/1}).
str_to_gcl_and_length(S0) -> gcl_and_length(unicode_util:gc(S0), [], 0).

gcl_and_length([C|Str], Acc, N) -> gcl_and_length(unicode_util:gc(Str), [C|Acc], N + 1);
gcl_and_length([], Acc, N) -> {lists:reverse(Acc), N};
gcl_and_length({error, Err}, _, _) -> error({badarg, Err}).

-compile({inline, str_to_indexmap/1}).
str_to_indexmap(S) -> str_to_map(unicode_util:gc(S), 0).

str_to_map([G|Gs], I) ->
    [M|L] = str_to_map(unicode_util:gc(Gs), I + 1),
    [M#{G => [I|maps:get(G, M, [])]}|L];
str_to_map([], L) -> [#{}|L];
str_to_map({error,Error}, _) -> error({badarg, Error}).
-endif.

-ifndef(HAVE_string__length_1).
length(<<CP1/utf8, Bin/binary>>) -> length(Bin, CP1, 0);
length(CD) -> length(CD, 0).

length([CP1|[CP2|_] = Cont], N) when ?ASCII_LIST(CP1, CP2) -> length(Cont, N + 1);
length(Str, N) ->
    case unicode_util:gc(Str) of
        [] -> N;
        [_|Rest] -> length(Rest, N + 1);
        {error, Err} -> error({badarg, Err})
    end.

length(<<CP2/utf8, Rest/binary>>, CP1, N) when ?ASCII_LIST(CP1, CP2) -> length(Rest, CP2, N + 1);
length(Bin0, CP1, N) ->
    case unicode_util:cp(tl(unicode_util:gc([CP1|Bin0]))) of
        [] -> N + 1;
        [CP3|Bin] -> length(Bin, CP3, N + 1);
        {error, Err} -> error({badarg, Err})
    end.
-endif.

-ifndef(HAVE_string__lexemes_2).
lexemes([], _) -> [];
lexemes(Str, []) -> [Str];
lexemes(Str, Seps) when is_list(Seps) -> lexemes(Str, search_pattern(Seps), []).

lexemes([CP|_] = Cs0, {GCs, CPs, _} = Seps0, Ts) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true -> lexemes(Cs2, Seps0, Ts);
                false -> lexemes_(Cs0, Seps0, Ts)
            end;
        false -> lexemes_(Cs0, Seps0, Ts)
    end;
lexemes([Bin|Cont0], {GCs, _, _} = Seps0, Ts) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, GCs) of
        {nomatch, Cont} -> lexemes(Cont, Seps0, Ts);
        Cs -> lexemes_(Cs, Seps0, Ts)
    end;
lexemes(Cs0, {GCs, _, _} = Seps0, Ts) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                true -> lexemes(Cs, Seps0, Ts);
                false -> lexemes_(Cs0, Seps0, Ts)
            end;
        [] -> lists:reverse(Ts)
    end;
lexemes(Bin, {GCs, _, _} = Seps0, Ts) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], GCs) of
        {nomatch, _} -> lists:reverse(Ts);
        [Cs] ->
            Seps = search_compile(Seps0),
            {Lexeme, Rest} = lexeme_pick(Cs, Seps, []),
            lexemes(Rest, Seps,
                    if
                        Lexeme =:= <<>> -> Rest;
                        true -> [Lexeme|Ts]
                    end)
    end.

lexemes_(Cs, Seps0, Ts) ->
    Seps = search_compile(Seps0),
    {Lexeme, Rest} = lexeme_pick(Cs, Seps, []),
    lexemes(Rest, Seps, [Lexeme|Ts]).


-ifndef(NEED_search_pattern_1).
-define(NEED_search_pattern_1, true).
-endif.
-ifndef(NEED_search_compile_1).
-define(NEED_search_compile_1, true).
-endif.
-ifndef(NEED_bin_search_inv_3).
-define(NEED_bin_search_inv_3, true).
-endif.
-ifndef(NEED_lexeme_pick_3).
-define(NEED_lexeme_pick_3, true).
-endif.
-endif.

-ifndef(HAVE_string__lowercase_1).
lowercase(CD) when is_list(CD) ->
    try
        lowercase_list(CD, false)
    catch
        throw:unchanged -> CD
    end;
lowercase(<<CP1/utf8, Rest/binary>> = Orig) ->
    try lowercase_bin(CP1, Rest, false) of
        List -> unicode:characters_to_binary(List)
    catch
        throw:unchanged -> Orig
    end;
lowercase(<<>>) -> <<>>;
lowercase(Bin) -> error({badarg, Bin}).

lowercase_list([CP1|[CP2|_] = Cont], _Changed)
  when is_integer(CP1), CP1 >= $A, CP1 =< $Z, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1 + $\s|lowercase_list(Cont, true)];
lowercase_list([CP1|[CP2|_]=Cont], Changed)
  when is_integer(CP1), CP1 >= 0, CP1 < 128, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1|lowercase_list(Cont, Changed)];
lowercase_list([], true) -> [];
lowercase_list([], false) -> throw(unchanged);
lowercase_list(CPs0, Changed) ->
    case unicode_util:lowercase(CPs0) of
        [Char|CPs] when Char =:= hd(CPs0) -> [Char|lowercase_list(CPs, Changed)];
        [Char|CPs] -> append(Char, lowercase_list(CPs, true));
        [] -> lowercase_list([], Changed)
    end.

lowercase_bin(CP1, <<CP2/utf8, Bin/binary>>, _Changed) when is_integer(CP1), CP1 >= $A, CP1 =< $Z, CP2 < 256 ->
    [CP1 + $\s|lowercase_bin(CP2, Bin, true)];
lowercase_bin(CP1, <<CP2/utf8, Bin/binary>>, Changed) when is_integer(CP1), CP1 >= 0, CP1 < 128, CP2 < 256 ->
    [CP1|lowercase_bin(CP2, Bin, Changed)];
lowercase_bin(CP1, Bin, Changed) ->
    case unicode_util:lowercase([CP1|Bin]) of
        [CP1|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [CP1|lowercase_bin(Next, Rest, Changed)];
                [] when Changed -> [CP1];
                [] -> throw(unchanged);
                {error, Err} -> error({badarg, Err})
            end;
        [Char|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [Char|lowercase_bin(Next, Rest, true)];
                [] -> [Char];
                {error, Err} -> error({badarg, Err})
            end
    end.
-endif.

-ifndef(HAVE_string__nth_lexeme_3).
nth_lexeme(Str, 1, []) -> Str;
nth_lexeme(Str, N, Seps) when is_list(Seps), is_integer(N), N > 0 -> nth_lexeme_(Str, search_pattern(Seps), N).

nth_lexeme_([Bin|Cont0], {GCs, _, _} = Seps, N) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, GCs) of
        {nomatch, Cont} -> nth_lexeme_(Cont, Seps, N);
        Cs when N > 1 -> nth_lexeme_(lexeme_skip(Cs, Seps), Seps, N - 1);
        Cs ->
            {Lexeme, _} = lexeme_pick(Cs, search_compile(Seps), []),
            Lexeme
    end;
nth_lexeme_(Cs0, {GCs, _, _} = Seps, N) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                true -> nth_lexeme_(Cs, Seps, N);
                false when N > 1 -> nth_lexeme_(lexeme_skip(Cs, Seps), Seps, N - 1);
                false ->
                    {Lexeme, _} = lexeme_pick(Cs0, search_compile(Seps), []),
                    Lexeme
            end;
        [] -> []
    end;
nth_lexeme_(Bin, {GCs, _, _} = Seps0, N) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], GCs) of
        [Cs] when N > 1 ->
            Seps = search_compile(Seps0),
            nth_lexeme_(lexeme_skip(Cs, Seps), Seps, N - 1);
        [Cs] ->
            {Lexeme, _} = lexeme_pick(Cs, search_compile(Seps0), []),
            Lexeme;
        {nomatch, _} -> <<>>
    end.

lexeme_skip([CP|Cs1] = Cs0, {GCs, CPs, _} = Seps) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true  ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true -> Cs2;
                false -> lexeme_skip(Cs2, Seps)
            end;
        false -> lexeme_skip(Cs1, Seps)
    end;
lexeme_skip([Bin|Cont0], Seps0) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, Cont0, Seps) of
        {nomatch, _} -> lexeme_skip(Cont0, Seps);
        Cs -> tl(unicode_util:gc(Cs))
    end;
lexeme_skip(Cs0, {GCs, CPs, _} = Seps) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [CP|Cs] ->
            case lists:member(CP, CPs) of
                true ->
                    [GC|Cs2] = unicode_util:gc(Cs0),
                    case lists:member(GC, GCs) of
                        true -> Cs2;
                        false -> lexeme_skip(Cs2, Seps)
                    end;
                false -> lexeme_skip(Cs, Seps)
            end;
        [] -> []
    end;
lexeme_skip(Bin, Seps) when is_binary(Bin) ->
    case bin_search(Bin, [], search_compile(Seps)) of
        {nomatch, _} -> <<>>;
        [Left] -> tl(unicode_util:gc(Left))
    end.

-ifndef(NEED_search_pattern_1).
-define(NEED_search_pattern_1, true).
-endif.
-ifndef(NEED_search_compile_1).
-define(NEED_search_compile_1, true).
-endif.
-ifndef(NEED_bin_search_inv_3).
-define(NEED_bin_search_inv_3, true).
-endif.
-ifndef(NEED_bin_search_3).
-define(NEED_bin_search_3, true).
-endif.
-endif.

-ifndef(HAVE_string__pad_2).
pad(CD, Length) -> pad(CD, Length, trailing).
-endif.

-ifndef(HAVE_string__pad_3).
pad(CD, Length, Dir) -> pad(CD, Length, Dir, $\s).
-endif.

-ifndef(HAVE_string__pad_4).
pad(CD, Length, Dir, Char) when is_integer(Length) -> pad_(CD, max(0, Length - length(CD)), Dir, Char).

pad_(CD, Size, both, Char) ->
    Pad = lists:duplicate(Size div 2, Char),
    case Size rem 2 of
        0 -> [Pad, CD, Pad];
        _ -> [Pad, CD, Pad, Char]
    end;
pad_(CD, Size, Dir, Char) ->
    Pad = lists:duplicate(Size, Char),
    if
        Dir =:= leading -> [Pad, CD];
        Dir =:= trailing -> [CD|Pad]
    end.
-endif.

-ifndef(HAVE_string__prefix_2).
prefix(Str, Prefix0) ->
    case unicode:characters_to_list(Prefix0) of
        [] -> [];
        Prefix ->
            case prefix_(Str, Prefix) of
                [] when is_binary(Str) -> <<>>;
                Res -> Res
            end
    end.

-ifndef(NEED_prefix__2).
-define(NEED_prefix__2, true).
-endif.
-endif.

-ifndef(HAVE_string__replace_3).
replace(String, SearchPattern, Replacement) -> lists:join(Replacement, split(String, SearchPattern)).
-endif.

-ifndef(HAVE_string__replace_4).
replace(String, SearchPattern, Replacement, Where) -> lists:join(Replacement, split(String, SearchPattern, Where)).
-endif.

-ifndef(HAVE_string__reverse_1).
reverse(<<CP1/utf8, Rest/binary>>) -> reverse(Rest, CP1, []);
reverse(CD) -> reverse(CD, []).

reverse([CP1|[CP2|_] = Cont], Acc) when ?ASCII_LIST(CP1, CP2) -> reverse(Cont, [CP1|Acc]);
reverse(CD, Acc) ->
    case unicode_util:gc(CD) of
        [GC|Rest] -> reverse(Rest, [GC|Acc]);
        [] -> Acc;
        {error, Err} -> error({badarg, Err})
    end.

reverse(<<CP2/utf8, Rest/binary>>, CP1, Acc) when ?ASCII_LIST(CP1, CP2) -> reverse(Rest, CP2,  [CP1|Acc]);
reverse(Bin0, CP1, Acc) ->
    [GC|Bin1] = unicode_util:gc([CP1|Bin0]),
    case unicode_util:cp(Bin1) of
        [] -> [GC|Acc];
        [CP3|Bin] -> reverse(Bin, CP3, [GC|Acc]);
        {error, Err} -> error({badarg, Err})
    end.
-endif.

-ifndef(HAVE_string__slice_2).
slice(CD, 0) -> CD;
slice(CD, N) when is_integer(N), N >= 0 ->
    case slice_l0(CD, N) of
        [] when is_binary(CD) -> <<>>;
        Res -> Res
    end.

-ifndef(NEED_slice_l0_2).
-define(NEED_slice_l0_2, true).
-endif.
-endif.

-ifndef(HAVE_string__slice_3).
slice(CD, _, 0) ->
    if
        is_binary(CD) -> <<>>;
        true -> []
    end;
slice(CD, N, Length) when is_integer(N), N >= 0 ->
    slice_(Length,
           case slice_l0(CD, N) of
               [] when is_binary(CD) -> <<>>;
               Res -> Res
           end).

-compile({inline, slice_/2}).
slice_(infinity, Res) -> Res;
slice_(Length, Res) when is_integer(Length), Length > 0 -> slice_trail(Res, Length).

-compile({inline, slice_trail/2}).
slice_trail(<<CP1/utf8, Bin/binary>> = Orig, N) when N > 0 ->
    binary:part(Orig, 0, byte_size(Orig) - slice_bin(Bin, CP1, N));
slice_trail(<<_, _/binary>> = Orig, N) when N > 0 -> error({badarg, Orig});
slice_trail(Orig, _) when is_binary(Orig) -> <<>>;
slice_trail(CD, N) when is_list(CD) -> slice_list(CD, N).

slice_list(_, 0) -> [];
slice_list(CD, N) when N > 0 ->
    case CD of
        [CP1|[CP2|_] = Cont] when ?ASCII_LIST(CP1, CP2) -> [CP1|slice_list(Cont, N - 1)];
        _ ->
            case unicode_util:gc(CD) of
                [GC|Cont] -> append(GC, slice_list(Cont, N - 1));
                [] -> [];
                {error, Err} -> error({badarg, Err})
            end
    end.

slice_bin(CD, CP1, 0) -> byte_size(CD) + byte_size(<<CP1/utf8>>);
slice_bin(CD, CP1, N) when N > 0 ->
    case CD of
        <<CP2/utf8, Bin/binary>> when ?ASCII_LIST(CP1, CP2) -> slice_bin(Bin, CP2, N - 1);
        _ ->
            case unicode_util:cp(tl(unicode_util:gc([CP1|CD]))) of
                [CP2|Cont] -> slice_bin(Cont, CP2, N - 1);
                [] -> 0;
                {error, Err} -> error({badarg, Err})
            end
    end.

-ifndef(NEED_slice_l0_2).
-define(NEED_slice_l0_2, true).
-endif.
-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-endif.

-ifndef(HAVE_string__split_2).
split(String, SearchPattern) -> split(String, SearchPattern, leading).
-endif.

-ifndef(HAVE_string__split_3).
split(String, SearchPattern, Where) ->
    case is_empty(SearchPattern) of
        true -> [String];
        _false ->
            case split(String, unicode:characters_to_list(SearchPattern), 0, Where, [], []) of
                {_Curr, []} -> [String];
                {_Curr, Acc} when Where =:= trailing -> Acc;
                {Curr, Acc} when Where =:= all -> lists:reverse(Acc, [Curr]);
                Acc when is_list(Acc) -> Acc
            end
    end.

split([C|Cs] = Cs0, [C|_] = Needle, _, Where, Curr, Acc) when is_integer(C) ->
    case prefix_(Cs0, Needle) of
        nomatch -> split(Cs, Needle, 0, Where, append(C, Curr), Acc);
        Rest when Where =:= leading -> [rev(Curr), Rest];
        Rest when Where =:= trailing -> split(Cs, Needle, 0, Where, [C|Curr], [rev(Curr), Rest]);
        Rest when Where =:= all -> split(Rest, Needle, 0, Where, [], [rev(Curr)|Acc])
    end;
split([CP1|Cs], [_|_] = Needle, _, Where, Curr, Acc) when is_integer(CP1) ->
    split(Cs, Needle, 0, Where, append(CP1, Curr), Acc);
split([Bin|Cont0], Needle, Start, Where, Curr0, Acc) when is_binary(Bin) ->
    case bin_search_str(Bin, Start, Cont0, Needle) of
        {nomatch, Sz, Cont} -> split(Cont, Needle, 0, Where, [binary:part(Bin, 0, Sz)|Curr0], Acc);
        {Before, [Cs0|Cont], After} ->
            Curr = rev(add_non_empty(Before, Curr0)),
            if
                Where =:= leading -> [Curr, After];
                Where =:= trailing ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    split([Bin|Cont], Needle, byte_size(Bin) - byte_size(Cs), Where, Curr0, [Curr, After]);
                Where =:= all -> split(After, Needle, 0, Where, [], [Curr|Acc])
            end
    end;
split(Cs0, [C|_] = Needle, _, Where, Curr, Acc) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_(Cs0, Needle) of
                nomatch -> split(Cs, Needle, 0, Where, append(C, Curr), Acc);
                Rest when Where =:= leading -> [rev(Curr), Rest];
                Rest when Where =:= trailing -> split(Cs, Needle, 0, Where, [C|Curr], [rev(Curr), Rest]);
                Rest when Where =:= all -> split(Rest, Needle, 0, Where, [], [rev(Curr)|Acc])
            end;
        [Other|Cs] -> split(Cs, Needle, 0, Where, append(Other, Curr), Acc);
        [] -> {rev(Curr), Acc}
    end;
split(Bin, [_C|_] = Needle, Start, Where, Curr0, Acc) ->
    case bin_search_str(Bin, Start, [], Needle) of
        {nomatch, _, _} ->
            <<_:Start/binary, Keep/binary>> = Bin,
            {rev([Keep|Curr0]), Acc};
        {Before, [_], After} when Where =:= leading -> [rev([Before|Curr0]), After];
        {Before, [<<_/utf8, Cs/binary>>], After} when Where =:= trailing ->
            split(Bin, Needle, byte_size(Bin) - byte_size(Cs), Where, Curr0, [btoken(Before, Curr0), After]);
        {<<_:Start/binary, Keep/binary>>, [_], After} when Where =:= all ->
            split(Bin, Needle, byte_size(Bin) - byte_size(After), Where, [], [rev([Keep|Curr0])|Acc])
    end.

-compile({inline, add_non_empty/2}).
add_non_empty(<<>>, L) -> L;
add_non_empty(Token, L) -> [Token|L].

-ifndef(NEED_prefix__2).
-define(NEED_prefix__2, true).
-endif.
-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-ifndef(NEED_rev_1).
-define(NEED_rev_1, true).
-endif.
-ifndef(NEED_bin_search_str_4).
-define(NEED_bin_search_str_4, true).
-endif.
-ifndef(NEED_btoken_2).
-define(NEED_btoken_2, true).
-endif.
-endif.

-ifndef(HAVE_string__take_2).
take(Str, Sep) -> take(Str, Sep, false).
-endif.

-ifndef(HAVE_string__take_3).
take(Str, Sep, Complement) -> take(Str, Sep, Complement, leading).
-endif.

-ifndef(HAVE_string__take_4).
take(Str, [], Complement, Dir) ->
    take_(Str,
          if
              is_binary(Str) -> <<>>;
              true -> []
          end,
          Complement, Dir);
take(Str, Sep, false, leading) -> take_l(Str, Sep, []);
take(Str, Sep, true, leading) -> take_lc(Str, search_pattern(Sep), []);
take(Str, Sep, false, trailing) -> take_t(Str, 0, search_pattern(Sep));
take(Str, Sep, true, trailing) -> take_tc(Str, 0, search_pattern(Sep)).

take_(Str, Empty, false, leading) -> {Empty, Str};
take_(Str, Empty, false, trailing) -> {Str, Empty};
take_(Str, Empty, true, leading) -> {Str, Empty};
take_(Str, Empty, true, trailing) -> {Empty, Str}.

take_l([CP1|[CP2|_] = Cont] = Str, Seps, Acc) when ?ASCII_LIST(CP1, CP2) ->
    case lists:member(CP1, Seps) of
        true -> take_l(Cont, Seps, [CP1|Acc]);
        _false -> {rev(Acc), Str}
    end;
take_l([Bin|Cont0], Seps, Acc) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, Seps) of
        {nomatch, Cont} -> take_l(Cont, Seps, [unicode:characters_to_binary([Bin|cp_prefix(Cont0, Cont)])|Acc]);
        [Bin1|_] = After when is_binary(Bin1) ->
            {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(Bin1)), Acc), After}
    end;
take_l(Str, Seps, Acc) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, Seps) of
                true -> take_l(Cs, Seps, append(rev(C), Acc));
                _false -> {rev(Acc), Str}
            end;
        [] -> {rev(Acc), []}
    end;
take_l(Bin, Seps, Acc) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], Seps) of
        {nomatch, _} -> {btoken(Bin, Acc), <<>>};
        [After] -> {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(After)), Acc), After}
    end.

take_lc([CP1|Cont] = Str0, {GCs, CPs, _} = Seps, Acc) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Str] = unicode_util:gc(Str0),
            case lists:member(GC, GCs) of
                true -> {rev(Acc), Str0};
                _false -> take_lc(Str, Seps, append(rev(GC), Acc))
            end;
        _false -> take_lc(Cont, Seps, append(CP1, Acc))
    end;
take_lc([Bin|Cont0], Seps0, Acc) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, Cont0, Seps) of
        {nomatch, Cont} -> take_lc(Cont, Seps, [unicode:characters_to_binary([Bin|cp_prefix(Cont0, Cont)])|Acc]);
        [Bin1|_] = After when is_binary(Bin1) ->
            {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(Bin1)), Acc), After}
    end;
take_lc(Str, {GCs, _, _} = Seps, Acc) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                true -> {rev(Acc), Str};
                _false -> take_lc(Cs, Seps, append(rev(C), Acc))
            end;
        [] -> {rev(Acc), []}
    end;
take_lc(Bin, Seps, Acc) when is_binary(Bin) ->
    case bin_search(Bin, [], search_compile(Seps)) of
        {nomatch, _} -> {btoken(Bin, Acc), <<>>};
        [After] -> {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(After)), Acc), After}
    end.

take_t([CP1|Cont] = Str0, _, {GCs, CPs, _} = Seps) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Str] = unicode_util:gc(Str0),
            {Head, Tail} = take_t(Str, 0, Seps),
            case lists:member(GC, GCs) andalso is_empty(Head) of
                true -> {Head, append(GC, Tail)};
                _false -> {append(GC, Head), Tail}
            end;
        _false ->
            {Head, Tail} = take_t(Cont, 0, Seps),
            {[CP1|Head], Tail}
    end;
take_t([Bin|Cont0], N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, Cont0, Seps) of
        {nomatch, Cont} ->
            {Head, Tail} = take_t(Cont, 0, Seps),
            {stack(unicode:characters_to_binary([Bin|cp_prefix(Cont0, Cont)]), Head), Tail};
        [SepStart|Cont1] ->
            case bin_search_inv(SepStart, Cont1, GCs) of
                {nomatch, Cont} ->
                    {Head, Tail} = take_t(Cont, 0, Seps),
                    Used = cp_prefix(Cont0, Cont),
                    case is_empty(Head) of
                        true ->
                            {Keep, End} = split_binary(Bin, byte_size(Bin) - byte_size(SepStart)),
                            {Keep, stack(stack(End, Used), Tail)};
                        _false -> {stack(unicode:characters_to_binary([Bin|Used]), Head), Tail}
                    end;
                [NonSep|Cont] when is_binary(NonSep) -> take_t([Bin|Cont], byte_size(Bin) - byte_size(NonSep), Seps)
            end
    end;
take_t(Str, 0, {GCs, _, _} = Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            {Head, Tail} = take_t(Cs1, 0, Seps),
            case lists:member(GC, GCs) andalso is_empty(Head) of
                true -> {Head, append(GC, Tail)};
                _false -> {append(GC, Head), Tail}
            end;
        [] -> {[], []}
    end;
take_t(Bin, N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, [], Seps) of
        {nomatch, _} -> {Bin, <<>>};
        [SepStart] ->
            BinSize = byte_size(Bin),
            case bin_search_inv(SepStart, [], GCs) of
                {nomatch, _} -> split_binary(Bin, BinSize - byte_size(SepStart));
                [NonSep] -> take_t(Bin, BinSize - byte_size(NonSep), Seps)
            end
    end.

take_tc([CP1|[CP2|_] = Cont], _, {GCs, _, _} = Seps) when ?ASCII_LIST(CP1,CP2) ->
    {Head, Tail} = take_tc(Cont, 0, Seps),
    case lists:member(CP1, GCs) andalso is_empty(Head) of
        true -> {Head, append(CP1, Tail)};
        _false -> {append(CP1, Head), Tail}
    end;
take_tc([Bin|Cont0], N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search_inv(Rest, Cont0, GCs) of
        {nomatch, Cont} ->
            {Head, Tail} = take_tc(Cont, 0, Seps0),
            {stack(unicode:characters_to_binary([Bin|cp_prefix(Cont0, Cont)]), Head), Tail};
        [SepStart|Cont1] ->
            Seps = search_compile(Seps0),
            case bin_search(SepStart, Cont1, Seps) of
                {nomatch, Cont} ->
                    {Head, Tail} = take_tc(Cont, 0, Seps),
                    Used = cp_prefix(Cont0, Cont),
                    case is_empty(Head) of
                        true ->
                            {Keep, End} = split_binary(Bin, byte_size(Bin) - byte_size(SepStart)),
                            {Keep, stack(stack(End, Used), Tail)};
                        false -> {stack(unicode:characters_to_binary([Bin|Used]), Head), Tail}
                    end;
                [NonSep|Cont] when is_binary(NonSep) -> take_tc([Bin|Cont], byte_size(Bin) - byte_size(NonSep), Seps)
            end
    end;
take_tc(Str, 0, {GCs, _, _} = Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            {Head, Tail} = take_tc(Cs1, 0, Seps),
            case not lists:member(GC, GCs) andalso is_empty(Head) of
                true -> {Head, append(GC, Tail)};
                _false -> {append(GC, Head), Tail}
            end;
        [] -> {[], []}
    end;
take_tc(Bin, N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search_inv(Rest, [], GCs) of
        {nomatch, _} -> {Bin, <<>>};
        [SepStart] ->
            Seps = search_compile(Seps0),
            BinSize = byte_size(Bin),
            case bin_search(SepStart, [], Seps) of
                {nomatch, _} -> split_binary(Bin, BinSize - byte_size(SepStart));
                [NonSep] -> take_tc(Bin, BinSize - byte_size(NonSep), Seps)
            end
    end.

-ifndef(NEED_search_pattern_1).
-define(NEED_search_pattern_1, true).
-endif.
-ifndef(NEED_rev_1).
-define(NEED_rev_1, true).
-endif.
-ifndef(NEED_bin_search_inv_3).
-define(NEED_bin_search_inv_3, true).
-endif.
-ifndef(NEED_cp_prefix_2).
-define(NEED_cp_prefix_2, true).
-endif.
-ifndef(NEED_btoken_2).
-define(NEED_btoken_2, true).
-endif.
-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-ifndef(NEED_search_compile_1).
-define(NEED_search_compile_1, true).
-endif.
-ifndef(NEED_bin_search_3).
-define(NEED_bin_search_3, true).
-endif.
-ifndef(NEED_stack_2).
-define(NEED_stack_2, true).
-endif.
-endif.

-ifndef(HAVE_string__titlecase_1).
titlecase(CD) when is_list(CD) ->
    case unicode_util:titlecase(CD) of
        [GC|Tail] -> append(GC, Tail);
        Empty -> Empty
    end;
titlecase(CD) when is_binary(CD) ->
    case unicode_util:titlecase(CD) of
        [CPs|Chars] ->
            <<(if
                   is_integer(CPs) -> <<CPs/utf8>>;
                   true -> <<<<CP/utf8>> || CP <- CPs>>
               end)/binary,
              Chars/binary>>;
        [] -> <<>>
    end.

-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-endif.

-ifndef(HAVE_string__to_graphemes_1).
to_graphemes(CD0) ->
    case unicode_util:gc(CD0) of
        [GC|CD] -> [GC|to_graphemes(CD)];
        [] -> [];
        {error, Err} -> error({badarg, Err})
    end.
-endif.

-ifndef(HAVE_string__trim_1).
trim(Str) -> trim(Str, both).
-endif.

-ifndef(HAVE_string__trim_2).
trim(Str, Dir) -> trim(Str, Dir, unicode_util:whitespace()).
-endif.

-ifndef(HAVE_string__trim_3).
trim(Str, _, []) -> Str;
trim(Str, Dir, [Sep]) when is_list(Str), is_integer(Sep), Sep >= 0, Sep < 256 ->
    if
        Dir =:= leading -> trim_ls(Str, Sep);
        Dir =:= trailing -> trim_ts(Str, Sep)
    end;
trim(Str, Dir, Seps) when is_list(Seps) ->
    if
        Dir =:= leading -> trim_l(Str, Seps);
        Dir =:= trailing -> trim_t(Str, 0, search_pattern(Seps));
        Dir =:= both -> trim(trim(Str, leading, Seps), trailing, Seps)
    end.

trim_ls([CP1|[CP2|_] = Cont] = Str, Sep) when ?ASCII_LIST(CP1, CP2) ->
    if
        CP1 =:= Sep -> trim_ls(Cont, Sep);
        true -> Str
    end;
trim_ls(Str, Sep) -> trim_l(Str, [Sep]).

trim_l([CP1|[CP2|_] = Cont] = Str, Sep) when ?ASCII_LIST(CP1, CP2) ->
    case lists:member(CP1, Sep) of
        true -> trim_l(Cont, Sep);
        _false -> Str
    end;
trim_l([Bin|Cont0], Sep) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, Sep) of
        {nomatch, Cont} -> trim_l(Cont, Sep);
        Keep -> Keep
    end;
trim_l(Str, Sep) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, Sep) of
                true -> trim_l(Cs, Sep);
                _false -> Str
            end;
        [] -> []
    end;
trim_l(Bin, Sep) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], Sep) of
        {nomatch, _} -> <<>>;
        [Keep] -> Keep
    end.

trim_ts([Sep], Sep) -> [];
trim_ts([Sep|[CP2|_] = Cs1], Sep) when ?ASCII_LIST(Sep, CP2) ->
    Tail = trim_ts(Cs1, Sep),
    case is_empty(Tail) of
        true -> [];
        _false -> [Sep|Tail]
    end;
trim_ts([CP|Cont], Sep) when is_integer(CP), CP =/= Sep -> [CP|trim_ts(Cont, Sep)];
trim_ts(Str, Sep) -> trim_t(Str, 0, search_pattern([Sep])).

trim_t([CP1|Cont] = Cs0, _, {GCs, CPs, _} = Seps) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Cs1] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true ->
                    Tail = trim_t(Cs1, 0, Seps),
                    case is_empty(Tail) of
                        true -> [];
                        _false -> append(GC, Tail)
                    end;
                _false -> append(GC, trim_t(Cs1, 0, Seps))
            end;
        _false -> [CP1|trim_t(Cont, 0, Seps)]
    end;
trim_t([Bin|Cont0], N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, Cont0, Seps) of
        {nomatch, _} -> stack(Bin, trim_t(Cont0, 0, Seps));
        [SepStart|Cont1] ->
            case bin_search_inv(SepStart, Cont1, GCs) of
                {nomatch, Cont} ->
                    Tail = trim_t(Cont, 0, Seps),
                    case is_empty(Tail) of
                        true -> binary:part(Bin, 0, byte_size(Bin) - byte_size(SepStart));
                        _false -> stack(Bin, stack(cp_prefix(Cont0, Cont), Tail))
                    end;
                [NonSep|Cont] when is_binary(NonSep) -> trim_t([Bin|Cont], byte_size(Bin) - byte_size(NonSep), Seps)
            end
    end;
trim_t(Str, 0, {GCs, _, _} = Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            case lists:member(GC, GCs) of
                true ->
                    Tail = trim_t(Cs1, 0, Seps),
                    case is_empty(Tail) of
                        true -> [];
                        _false -> append(GC, Tail)
                    end;
                _false -> append(GC, trim_t(Cs1, 0, Seps))
            end;
        [] -> []
    end;
trim_t(Bin, N, {GCs, _, _} = Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, [], Seps) of
        {nomatch, _} -> Bin;
        [SepStart] ->
            BinSize = byte_size(Bin),
            case bin_search_inv(SepStart, [], GCs) of
                {nomatch, _} -> binary:part(Bin, 0, BinSize - byte_size(SepStart));
                [NonSep] -> trim_t(Bin, BinSize - byte_size(NonSep), Seps)
            end
    end.

-ifndef(NEED_search_compile_1).
-define(NEED_search_compile_1, true).
-endif.
-ifndef(NEED_search_pattern_1).
-define(NEED_search_pattern_1, true).
-endif.
-ifndef(NEED_bin_search_3).
-define(NEED_bin_search_3, true).
-endif.
-ifndef(NEED_bin_search_inv_3).
-define(NEED_bin_search_inv_3, true).
-endif.
-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-ifndef(NEED_stack_2).
-define(NEED_stack_2, true).
-endif.
-ifndef(NEED_cp_prefix_2).
-define(NEED_cp_prefix_2, true).
-endif.
-endif.

-ifndef(HAVE_string__uppercase_1).
uppercase(CD) when is_list(CD) ->
    try
        uppercase_list(CD, false)
    catch
        throw:unchanged -> CD
    end;
uppercase(<<CP1/utf8, Rest/binary>> = Orig) ->
    try uppercase_bin(CP1, Rest, false) of
        List -> unicode:characters_to_binary(List)
    catch
        throw:unchanged -> Orig
    end;
uppercase(<<>>) -> <<>>;
uppercase(Bin) -> error({badarg, Bin}).

uppercase_list([CP1|[CP2|_] = Cont], _Changed)
  when is_integer(CP1), CP1 >= $a, CP1 =< $z, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1 - $\s|uppercase_list(Cont, true)];
uppercase_list([CP1|[CP2|_] = Cont], Changed)
  when is_integer(CP1), CP1 >= 0, CP1 < 128, is_integer(CP2), CP2 >= 0, CP2 < 256 ->
    [CP1|uppercase_list(Cont, Changed)];
uppercase_list([], true) -> [];
uppercase_list([], false) -> throw(unchanged);
uppercase_list(CPs0, Changed) ->
    case unicode_util:uppercase(CPs0) of
        [Char|CPs] when Char =:= hd(CPs0) -> [Char|uppercase_list(CPs, Changed)];
        [Char|CPs] -> append(Char, uppercase_list(CPs, true));
        [] -> uppercase_list([], Changed)
    end.

uppercase_bin(CP1, <<CP2/utf8, Bin/binary>>, _Changed) when is_integer(CP1), CP1 >= $a, CP1 =< $z, CP2 < 256 ->
    [CP1 - $\s|uppercase_bin(CP2, Bin, true)];
uppercase_bin(CP1, <<CP2/utf8, Bin/binary>>, Changed) when is_integer(CP1), CP1 >= 0, CP1 < 128, CP2 < 256 ->
    [CP1|uppercase_bin(CP2, Bin, Changed)];
uppercase_bin(CP1, Bin, Changed) ->
    case unicode_util:uppercase([CP1|Bin]) of
        [CP1|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [CP1|uppercase_bin(Next, Rest, Changed)];
                [] when Changed -> [CP1];
                [] -> throw(unchanged);
                {error, Err} -> error({badarg, Err})
            end;
        [Char|CPs] ->
            case unicode_util:cp(CPs) of
                [Next|Rest] when is_integer(Next), Next >= 0 -> [Char|uppercase_bin(Next, Rest, true)];
                [] -> [Char];
                {error, Err} -> error({badarg, Err})
            end
    end.

-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-endif.

-ifdef(NEED_search_pattern_1).
search_pattern(P) when tuple_size(P) =:= 3 -> P;
search_pattern(Seps) -> {Seps, search_cp(Seps), undefined}.

search_cp([P|Seps]) ->
    [if
         is_integer(P) -> P;
         true -> hd(unicode_util:cp(P))
     end|search_cp(Seps)];
search_cp([]) -> [].
-endif.

-ifdef(NEED_search_compile_1).
-compile({inline, search_compile/1}).
search_compile({Sep, CPs, undefined}) -> {Sep, CPs, binary:compile_pattern(bin_pattern(CPs))};
search_compile(Compiled) when tuple_size(Compiled) =:= 3 -> Compiled.

bin_pattern([CP|Seps]) -> [<<CP/utf8>>|bin_pattern(Seps)];
bin_pattern([]) -> [].
-endif.

-ifdef(NEED_bin_search_inv_3).
bin_search_inv(<<>>, Cont, _) -> {nomatch, Cont};
bin_search_inv(Bin, Cont, [Sep]) -> bin_search_inv_1(Bin, Cont, Sep);
bin_search_inv(Bin, Cont, Seps) -> bin_search_inv_n(Bin, Cont, Seps).

bin_search_inv_1(<<Sep/utf8, CP2/utf8, BinRest/binary>>, Cont, Sep) when ?ASCII_LIST(Sep, CP2) ->
    bin_search_inv_1(BinRest, Cont, Sep);
bin_search_inv_1(<<CP1/utf8, CP2/utf8, _/binary>> = Bin0, Cont, _Sep) when ?ASCII_LIST(CP1, CP2) -> [Bin0|Cont];
bin_search_inv_1(<<_/utf8, _/binary>> = Bin0, [], Sep) ->
    case unicode_util:gc(Bin0) of
        [Sep|Bin] -> bin_search_inv_1(Bin, [], Sep);
        _ -> [Bin0]
    end;
bin_search_inv_1(<<_/utf8, _/binary>> = Bin0, Cont, Sep) ->
    case unicode_util:gc([Bin0|Cont]) of
        [Sep|[Bin|Cont]] when is_binary(Bin) -> bin_search_inv_1(Bin, Cont, Sep);
        [Sep|Cs] -> {nomatch, Cs};
        _ -> [Bin0|Cont]
    end;
bin_search_inv_1(S, Cont, _Sep) when S =:= <<>>; S =:= [] -> {nomatch, Cont};
bin_search_inv_1(Bin, _, _) -> error({badarg, Bin}).

bin_search_inv_n(<<CP1/utf8, CP2/utf8, BinRest/binary>> = Bin0, Cont, Seps) when ?ASCII_LIST(CP1, CP2) ->
    case lists:member(CP1, Seps) of
        true -> bin_search_inv_n(BinRest, Cont, Seps);
        false -> [Bin0|Cont]
    end;
bin_search_inv_n(<<_/utf8, _/binary>> = Bin0, [], Seps) ->
    [GC|Bin] = unicode_util:gc(Bin0),
    case lists:member(GC, Seps) of
        true -> bin_search_inv_n(Bin, [], Seps);
        false -> [Bin0]
    end;
bin_search_inv_n(<<_/utf8, _/binary>> = Bin0, Cont, Seps) ->
    [GC|Cs0] = unicode_util:gc([Bin0|Cont]),
    case lists:member(GC, Seps) of
        false -> [Bin0|Cont];
        true ->
            case Cs0 of
                [Bin|Cont] when is_binary(Bin) -> bin_search_inv_n(Bin, Cont, Seps);
                _ -> {nomatch, Cs0}
            end
    end;
bin_search_inv_n(S, Cont, _Sep) when S =:= <<>>; S =:= [] -> {nomatch, Cont};
bin_search_inv_n(Bin, _, _) -> error({badarg, Bin}).
-endif.

-ifdef(NEED_lexeme_pick_3).
lexeme_pick([CP|Cs1] = Cs0, {GCs, CPs, _} = Seps, Tkn) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true  ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true -> {rev(Tkn), Cs2};
                false -> lexeme_pick(Cs2, Seps, append(rev(GC), Tkn))
            end;
        false -> lexeme_pick(Cs1, Seps, [CP|Tkn])
    end;
lexeme_pick([Bin|Cont0], Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, Cont0, Seps) of
        {nomatch, _} -> lexeme_pick(Cont0, Seps, [Bin|Tkn]);
        [Left|_Cont] = Cs -> {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(Left)), Tkn), Cs}
    end;
lexeme_pick(Cs0, {GCs, CPs, _} = Seps, Tkn) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [CP|Cs] ->
            case lists:member(CP, CPs) of
                true ->
                    [GC|Cs2] = unicode_util:gc(Cs0),
                    case lists:member(GC, GCs) of
                        true -> {rev(Tkn), Cs2};
                        false -> lexeme_pick(Cs2, Seps, append(rev(GC), Tkn))
                    end;
                false -> lexeme_pick(Cs, Seps, append(CP,Tkn))
            end;
        [] -> {rev(Tkn), []}
    end;
lexeme_pick(Bin, Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, [], Seps) of
        {nomatch, _} -> {btoken(Bin, Tkn), []};
        [Left] -> {btoken(binary:part(Bin, 0, byte_size(Bin) - byte_size(Left)), Tkn), Left}
    end.
-ifndef(NEED_rev_1).
-define(NEED_rev_1, true).
-endif.
-ifndef(NEED_append_2).
-define(NEED_append_2, true).
-endif.
-ifndef(NEED_btoken_2).
-define(NEED_btoken_2, true).
-endif.
-ifndef(NEED_bin_search_3).
-define(NEED_bin_search_3, true).
-endif.
-endif.

-ifdef(NEED_btoken_2).
-compile({inline, btoken/2}).
btoken(Token, []) -> Token;
btoken(BinPart, [C]) when is_integer(C) -> <<C/utf8, BinPart/binary>>;
btoken(<<>>, Tkn) -> lists:reverse(Tkn);
btoken(BinPart, Cs) -> [lists:reverse(Cs), BinPart].
-endif.

-ifdef(NEED_rev_1).
-compile({inline, rev/1}).
rev([B]) when is_binary(B) -> B;
rev(L) when is_list(L) -> lists:reverse(L);
rev(C) when is_integer(C) -> C.
-endif.

-ifdef(NEED_append_2).
-compile({inline, append/2}).
append(Char, <<>>) when is_integer(Char) -> [Char];
append(Char, <<>>) when is_list(Char) -> Char;
append(Char, Bin) when is_binary(Bin) -> [Char, Bin];
append(Char, Str) when is_integer(Char) -> [Char|Str];
append(GC, Str) when is_list(GC) -> GC ++ Str.
-endif.

-ifdef(NEED_bin_search_3).
bin_search(Bin, Cont, {Seps, _, BP}) -> bin_search_loop(Bin, 0, BP, Cont, Seps).

bin_search_loop(Bin0, Start, _, Cont, _Seps) when byte_size(Bin0) =< Start; Start < 0 -> {nomatch, Cont};
bin_search_loop(Bin0, Start, BinSeps, Cont, Seps) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, BinSeps) of
        nomatch -> {nomatch, Cont};
        {Where, _CL} when Cont =:= [] ->
            <<_:Where/binary, Cont1/binary>> = Bin,
            [GC|Cont2] = unicode_util:gc(Cont1),
            case lists:member(GC, Seps) of
                false when Cont2 =:= [] -> {nomatch, []};
                false -> bin_search_loop(Bin0, byte_size(Bin0) - byte_size(Cont2), BinSeps, Cont, Seps);
                true -> [Cont1]
            end;
        {Where, _CL} ->
            <<_:Where/binary, Cont0/binary>> = Bin,
            Cont1 = [Cont0|Cont],
            [GC|Cont2] = unicode_util:gc(Cont1),
            case lists:member(GC, Seps) of
                false ->
                    case Cont2 of
                        [BinR|Cont] when is_binary(BinR) ->
                            bin_search_loop(Bin0, byte_size(Bin0) - byte_size(BinR), BinSeps, Cont, Seps);
                        _ -> {nomatch, Cont2}
                    end;
                true -> Cont1
            end
    end.
-endif.

-ifdef(NEED_cp_prefix_2).
cp_prefix(Orig, Cont) ->
    case unicode_util:cp(Cont) of
        [] -> Orig;
        [Cp|Rest] -> cp_prefix(Orig, Cp, Rest)
    end.

cp_prefix(Orig, Until, Cont) ->
    [CP|Rest] = unicode_util:cp(Orig),
    case CP =:= Until andalso equal(Rest, Cont) of
        true -> [];
        _false -> [CP|cp_prefix(Rest, Until, Cont)]
    end.
-endif.

-ifdef(NEED_slice_l0_2).
slice_l0(<<CP1/utf8, Bin/binary>>, N) when N > 0 -> slice_lb(Bin, CP1, N);
slice_l0(L, N) -> slice_l(L, N).

slice_l(Cont, 0) -> Cont;
slice_l(CD, N) when is_integer(N), N > 0 ->
    case CD of
        [CP1|[CP2|_] = Cont] when ?ASCII_LIST(CP1, CP2) -> slice_l(Cont, N - 1);
        _ ->
            case unicode_util:gc(CD) of
                [_|Cont] -> slice_l(Cont, N - 1);
                [] -> [];
                {error, Err} -> error({badarg, Err})
            end
    end.

slice_lb(Bin, CP1, 1) -> tl(unicode_util:gc([CP1|Bin]));
slice_lb(Bin, CP1, N) when is_integer(N), N > 1 ->
    case Bin of
        <<CP2/utf8, Bin1/binary>> when ?ASCII_LIST(CP1, CP2) -> slice_lb(Bin1, CP2, N - 1);
        _ ->
            case unicode_util:cp(tl(unicode_util:gc([CP1|Bin]))) of
                [CP2|Cont] -> slice_lb(Cont, CP2, N - 1);
                [] -> <<>>;
                {error, Err} -> error({badarg, Err})
            end
    end.
-endif.

-ifdef(NEED_bin_search_str_4).
bin_search_str(Bin0, Start, [], SearchCPs) ->
    bin_search_str_(Bin0, Start, binary:compile_pattern(unicode:characters_to_binary(SearchCPs)), SearchCPs);
bin_search_str(Bin0, Start, Cont, [CP|_] = SearchCPs) ->
    bin_search_str_(Bin0, Start, Cont, binary:compile_pattern(<<CP/utf8>>), SearchCPs).

bin_search_str_(Bin0, Start, First, SearchCPs) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, First) of
        nomatch -> {nomatch, byte_size(Bin0), []};
        {Where, _} ->
            {Keep, Cs0} = split_binary(Bin0, Start + Where),
            case prefix_(Cs0, SearchCPs) of
                nomatch ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    bin_search_str_(Bin0, byte_size(Bin0) - byte_size(Cs), First, SearchCPs);
                [] -> {Keep, [Cs0], <<>>};
                Rest -> {Keep, [Cs0], Rest}
            end
    end.

bin_search_str_(Bin0, Start, Cont, First, SearchCPs) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, First) of
        nomatch -> {nomatch, byte_size(Bin0), Cont};
        {Where, _} when is_integer(Where) ->
            {Keep, Cs0} = split_binary(Bin0, Start + Where),
            case prefix_(stack(Cs0, Cont), SearchCPs) of
                nomatch ->
                    case unicode_util:gc(Cs0) of
                        [_|Cs] when is_binary(Cs) ->
                            bin_search_str_(Bin0, byte_size(Bin0) - byte_size(Cs), Cont, First, SearchCPs);
                        [_|_] = Cs -> {nomatch, Where, stack(Cs, Cont)}
                    end;
                Rest ->
                    {Keep, [Cs0|Cont],
                     if
                         Rest =:= [] -> <<>>;
                         true -> Rest
                     end}
            end
    end.

-ifndef(NEED_prefix__2).
-define(NEED_prefix__2, true).
-endif.
-ifndef(NEED_stack_2).
-define(NEED_stack_2, true).
-endif.
-endif.

-ifdef(NEED_prefix__2).
prefix_(Cs0, [GC]) ->
    case unicode_util:gc(Cs0) of
        [GC|Cs] -> Cs;
        _ -> nomatch
    end;
prefix_([CP|Cs], [Pre|PreR]) when is_integer(CP) ->
    if
        CP =:= Pre -> prefix_(Cs, PreR);
        true -> nomatch
    end;
prefix_(<<CP/utf8, Cs/binary>>, [CP|PreR]) -> prefix_(Cs, PreR);
prefix_(<<_/utf8, _/binary>>, [_|_]) -> nomatch;
prefix_(Cs0, [Pre|PreR]) ->
    case unicode_util:cp(Cs0) of
        [Pre|Cs] ->  prefix_(Cs, PreR);
        _ -> nomatch
    end.
-endif.

-ifdef(NEED_stack_2).
-compile({inline, stack/2}).
stack(S, St) when S =:= <<>>; S =:= [] -> St;
stack(Bin, St) -> [Bin|St].
-endif.

-ifndef(HAVE_string__list_to_integer_1).
to_integer(S) when is_binary(S) -> to_integer(S, fun binary_to_integer/1, binary);
to_integer(S) when is_list(S) -> to_integer(S, fun list_to_integer/1, list);
to_integer(S) -> error(badarg, [S]).

to_integer(S, F, C) -> to_number(S, <<"^(?<N>[-+]?\\d*)(?<T>.*)">>, C, F, no_integer).

-ifndef(NEED_to_number_5).
-define(NEED_to_number_5, true).
-endif.
-endif.

-ifndef(HAVE_string__list_to_float_1).
to_float(S) when is_binary(S) -> to_float(S, fun binary_to_float/1, binary);
to_float(S) when is_list(S) -> to_float(S, fun list_to_float/1, list);
to_float(S) -> error(badarg, [S]).

to_float(S, F, C) -> to_number(S, <<"^(?<N>[-+]?\\d+\.\\d+([Ee][-+]?\\d+))?(?<T>.*)">>, C, F, no_float).

-ifndef(NEED_to_number_5).
-define(NEED_to_number_5, true).
-endif.
-endif.

-ifdef(NEED_to_number_5).
to_number(S, P, C, F, E) ->
    try re:run(S, P, [{capture, all_names, C}]) of
        {match, [N, T]} -> {F(N), T};
        nomatch -> {error, E}
    catch
        error:badarg -> {error, badarg}
    end.
-endif.
