%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%% A string library that works on grapheme clusters, with the exception
%% of codepoints of class 'prepend' and non modern (or decomposed) Hangul.
%% If these codepoints appear, functions like 'find/2' may return a string
%% which starts inside a grapheme cluster.
%% These exceptions are made because the codepoints classes are
%% seldom used and require that we are able look at previous codepoints in
%% the stream and is thus hard to implement effectively.
%%
%% GC (grapheme cluster) implies that the length of string 'ß↑e̊' is 3 though
%% it is represented by the codepoints [223,8593,101,778] or the
%% utf8 binary <<195,159,226,134,145,101,204,138>>
%%
%% And that searching for strings or graphemes finds the correct positions:
%%
%% find("eeeee̊eee", "e̊") -> "e̊ee".:
%% find("1£4e̊abcdef", "e") -> "ef"
%%
%% Most functions expect all input to be normalized to one form,
%% see unicode:characters_to_nfc and unicode:characters_to_nfd functions.
%% When appending strings no checking is done to verify that the
%% result is valid unicode strings.
%%
%% The functions may crash for invalid utf-8 input.
%%
%% Return value should be kept consistent when return type is
%% unicode:chardata() i.e. binary input => binary output,
%% list input => list output mixed input => mixed output
%%
-module(otpbp_string).

-ifndef(HAVE_string__length_1).
-compile([{parse_transform, otpbp_pt}]).

-export([is_empty/1, length/1, to_graphemes/1,
         reverse/1,
         equal/2, equal/3, equal/4,
         slice/2, slice/3,
         pad/2, pad/3, pad/4, trim/1, trim/2, trim/3, chomp/1,
         take/2, take/3, take/4,
         lexemes/2, nth_lexeme/3,
         uppercase/1, lowercase/1, titlecase/1,casefold/1,
         prefix/2,
         split/2,split/3,replace/3,replace/4,
         find/2,find/3,
         next_codepoint/1, next_grapheme/1
        ]).

-compile({no_auto_import,[length/1]}).
-compile({inline, [btoken/2, rev/1, append/2, stack/2, search_compile/1]}).
-define(ASCII_LIST(CP1,CP2), CP1 < 256, CP2 < 256, CP1 =/= $\r).

-export_type([grapheme_cluster/0]).

-type grapheme_cluster() :: char() | [char()].
-type direction() :: 'leading' | 'trailing'.

-dialyzer({no_improper_lists, [stack/2, length_b/3]}).

%% Check if string is the empty string
-spec is_empty(String::unicode:chardata()) -> boolean().
is_empty([]) -> true;
is_empty(<<>>) -> true;
is_empty([L|R]) -> is_empty(L) andalso is_empty(R);
is_empty(_) -> false.

%% Count the number of grapheme clusters in chardata
-spec length(String::unicode:chardata()) -> non_neg_integer().
length(<<CP1/utf8, Bin/binary>>) ->
    length_b(Bin, CP1, 0);
length(CD) ->
    length_1(CD, 0).

%% Convert a string to a list of grapheme clusters
-spec to_graphemes(String::unicode:chardata()) -> [grapheme_cluster()].
to_graphemes(CD0) ->
    case unicode_util:gc(CD0) of
        [GC|CD] -> [GC|to_graphemes(CD)];
        [] -> []
    end.

%% Compare two strings return boolean, assumes that the input are
%% normalized to same form, see unicode:characters_to_nfX_xxx(..)
-spec equal(A, B) -> boolean() when
      A::unicode:chardata(),
      B::unicode:chardata().
equal(A,B) when is_binary(A), is_binary(B) ->
    A =:= B;
equal(A,B) ->
    equal_1(A,B).

%% Compare two strings return boolean, assumes that the input are
%% normalized to same form, see unicode:characters_to_nfX_xxx(..)
%% does casefold on the fly
-spec equal(A, B, IgnoreCase) -> boolean() when
      A::unicode:chardata(),
      B::unicode:chardata(),
      IgnoreCase :: boolean().
equal(A, B, false) ->
    equal(A,B);
equal(A, B, true) ->
    equal_nocase(A,B).

%% Compare two strings return boolean
%% if specified does casefold and normalization on the fly
-spec equal(A, B, IgnoreCase, Norm) -> boolean() when
      A :: unicode:chardata(),
      B :: unicode:chardata(),
      IgnoreCase :: boolean(),
      Norm :: 'none' | 'nfc' | 'nfd' | 'nfkc' | 'nfkd'.
equal(A, B, Case, none) ->
    equal(A,B,Case);
equal(A, B, false, Norm) ->
    equal_norm(A, B, Norm);
equal(A, B, true, Norm) ->
    equal_norm_nocase(A, B, Norm).

%% Reverse grapheme clusters
-spec reverse(String::unicode:chardata()) -> [grapheme_cluster()].
reverse(<<CP1/utf8, Rest/binary>>) ->
    reverse_b(Rest, CP1, []);
reverse(CD) ->
    reverse_1(CD, []).

%% Slice a string and return rest of string
%% Note: counts grapheme_clusters
-spec slice(String, Start) -> Slice when
      String::unicode:chardata(),
      Start :: non_neg_integer(),
      Slice :: unicode:chardata().
slice(CD, N) when is_integer(N), N >= 0 ->
    case slice_l0(CD, N) of
        [] when is_binary(CD) -> <<>>;
        Res -> Res
    end.

-spec slice(String, Start, Length) -> Slice when
      String::unicode:chardata(),
      Start :: non_neg_integer(),
      Length :: 'infinity' | non_neg_integer(),
      Slice :: unicode:chardata().
slice(CD, N, Length)
  when is_integer(N), N >= 0, is_integer(Length), Length > 0 ->
    case slice_l0(CD, N) of
        [] when is_binary(CD) -> <<>>;
        L -> slice_trail(L, Length)
    end;
slice(CD, N, infinity) ->
    case slice_l0(CD, N) of
        [] when is_binary(CD) -> <<>>;
        Res -> Res
    end;
slice(CD, _, 0) ->
    case is_binary(CD) of
        true  -> <<>>;
        false -> []
    end.

%% Pad a string to desired length
-spec pad(String, Length) -> unicode:charlist() when
      String ::unicode:chardata(),
      Length :: integer().
pad(CD, Length) ->
    pad(CD, Length, trailing, $\s).

-spec pad(String, Length, Dir) -> unicode:charlist() when
      String ::unicode:chardata(),
      Length :: integer(),
      Dir :: direction() | 'both'.
pad(CD, Length, Dir) ->
    pad(CD, Length, Dir, $\s).

-spec pad(String, Length, Dir, Char) -> unicode:charlist() when
      String ::unicode:chardata(),
      Length :: integer(),
      Dir :: direction() | 'both',
      Char :: grapheme_cluster().
pad(CD, Length, leading, Char) when is_integer(Length) ->
    Len = length(CD),
    [lists:duplicate(max(0, Length-Len), Char), CD];
pad(CD, Length, trailing, Char) when is_integer(Length) ->
    Len = length(CD),
    [CD|lists:duplicate(max(0, Length-Len), Char)];
pad(CD, Length, both, Char) when is_integer(Length) ->
    Len = length(CD),
    Size = max(0, Length-Len),
    Pre = lists:duplicate(Size div 2, Char),
    Post = case Size rem 2 of
               1 -> [Char];
               _ -> []
           end,
    [Pre, CD, Pre|Post].

%%  Strip characters from whitespace or Separator in Direction
-spec trim(String) -> unicode:chardata() when
      String :: unicode:chardata().
trim(Str) ->
    trim(Str, both, unicode_util:whitespace()).

-spec trim(String, Dir) -> unicode:chardata() when
      String :: unicode:chardata(),
      Dir :: direction() | 'both'.
trim(Str, Dir) ->
    trim(Str, Dir, unicode_util:whitespace()).

-spec trim(String, Dir, Characters) -> unicode:chardata() when
      String :: unicode:chardata(),
      Dir :: direction() | 'both',
      Characters :: [grapheme_cluster()].
trim(Str, _, []) -> Str;
trim(Str, leading, [Sep]) when is_list(Str), Sep < 256 ->
    trim_ls(Str, Sep);
trim(Str, leading, Sep) when is_list(Sep) ->
    trim_l(Str, Sep);
trim(Str, trailing, [Sep]) when is_list(Str), Sep < 256 ->
    trim_ts(Str, Sep);
trim(Str, trailing, Seps0) when is_list(Seps0) ->
    Seps = search_pattern(Seps0),
    trim_t(Str, 0, Seps);
trim(Str, both, Sep) when is_list(Sep) ->
    trim(trim(Str,leading,Sep), trailing, Sep).

%% Delete trailing newlines or \r\n
-spec chomp(String::unicode:chardata()) -> unicode:chardata().
chomp(Str) ->
    trim(Str, trailing, [[$\r,$\n],$\n]).

%% Split String into two parts where the leading part consists of Characters
-spec take(String, Characters) -> {Leading, Trailing} when
      String::unicode:chardata(),
      Characters::[grapheme_cluster()],
      Leading::unicode:chardata(),
      Trailing::unicode:chardata().
take(Str, Sep) ->
    take(Str, Sep, false, leading).
-spec take(String, Characters, Complement) -> {Leading, Trailing} when
      String::unicode:chardata(),
      Characters::[grapheme_cluster()],
      Complement::boolean(),
      Leading::unicode:chardata(),
      Trailing::unicode:chardata().
take(Str, Sep, Complement) ->
    take(Str, Sep, Complement, leading).
-spec take(String, Characters, Complement, Dir) -> {Leading, Trailing} when
      String::unicode:chardata(),
      Characters::[grapheme_cluster()],
      Complement::boolean(),
      Dir::direction(),
      Leading::unicode:chardata(),
      Trailing::unicode:chardata().
take(Str, [], Complement, Dir) ->
    Empty = case is_binary(Str) of true -> <<>>; false -> [] end,
    case {Complement,Dir} of
        {false, leading} -> {Empty, Str};
        {false, trailing} -> {Str, Empty};
        {true,  leading} -> {Str, Empty};
        {true,  trailing} -> {Empty, Str}
    end;
take(Str, Sep, false, leading) ->
    take_l(Str, Sep, []);
take(Str, Sep0, true, leading) ->
    Sep = search_pattern(Sep0),
    take_lc(Str, Sep, []);
take(Str, Sep0, false, trailing) ->
    Sep = search_pattern(Sep0),
    take_t(Str, 0, Sep);
take(Str, Sep0, true, trailing) ->
    Sep = search_pattern(Sep0),
    take_tc(Str, 0, Sep).

%% Uppercase all chars in Str
-spec uppercase(String::unicode:chardata()) -> unicode:chardata().
uppercase(CD) when is_list(CD) ->
    uppercase_list(CD);
uppercase(CD) when is_binary(CD) ->
    uppercase_bin(CD,<<>>).

%% Lowercase all chars in Str
-spec lowercase(String::unicode:chardata()) -> unicode:chardata().
lowercase(CD) when is_list(CD) ->
    lowercase_list(CD);
lowercase(CD) when is_binary(CD) ->
    lowercase_bin(CD,<<>>).

%% Make a titlecase of the first char in Str
-spec titlecase(String::unicode:chardata()) -> unicode:chardata().
titlecase(CD) when is_list(CD) ->
    case unicode_util:titlecase(CD) of
        [GC|Tail] -> append(GC,Tail);
        Empty -> Empty
    end;
titlecase(CD) when is_binary(CD) ->
    case unicode_util:titlecase(CD) of
        [CP|Chars] when is_integer(CP) -> <<CP/utf8,Chars/binary>>;
        [CPs|Chars] ->
            << << <<CP/utf8>> || CP <- CPs>>/binary, Chars/binary>>;
        [] -> <<>>
    end.

%% Make a comparable string of the Str should be used for equality tests only
-spec casefold(String::unicode:chardata()) -> unicode:chardata().
casefold(CD) when is_list(CD) ->
    casefold_list(CD);
casefold(CD) when is_binary(CD) ->
    casefold_bin(CD,<<>>).

%% Return the remaining string with prefix removed or else nomatch
-spec prefix(String::unicode:chardata(), Prefix::unicode:chardata()) ->
                    'nomatch' | unicode:chardata().
prefix(Str, []) -> Str;
prefix(Str, Prefix0) ->
    Prefix = unicode:characters_to_list(Prefix0),
    case prefix_1(Str, Prefix) of
        [] when is_binary(Str) -> <<>>;
        Res -> Res
    end.

%% split String with the first occurrence of SearchPattern, return list of splits
-spec split(String, SearchPattern) -> [unicode:chardata()] when
      String :: unicode:chardata(),
      SearchPattern :: unicode:chardata().
split(String, SearchPattern) ->
    split(String, SearchPattern, leading).

%% split String with SearchPattern, return list of splits
-spec split(String, SearchPattern, Where) -> [unicode:chardata()] when
      String :: unicode:chardata(),
      SearchPattern :: unicode:chardata(),
      Where :: direction() | 'all'.
split(String, SearchPattern, Where) ->
    case is_empty(SearchPattern) of
        true -> [String];
        false ->
            SearchPatternCPs = unicode:characters_to_list(SearchPattern),
            case split_1(String, SearchPatternCPs, 0, Where, [], []) of
                {_Curr, []} -> [String];
                {_Curr, Acc} when Where =:= trailing -> Acc;
                {Curr, Acc} when Where =:= all -> lists:reverse([Curr|Acc]);
                Acc when is_list(Acc) -> Acc
            end
    end.

%% Replace the first SearchPattern in String with Replacement
-spec replace(String, SearchPattern, Replacement) ->
                     [unicode:chardata()] when
      String :: unicode:chardata(),
      SearchPattern :: unicode:chardata(),
      Replacement :: unicode:chardata().
replace(String, SearchPattern, Replacement) ->
    lists:join(Replacement, split(String, SearchPattern)).

%% Replace Where SearchPattern in String with Replacement
-spec replace(String, SearchPattern, Replacement, Where) ->
                     [unicode:chardata()] when
      String :: unicode:chardata(),
      SearchPattern :: unicode:chardata(),
      Replacement :: unicode:chardata(),
      Where :: direction() | 'all'.
replace(String, SearchPattern, Replacement, Where) ->
    lists:join(Replacement, split(String, SearchPattern, Where)).

%% Split Str into a list of chardata separated by one of the grapheme
%% clusters in Seps
-spec lexemes(String::unicode:chardata(),
              SeparatorList::[grapheme_cluster()]) ->
                     [unicode:chardata()].
lexemes([], _) -> [];
lexemes(Str, []) -> [Str];
lexemes(Str, Seps0) when is_list(Seps0) ->
    Seps = search_pattern(Seps0),
    lexemes_m(Str, Seps, []).

-spec nth_lexeme(String, N, SeparatorList) -> unicode:chardata() when
      String::unicode:chardata(),
      N::non_neg_integer(),
      SeparatorList::[grapheme_cluster()].

nth_lexeme(Str, 1, []) -> Str;
nth_lexeme(Str, N, Seps0) when is_list(Seps0), is_integer(N), N > 0 ->
    Seps = search_pattern(Seps0),
    nth_lexeme_m(Str, Seps, N).

%% find first SearchPattern in String return rest of string
-spec find(String, SearchPattern) -> unicode:chardata() | 'nomatch' when
      String::unicode:chardata(),
      SearchPattern::unicode:chardata().
find(String, SearchPattern) ->
    find(String, SearchPattern, leading).

%% find SearchPattern in String (search in Dir direction) return rest of string
-spec find(String, SearchPattern, Dir) -> unicode:chardata() | 'nomatch' when
      String::unicode:chardata(),
      SearchPattern::unicode:chardata(),
      Dir::direction().
find(String, "", _) -> String;
find(String, <<>>, _) -> String;
find(String, SearchPattern, leading) ->
    find_l(String, unicode:characters_to_list(SearchPattern));
find(String, SearchPattern, trailing) ->
    find_r(String, unicode:characters_to_list(SearchPattern), nomatch).

%% Fetch first grapheme cluster and return rest in tail
-spec next_grapheme(String::unicode:chardata()) ->
                           maybe_improper_list(grapheme_cluster(),unicode:chardata()) |
                           {error,unicode:chardata()}.
next_grapheme(CD) -> unicode_util:gc(CD).

%% Fetch first codepoint and return rest in tail
-spec next_codepoint(String::unicode:chardata()) ->
                            maybe_improper_list(char(),unicode:chardata()) |
                            {error,unicode:chardata()}.
next_codepoint(CD) -> unicode_util:cp(CD).

%% Internals

length_1([CP1|[CP2|_]=Cont], N) when ?ASCII_LIST(CP1,CP2) ->
    length_1(Cont, N+1);
length_1(Str, N) ->
    case unicode_util:gc(Str) of
        [] -> N;
        [_|Rest] -> length_1(Rest, N+1)
    end.

length_b(<<CP2/utf8, Rest/binary>>, CP1, N)
  when ?ASCII_LIST(CP1,CP2) ->
    length_b(Rest, CP2, N+1);
length_b(Bin0, CP1, N) ->
    [_|Bin1] = unicode_util:gc([CP1|Bin0]),
    case unicode_util:cp(Bin1) of
        [] -> N+1;
        [CP3|Bin] -> length_b(Bin, CP3, N+1)
    end.

equal_1([A|AR], [B|BR]) when is_integer(A), is_integer(B) ->
    A =:= B andalso equal_1(AR, BR);
equal_1([], BR) -> is_empty(BR);
equal_1(A0,B0) ->
    case {unicode_util:cp(A0), unicode_util:cp(B0)} of
        {[CP|A],[CP|B]} -> equal_1(A,B);
        {[], []} -> true;
        {L1,L2} when is_list(L1), is_list(L2) -> false
    end.

equal_nocase(A, A) -> true;
equal_nocase(A0, B0) ->
    case {unicode_util:cp(unicode_util:casefold(A0)),
          unicode_util:cp(unicode_util:casefold(B0))} of
        {[CP|A],[CP|B]} -> equal_nocase(A,B);
        {[], []} -> true;
        {L1,L2} when is_list(L1), is_list(L2) -> false
    end.

equal_norm(A, A, _Norm) -> true;
equal_norm(A0, B0, Norm) ->
    case {unicode_util:cp(unicode_util:Norm(A0)),
          unicode_util:cp(unicode_util:Norm(B0))} of
        {[CP|A],[CP|B]} -> equal_norm(A,B, Norm);
        {[], []} -> true;
        {L1,L2} when is_list(L1), is_list(L2) -> false
    end.

equal_norm_nocase(A, A, _Norm) -> true;
equal_norm_nocase(A0, B0, Norm) ->
    case {unicode_util:cp(unicode_util:casefold(unicode_util:Norm(A0))),
          unicode_util:cp(unicode_util:casefold(unicode_util:Norm(B0)))} of
        {[CP|A],[CP|B]} -> equal_norm_nocase(A,B, Norm);
        {[], []} -> true;
        {L1,L2} when is_list(L1), is_list(L2) -> false
    end.

reverse_1([CP1|[CP2|_]=Cont], Acc) when ?ASCII_LIST(CP1,CP2) ->
    reverse_1(Cont, [CP1|Acc]);
reverse_1(CD, Acc) ->
    case unicode_util:gc(CD) of
        [GC|Rest] -> reverse_1(Rest, [GC|Acc]);
        [] -> Acc
    end.

reverse_b(<<CP2/utf8, Rest/binary>>, CP1, Acc)
  when ?ASCII_LIST(CP1,CP2) ->
    reverse_b(Rest, CP2,  [CP1|Acc]);
reverse_b(Bin0, CP1, Acc) ->
    [GC|Bin1] = unicode_util:gc([CP1|Bin0]),
    case unicode_util:cp(Bin1) of
        [] -> [GC|Acc];
        [CP3|Bin] -> reverse_b(Bin, CP3, [GC|Acc])
    end.

slice_l0(<<CP1/utf8, Bin/binary>>, N) when N > 0 ->
    slice_lb(Bin, CP1, N);
slice_l0(L, N) ->
    slice_l(L, N).

slice_l([CP1|[CP2|_]=Cont], N) when ?ASCII_LIST(CP1,CP2),N > 0 ->
    slice_l(Cont, N-1);
slice_l(CD, N) when N > 0 ->
    case unicode_util:gc(CD) of
        [_|Cont] -> slice_l(Cont, N-1);
        [] -> []
    end;
slice_l(Cont, 0) ->
    Cont.

slice_lb(<<CP2/utf8, Bin/binary>>, CP1, N) when ?ASCII_LIST(CP1,CP2), N > 1 ->
    slice_lb(Bin, CP2, N-1);
slice_lb(Bin, CP1, N) ->
    [_|Rest] = unicode_util:gc([CP1|Bin]),
    if N > 1 ->
            case unicode_util:cp(Rest) of
                [CP2|Cont] -> slice_lb(Cont, CP2, N-1);
                [] -> <<>>
            end;
       N =:= 1 ->
            Rest
    end.

slice_trail(Orig, N) when is_binary(Orig) ->
    case Orig of
        <<CP1/utf8, Bin/binary>> when N > 0 ->
            Length = slice_bin(Bin, CP1, N),
            Sz = byte_size(Orig) - Length,
            <<Keep:Sz/binary, _/binary>> = Orig,
            Keep;
        _ -> <<>>
    end;
slice_trail(CD, N) when is_list(CD) ->
    slice_list(CD, N).

slice_list([CP1|[CP2|_]=Cont], N) when ?ASCII_LIST(CP1,CP2),N > 0 ->
    [CP1|slice_list(Cont, N-1)];
slice_list(CD, N) when N > 0 ->
    case unicode_util:gc(CD) of
        [GC|Cont] -> append(GC, slice_list(Cont, N-1));
        [] -> []
    end;
slice_list(_, 0) ->
    [].

slice_bin(<<CP2/utf8, Bin/binary>>, CP1, N) when ?ASCII_LIST(CP1,CP2), N > 0 ->
    slice_bin(Bin, CP2, N-1);
slice_bin(CD, CP1, N) when N > 0 ->
    [_|Bin] = unicode_util:gc([CP1|CD]),
    case unicode_util:cp(Bin) of
        [CP2|Cont] -> slice_bin(Cont, CP2, N-1);
        [] -> 0
    end;
slice_bin(CD, CP1, 0) ->
    byte_size(CD)+byte_size(<<CP1/utf8>>).

uppercase_list(CPs0) ->
    case unicode_util:uppercase(CPs0) of
        [Char|CPs] -> append(Char,uppercase_list(CPs));
        [] -> []
    end.

uppercase_bin(CPs0, Acc) ->
    case unicode_util:uppercase(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            uppercase_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            uppercase_bin(CPs, <<Acc/binary,
                                 << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.

lowercase_list(CPs0) ->
    case unicode_util:lowercase(CPs0) of
        [Char|CPs] -> append(Char,lowercase_list(CPs));
        [] -> []
    end.

lowercase_bin(CPs0, Acc) ->
    case unicode_util:lowercase(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            lowercase_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            lowercase_bin(CPs, <<Acc/binary,
                                 << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.

casefold_list(CPs0) ->
    case unicode_util:casefold(CPs0) of
        [Char|CPs] -> append(Char, casefold_list(CPs));
        [] -> []
    end.

casefold_bin(CPs0, Acc) ->
    case unicode_util:casefold(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            casefold_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            casefold_bin(CPs, <<Acc/binary,
                                << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.

%% Fast path for ascii searching for one character in lists
trim_ls([CP1|[CP2|_]=Cont]=Str, Sep)
  when ?ASCII_LIST(CP1,CP2) ->
    case Sep of
        CP1 -> trim_ls(Cont, Sep);
        _ -> Str
    end;
trim_ls(Str, Sep) ->
    trim_l(Str, [Sep]).

trim_l([CP1|[CP2|_]=Cont]=Str, Sep)
  when ?ASCII_LIST(CP1,CP2) ->
    case lists:member(CP1, Sep) of
        true -> trim_l(Cont, Sep);
        false -> Str
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
                false -> Str
            end;
        [] -> []
    end;
trim_l(Bin, Sep) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], Sep) of
        {nomatch,_} -> <<>>;
        [Keep] -> Keep
    end.

%% Fast path for ascii searching for one character in lists
trim_ts([Sep|Cs1]=Str, Sep) ->
    case Cs1 of
        [] -> [];
        [CP2|_] when ?ASCII_LIST(Sep,CP2) ->
            Tail = trim_ts(Cs1, Sep),
            case is_empty(Tail) of
                true -> [];
                false -> [Sep|Tail]
            end;
        _ ->
            trim_t(Str, 0, search_pattern([Sep]))
    end;
trim_ts([CP|Cont],Sep) when is_integer(CP) ->
    [CP|trim_ts(Cont, Sep)];
trim_ts(Str, Sep) ->
    trim_t(Str, 0, search_pattern([Sep])).

trim_t([CP1|Cont]=Cs0, _, {GCs,CPs,_}=Seps) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Cs1] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true ->
                    Tail = trim_t(Cs1, 0, Seps),
                    case is_empty(Tail) of
                        true -> [];
                        false -> append(GC,Tail)
                    end;
                false ->
                    append(GC,trim_t(Cs1, 0, Seps))
            end;
        false ->
            [CP1|trim_t(Cont, 0, Seps)]
    end;
trim_t([Bin|Cont0], N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, Cont0, Seps) of
        {nomatch,_} ->
            stack(Bin, trim_t(Cont0, 0, Seps));
        [SepStart|Cont1] ->
            case bin_search_inv(SepStart, Cont1, GCs) of
                {nomatch, Cont} ->
                    Tail = trim_t(Cont, 0, Seps),
                    case is_empty(Tail) of
                        true ->
                            KeepSz = byte_size(Bin) - byte_size(SepStart),
                            <<Keep:KeepSz/binary, _/binary>> = Bin,
                            Keep;
                        false ->
                            Used = cp_prefix(Cont0, Cont),
                            stack(Bin, stack(Used, Tail))
                    end;
                [NonSep|Cont] when is_binary(NonSep) ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    trim_t([Bin|Cont], KeepSz, Seps)
            end
    end;
trim_t(Str, 0, {GCs,_,_}=Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            case lists:member(GC, GCs) of
                true ->
                    Tail = trim_t(Cs1, 0, Seps),
                    case is_empty(Tail) of
                        true -> [];
                        false -> append(GC,Tail)
                    end;
                false ->
                    append(GC,trim_t(Cs1, 0, Seps))
            end;
        [] -> []
    end;
trim_t(Bin, N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, [], Seps) of
        {nomatch,_} -> Bin;
        [SepStart] ->
            case bin_search_inv(SepStart, [], GCs) of
                {nomatch,_} ->
                    KeepSz = byte_size(Bin) - byte_size(SepStart),
                    <<Keep:KeepSz/binary, _/binary>> = Bin,
                    Keep;
                [NonSep] ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    trim_t(Bin, KeepSz, Seps)
            end
    end.


take_l([CP1|[CP2|_]=Cont]=Str, Seps, Acc)
  when ?ASCII_LIST(CP1,CP2) ->
    case lists:member(CP1, Seps) of
        true -> take_l(Cont, Seps, [CP1|Acc]);
        false -> {rev(Acc), Str}
    end;
take_l([Bin|Cont0], Seps, Acc) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, Seps) of
        {nomatch, Cont} ->
            Used = cp_prefix(Cont0, Cont),
            take_l(Cont, Seps, [unicode:characters_to_binary([Bin|Used])|Acc]);
        [Bin1|_]=After when is_binary(Bin1) ->
            First = byte_size(Bin) - byte_size(Bin1),
            <<Keep:First/binary, _/binary>> = Bin,
            {btoken(Keep,Acc), After}
    end;
take_l(Str, Seps, Acc) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, Seps) of
                true -> take_l(Cs, Seps, append(rev(C),Acc));
                false -> {rev(Acc), Str}
            end;
        [] -> {rev(Acc), []}
    end;
take_l(Bin, Seps, Acc) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], Seps) of
        {nomatch,_} ->
            {btoken(Bin, Acc), <<>>};
        [After] ->
            First = byte_size(Bin) - byte_size(After),
            <<Keep:First/binary, _/binary>> = Bin,
            {btoken(Keep, Acc), After}
    end.


take_lc([CP1|Cont]=Str0, {GCs,CPs,_}=Seps, Acc) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Str] = unicode_util:gc(Str0),
            case lists:member(GC, GCs) of
                false -> take_lc(Str, Seps, append(rev(GC),Acc));
                true  -> {rev(Acc), Str0}
            end;
        false ->
            take_lc(Cont, Seps, append(CP1,Acc))
    end;
take_lc([Bin|Cont0], Seps0, Acc) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, Cont0, Seps) of
        {nomatch, Cont} ->
            Used = cp_prefix(Cont0, Cont),
            take_lc(Cont, Seps, [unicode:characters_to_binary([Bin|Used])|Acc]);
        [Bin1|_]=After when is_binary(Bin1) ->
            First = byte_size(Bin) - byte_size(Bin1),
            <<Keep:First/binary, _/binary>> = Bin,
            {btoken(Keep,Acc), After}
    end;
take_lc(Str, {GCs,_,_}=Seps, Acc) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                false -> take_lc(Cs, Seps, append(rev(C),Acc));
                true  -> {rev(Acc), Str}
            end;
        [] -> {rev(Acc), []}
    end;
take_lc(Bin, Seps0, Acc) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, [], Seps) of
        {nomatch,_} ->
            {btoken(Bin, Acc), <<>>};
        [After] ->
            First = byte_size(Bin) - byte_size(After),
            <<Keep:First/binary, _/binary>> = Bin,
            {btoken(Keep, Acc), After}
    end.


take_t([CP1|Cont]=Str0, _, {GCs,CPs,_}=Seps) when is_integer(CP1) ->
    case lists:member(CP1, CPs) of
        true ->
            [GC|Str] = unicode_util:gc(Str0),
            case lists:member(GC, GCs) of
                true ->
                    {Head, Tail} = take_t(Str, 0, Seps),
                    case is_empty(Head) of
                        true ->  {Head, append(GC,Tail)};
                        false -> {append(GC,Head), Tail}
                    end;
                false ->
                    {Head, Tail} = take_t(Str, 0, Seps),
                    {append(GC,Head), Tail}
            end;
        false ->
            {Head, Tail} = take_t(Cont, 0, Seps),
            {[CP1|Head], Tail}
    end;
take_t([Bin|Cont0], N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, Cont0, Seps) of
        {nomatch,Cont} ->
            Used = cp_prefix(Cont0, Cont),
            {Head, Tail} = take_t(Cont, 0, Seps),
            {stack(unicode:characters_to_binary([Bin|Used]), Head), Tail};
        [SepStart|Cont1] ->
            case bin_search_inv(SepStart, Cont1, GCs) of
                {nomatch, Cont} ->
                    {Head, Tail} = take_t(Cont, 0, Seps),
                    Used = cp_prefix(Cont0, Cont),
                    case is_empty(Head) of
                        true ->
                            KeepSz = byte_size(Bin) - byte_size(SepStart),
                            <<Keep:KeepSz/binary, End/binary>> = Bin,
                            {Keep, stack(stack(End,Used),Tail)};
                        false ->
                            {stack(unicode:characters_to_binary([Bin|Used]),Head), Tail}
                    end;
                [NonSep|Cont] when is_binary(NonSep) ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    take_t([Bin|Cont], KeepSz, Seps)
            end
    end;
take_t(Str, 0, {GCs,_,_}=Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            case lists:member(GC, GCs) of
                true ->
                    {Head, Tail} = take_t(Cs1, 0, Seps),
                    case is_empty(Head) of
                        true ->  {Head, append(GC,Tail)};
                        false -> {append(GC,Head), Tail}
                    end;
                false ->
                    {Head, Tail} = take_t(Cs1, 0, Seps),
                    {append(GC,Head), Tail}
            end;
        [] -> {[],[]}
    end;
take_t(Bin, N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    Seps = search_compile(Seps0),
    case bin_search(Rest, [], Seps) of
        {nomatch,_} -> {Bin, <<>>};
        [SepStart] ->
            case bin_search_inv(SepStart, [], GCs) of
                {nomatch,_} ->
                    KeepSz = byte_size(Bin) - byte_size(SepStart),
                    <<Before:KeepSz/binary, End/binary>> = Bin,
                    {Before, End};
                [NonSep] ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    take_t(Bin, KeepSz, Seps)
            end
    end.

take_tc([CP1|[CP2|_]=Cont], _, {GCs,_,_}=Seps) when ?ASCII_LIST(CP1,CP2) ->
    case lists:member(CP1, GCs) of
        false ->
            {Head, Tail} = take_tc(Cont, 0, Seps),
            case is_empty(Head) of
                true -> {Head, append(CP1,Tail)};
                false -> {append(CP1,Head), Tail}
            end;
        true ->
            {Head, Tail} = take_tc(Cont, 0, Seps),
            {append(CP1,Head), Tail}
    end;
take_tc([Bin|Cont0], N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search_inv(Rest, Cont0, GCs) of
        {nomatch,Cont} ->
            Used = cp_prefix(Cont0, Cont),
            {Head, Tail} = take_tc(Cont, 0, Seps0),
            {stack(unicode:characters_to_binary([Bin|Used]), Head), Tail};
        [SepStart|Cont1] ->
            Seps = search_compile(Seps0),
            case bin_search(SepStart, Cont1, Seps) of
                {nomatch, Cont} ->
                    {Head, Tail} = take_tc(Cont, 0, Seps),
                    Used = cp_prefix(Cont0, Cont),
                    case is_empty(Head) of
                        true ->
                            KeepSz = byte_size(Bin) - byte_size(SepStart),
                            <<Keep:KeepSz/binary, End/binary>> = Bin,
                            {Keep, stack(stack(End,Used),Tail)};
                        false ->
                            {stack(unicode:characters_to_binary([Bin|Used]),Head), Tail}
                    end;
                [NonSep|Cont] when is_binary(NonSep) ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    take_tc([Bin|Cont], KeepSz, Seps)
            end
    end;
take_tc(Str, 0, {GCs,_,_}=Seps) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [GC|Cs1] ->
            case lists:member(GC, GCs) of
                false ->
                    {Head, Tail} = take_tc(Cs1, 0, Seps),
                    case is_empty(Head) of
                        true -> {Head, append(GC,Tail)};
                        false -> {append(GC,Head), Tail}
                    end;
                true ->
                    {Head, Tail} = take_tc(Cs1, 0, Seps),
                    {append(GC,Head), Tail}
            end;
        [] -> {[],[]}
    end;
take_tc(Bin, N, {GCs,_,_}=Seps0) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search_inv(Rest, [], GCs) of
        {nomatch,_} -> {Bin, <<>>};
        [SepStart] ->
            Seps = search_compile(Seps0),
            case bin_search(SepStart, [], Seps) of
                {nomatch,_} ->
                    KeepSz = byte_size(Bin) - byte_size(SepStart),
                    <<Before:KeepSz/binary, End/binary>> = Bin,
                    {Before, End};
                [NonSep] ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    take_tc(Bin, KeepSz, Seps)
            end
    end.

prefix_1(Cs0, [GC]) ->
    case unicode_util:gc(Cs0) of
        [GC|Cs] -> Cs;
        _ -> nomatch
    end;
prefix_1([CP|Cs], [Pre|PreR]) when is_integer(CP) ->
    case CP =:= Pre of
        true -> prefix_1(Cs,PreR);
        false -> nomatch
    end;
prefix_1(<<CP/utf8, Cs/binary>>, [Pre|PreR]) ->
    case CP =:= Pre of
        true -> prefix_1(Cs,PreR);
        false -> nomatch
    end;
prefix_1(Cs0, [Pre|PreR]) ->
    case unicode_util:cp(Cs0) of
        [Pre|Cs] ->  prefix_1(Cs,PreR);
        _ -> nomatch
    end.

split_1([CP1|Cs]=Cs0, [C|_]=Needle, _, Where, Curr, Acc) when is_integer(CP1) ->
    case CP1=:=C of
        true ->
            case prefix_1(Cs0, Needle) of
                nomatch -> split_1(Cs, Needle, 0, Where, append(C,Curr), Acc);
                Rest when Where =:= leading ->
                    [rev(Curr), Rest];
                Rest when Where =:= trailing ->
                    split_1(Cs, Needle, 0, Where, [C|Curr], [rev(Curr), Rest]);
                Rest when Where =:= all ->
                    split_1(Rest, Needle, 0, Where, [], [rev(Curr)|Acc])
            end;
        false ->
            split_1(Cs, Needle, 0, Where, append(CP1,Curr), Acc)
    end;
split_1([Bin|Cont0], Needle, Start, Where, Curr0, Acc)
  when is_binary(Bin) ->
    case bin_search_str(Bin, Start, Cont0, Needle) of
        {nomatch,Sz,Cont} ->
            <<Keep:Sz/binary, _/binary>> = Bin,
            split_1(Cont, Needle, 0, Where, [Keep|Curr0], Acc);
        {Before, [Cs0|Cont], After} ->
            Curr = add_non_empty(Before,Curr0),
            case Where of
                leading ->
                    [rev(Curr),After];
                trailing ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    Next = byte_size(Bin) - byte_size(Cs),
                    split_1([Bin|Cont], Needle, Next, Where,
                            Curr0, [rev(Curr),After]);
                all ->
                    split_1(After, Needle, 0, Where, [], [rev(Curr)|Acc])
            end
    end;
split_1(Cs0, [C|_]=Needle, _, Where, Curr, Acc) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs0, Needle) of
                nomatch -> split_1(Cs, Needle, 0, Where, append(C,Curr), Acc);
                Rest when Where =:= leading ->
                    [rev(Curr), Rest];
                Rest when Where =:= trailing ->
                    split_1(Cs, Needle, 0, Where, [C|Curr], [rev(Curr), Rest]);
                Rest when Where =:= all ->
                    split_1(Rest, Needle, 0, Where, [], [rev(Curr)|Acc])
            end;
        [Other|Cs] ->
            split_1(Cs, Needle, 0, Where, append(Other,Curr), Acc);
        [] ->
            {rev(Curr), Acc}
    end;
split_1(Bin, [_C|_]=Needle, Start, Where, Curr0, Acc) ->
    case bin_search_str(Bin, Start, [], Needle) of
        {nomatch,_,_} ->
            <<_:Start/binary, Keep/binary>> = Bin,
            {rev([Keep|Curr0]), Acc};
        {Before, [Cs0], After} ->
            case Where of
                leading ->
                    [rev([Before|Curr0]),After];
                trailing ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    Next = byte_size(Bin) - byte_size(Cs),
                    split_1(Bin, Needle, Next, Where, Curr0,
                            [btoken(Before,Curr0),After]);
                all ->
                    Next = byte_size(Bin) - byte_size(After),
                    <<_:Start/binary, Keep/binary>> = Before,
                    Curr = [Keep|Curr0],
                    split_1(Bin, Needle, Next, Where, [], [rev(Curr)|Acc])
            end
    end.

lexemes_m([CP|_]=Cs0, {GCs,CPs,_}=Seps, Ts) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true ->
                    lexemes_m(Cs2, Seps, Ts);
                false ->
                    {Lexeme,Rest} = lexeme_pick(Cs0, Seps, []),
                    lexemes_m(Rest, Seps, [Lexeme|Ts])
            end;
        false ->
            {Lexeme,Rest} = lexeme_pick(Cs0, Seps, []),
            lexemes_m(Rest, Seps, [Lexeme|Ts])
    end;
lexemes_m([Bin|Cont0], {GCs,_,_}=Seps0, Ts) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, GCs) of
        {nomatch,Cont} ->
            lexemes_m(Cont, Seps0, Ts);
        Cs ->
            Seps = search_compile(Seps0),
            {Lexeme,Rest} = lexeme_pick(Cs, Seps, []),
            lexemes_m(Rest, Seps, [Lexeme|Ts])
    end;
lexemes_m(Cs0, {GCs, _, _}=Seps0, Ts) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                true  ->
                    lexemes_m(Cs, Seps0, Ts);
                false ->
                    Seps = search_compile(Seps0),
                    {Lexeme,Rest} = lexeme_pick(Cs0, Seps, []),
                    lexemes_m(Rest, Seps, [Lexeme|Ts])
            end;
        [] ->
            lists:reverse(Ts)
    end;
lexemes_m(Bin, {GCs,_,_}=Seps0, Ts) when is_binary(Bin) ->
    case bin_search_inv(Bin, [], GCs) of
        {nomatch,_} ->
            lists:reverse(Ts);
        [Cs] ->
            Seps = search_compile(Seps0),
            {Lexeme,Rest} = lexeme_pick(Cs, Seps, []),
            lexemes_m(Rest, Seps, add_non_empty(Lexeme,Ts))
    end.

lexeme_pick([CP|Cs1]=Cs0, {GCs,CPs,_}=Seps, Tkn) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true  ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true -> {rev(Tkn), Cs2};
                false -> lexeme_pick(Cs2, Seps, append(rev(GC),Tkn))
            end;
        false -> lexeme_pick(Cs1, Seps, [CP|Tkn])
    end;
lexeme_pick([Bin|Cont0], Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, Cont0, Seps) of
        {nomatch,_} ->
            lexeme_pick(Cont0, Seps, [Bin|Tkn]);
        [Left|_Cont] = Cs ->
            Bytes = byte_size(Bin) - byte_size(Left),
            <<Lexeme:Bytes/binary, _/binary>> = Bin,
            {btoken(Lexeme, Tkn), Cs}
    end;
lexeme_pick(Cs0, {GCs, CPs, _} = Seps, Tkn) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [CP|Cs] ->
            case lists:member(CP, CPs) of
                true ->
                    [GC|Cs2] = unicode_util:gc(Cs0),
                    case lists:member(GC, GCs) of
                        true -> {rev(Tkn), Cs2};
                        false -> lexeme_pick(Cs2, Seps, append(rev(GC),Tkn))
                    end;
                false ->
                    lexeme_pick(Cs, Seps, append(CP,Tkn))
            end;
        [] ->
            {rev(Tkn), []}
    end;
lexeme_pick(Bin, Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, [], Seps) of
        {nomatch,_} ->
            {btoken(Bin,Tkn), []};
        [Left] ->
            Bytes = byte_size(Bin) - byte_size(Left),
            <<Lexeme:Bytes/binary, _/binary>> = Bin,
            {btoken(Lexeme, Tkn), Left}
    end.

nth_lexeme_m([Bin|Cont0], {GCs,_,_}=Seps0, N) when is_binary(Bin) ->
    case bin_search_inv(Bin, Cont0, GCs) of
        {nomatch,Cont} ->
            nth_lexeme_m(Cont, Seps0, N);
        Cs when N > 1 ->
            Rest = lexeme_skip(Cs, Seps0),
            nth_lexeme_m(Rest, Seps0, N-1);
        Cs ->
            Seps = search_compile(Seps0),
            {Lexeme,_} = lexeme_pick(Cs, Seps, []),
            Lexeme
    end;
nth_lexeme_m(Cs0, {GCs, _, _}=Seps0, N) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, GCs) of
                true ->
                    nth_lexeme_m(Cs, Seps0, N);
                false when N > 1 ->
                    Cs1 = lexeme_skip(Cs, Seps0),
                    nth_lexeme_m(Cs1, Seps0, N-1);
                false ->
                    Seps = search_compile(Seps0),
                    {Lexeme,_} = lexeme_pick(Cs0, Seps, []),
                    Lexeme
            end;
        [] ->
            []
    end;
nth_lexeme_m(Bin, {GCs,_,_}=Seps0, N) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search_inv(Bin, [], GCs) of
        [Cs] when N > 1 ->
            Cs1 = lexeme_skip(Cs, Seps),
            nth_lexeme_m(Cs1, Seps, N-1);
        [Cs] ->
            {Lexeme,_} = lexeme_pick(Cs, Seps, []),
            Lexeme;
        {nomatch,_} ->
            <<>>
    end.

lexeme_skip([CP|Cs1]=Cs0, {GCs,CPs,_}=Seps) when is_integer(CP) ->
    case lists:member(CP, CPs) of
        true  ->
            [GC|Cs2] = unicode_util:gc(Cs0),
            case lists:member(GC, GCs) of
                true -> Cs2;
                false -> lexeme_skip(Cs2, Seps)
            end;
        false ->
            lexeme_skip(Cs1, Seps)
    end;
lexeme_skip([Bin|Cont0], Seps0) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, Cont0, Seps) of
        {nomatch,_} -> lexeme_skip(Cont0, Seps);
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
                false ->
                    lexeme_skip(Cs, Seps)
            end;
        [] ->
            []
    end;
lexeme_skip(Bin, Seps0) when is_binary(Bin) ->
    Seps = search_compile(Seps0),
    case bin_search(Bin, [], Seps) of
        {nomatch,_} -> <<>>;
        [Left] -> tl(unicode_util:gc(Left))
    end.

find_l([C1|Cs]=Cs0, [C|_]=Needle) when is_integer(C1) ->
    case C1 of
        C ->
            case prefix_1(Cs0, Needle) of
                nomatch -> find_l(Cs, Needle);
                _ -> Cs0
            end;
        _ ->
            find_l(Cs, Needle)
    end;
find_l([Bin|Cont0], Needle) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch, _, Cont} ->
            find_l(Cont, Needle);
        {_Before, Cs, _After} ->
            Cs
    end;
find_l(Cs0, [C|_]=Needle) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs0, Needle) of
                nomatch -> find_l(Cs, Needle);
                _ -> Cs0
            end;
        [_C|Cs] ->
            find_l(Cs, Needle);
        [] -> nomatch
    end;
find_l(Bin, Needle) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch,_,_} -> nomatch;
        {_Before, [Cs], _After} -> Cs
    end.

find_r([Cp|Cs]=Cs0, [C|_]=Needle, Res) when is_integer(Cp) ->
    case Cp of
        C ->
            case prefix_1(Cs0, Needle) of
                nomatch -> find_r(Cs, Needle, Res);
                _ -> find_r(Cs, Needle, Cs0)
            end;
        _ ->
            find_r(Cs, Needle, Res)
    end;
find_r([Bin|Cont0], Needle, Res) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch,_,Cont} ->
            find_r(Cont, Needle, Res);
        {_, Cs0, _} ->
            [_|Cs] = unicode_util:gc(Cs0),
            find_r(Cs, Needle, Cs0)
    end;
find_r(Cs0, [C|_]=Needle, Res) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs0, Needle) of
                nomatch -> find_r(Cs, Needle, Res);
                _ -> find_r(Cs, Needle, Cs0)
            end;
        [_C|Cs] ->
            find_r(Cs, Needle, Res);
        [] -> Res
    end;
find_r(Bin, Needle, Res) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch,_,_} -> Res;
        {_Before, [Cs0], _After} ->
            <<_/utf8, Cs/binary>> = Cs0,
            find_r(Cs, Needle, Cs0)
    end.

%% These are used to avoid creating lists around binaries
%% might be unnecessary, is there a better solution?
btoken(Token, []) -> Token;
btoken(BinPart, [C]) when is_integer(C) -> <<C/utf8, BinPart/binary>>;
btoken(<<>>, Tkn) -> lists:reverse(Tkn);
btoken(BinPart, Cs) -> [lists:reverse(Cs),BinPart].

rev([B]) when is_binary(B) -> B;
rev(L) when is_list(L) -> lists:reverse(L);
rev(C) when is_integer(C) -> C.

append(Char, <<>>) when is_integer(Char) -> [Char];
append(Char, <<>>) when is_list(Char) -> Char;
append(Char, Bin) when is_binary(Bin) -> [Char,Bin];
append(Char, Str) when is_integer(Char) -> [Char|Str];
append(GC, Str) when is_list(GC) -> GC ++ Str.

stack(Bin, []) -> Bin;
stack(<<>>, St) -> St;
stack([], St) -> St;
stack(Bin, St) -> [Bin|St].

add_non_empty(<<>>, L) -> L;
add_non_empty(Token, L) -> [Token|L].

cp_prefix(Orig, Cont) ->
    case unicode_util:cp(Cont) of
        [] -> Orig;
        [Cp|Rest] -> cp_prefix_1(Orig, Cp, Rest)
    end.

cp_prefix_1(Orig, Until, Cont) ->
    case unicode_util:cp(Orig) of
        [Until|Rest] ->
            case equal(Rest, Cont) of
                true -> [];
                false-> [Until|cp_prefix_1(Rest, Until, Cont)]
            end;
        [CP|Rest] -> [CP|cp_prefix_1(Rest, Until, Cont)]
    end.


%% Binary special
bin_search(Bin, Cont, {Seps,_,BP}) ->
    bin_search_loop(Bin, 0, BP, Cont, Seps).

%% Need to work with [<<$a>>, <<778/utf8>>],
%% i.e. å in nfd form  $a "COMBINING RING ABOVE"
%% and PREPEND characters like "ARABIC NUMBER SIGN" 1536 <<216,128>>
%% combined with other characters are currently ignored.
search_pattern({_,_,_}=P) -> P;
search_pattern(Seps) ->
    CPs = search_cp(Seps),
    {Seps, CPs, undefined}.

search_compile({Sep, CPs, undefined}) ->
    {Sep, CPs, binary:compile_pattern(bin_pattern(CPs))};
search_compile({_,_,_}=Compiled) -> Compiled.

search_cp([CP|Seps]) when is_integer(CP) ->
    [CP|search_cp(Seps)];
search_cp([Pattern|Seps]) ->
    [CP|_] = unicode_util:cp(Pattern),
    [CP|search_cp(Seps)];
search_cp([]) -> [].

bin_pattern([CP|Seps]) ->
    [<<CP/utf8>>|bin_pattern(Seps)];
bin_pattern([]) -> [].

bin_search_loop(Bin0, Start, _, Cont, _Seps)
  when byte_size(Bin0) =< Start; Start < 0 ->
    {nomatch, Cont};
bin_search_loop(Bin0, Start, BinSeps, Cont, Seps) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, BinSeps) of
        nomatch ->
            {nomatch,Cont};
        {Where, _CL} when Cont =:= [] ->
            <<_:Where/binary, Cont1/binary>> = Bin,
            [GC|Cont2] = unicode_util:gc(Cont1),
            case lists:member(GC, Seps) of
                false when Cont2 =:= [] ->
                    {nomatch, []};
                false ->
                    Next = byte_size(Bin0) - byte_size(Cont2),
                    bin_search_loop(Bin0, Next, BinSeps, Cont, Seps);
                true ->
                    [Cont1]
            end;
        {Where, _CL} ->
            <<_:Where/binary, Cont0/binary>> = Bin,
            Cont1 = [Cont0|Cont],
            [GC|Cont2] = unicode_util:gc(Cont1),
            case lists:member(GC, Seps) of
                false ->
                    case Cont2 of
                        [BinR|Cont] when is_binary(BinR) ->
                            Next = byte_size(Bin0) - byte_size(BinR),
                            bin_search_loop(Bin0, Next, BinSeps, Cont, Seps);
                        _ ->
                            {nomatch, Cont2}
                    end;
                true ->
                    Cont1
            end
    end.

bin_search_inv(<<>>, Cont, _) ->
    {nomatch, Cont};
bin_search_inv(Bin, Cont, [Sep]) ->
    bin_search_inv_1(Bin, Cont, Sep);
bin_search_inv(Bin, Cont, Seps) ->
    bin_search_inv_n(Bin, Cont, Seps).

bin_search_inv_1(<<CP1/utf8, BinRest/binary>>=Bin0, Cont, Sep) ->
    case BinRest of
        <<CP2/utf8, _/binary>> when ?ASCII_LIST(CP1, CP2) ->
            case CP1 of
                Sep -> bin_search_inv_1(BinRest, Cont, Sep);
                _ -> [Bin0|Cont]
            end;
        _ when Cont =:= [] ->
            case unicode_util:gc(Bin0) of
                [Sep|Bin] -> bin_search_inv_1(Bin, Cont, Sep);
                _ -> [Bin0|Cont]
            end;
        _ ->
            case unicode_util:gc([Bin0|Cont]) of
                [Sep|[Bin|Cont]] when is_binary(Bin) ->
                    bin_search_inv_1(Bin, Cont, Sep);
                [Sep|Cs] ->
                    {nomatch, Cs};
                _ -> [Bin0|Cont]
            end
    end;
bin_search_inv_1(<<>>, Cont, _Sep) ->
    {nomatch, Cont};
bin_search_inv_1([], Cont, _Sep) ->
    {nomatch, Cont}.


bin_search_inv_n(<<CP1/utf8, BinRest/binary>>=Bin0, Cont, Seps) ->
    case BinRest of
        <<CP2/utf8, _/binary>> when ?ASCII_LIST(CP1, CP2) ->
            case lists:member(CP1,Seps) of
                true -> bin_search_inv_n(BinRest, Cont, Seps);
                false -> [Bin0|Cont]
            end;
        _ when Cont =:= [] ->
            [GC|Bin] = unicode_util:gc(Bin0),
            case lists:member(GC, Seps) of
                true -> bin_search_inv_n(Bin, Cont, Seps);
                false -> [Bin0|Cont]
            end;
        _ ->
            [GC|Cs0] = unicode_util:gc([Bin0|Cont]),
            case lists:member(GC, Seps) of
                false -> [Bin0|Cont];
                true ->
                    case Cs0 of
                        [Bin|Cont] when is_binary(Bin) ->
                            bin_search_inv_n(Bin, Cont, Seps);
                        _ ->
                            {nomatch, Cs0}
                    end
            end
    end;
bin_search_inv_n(<<>>, Cont, _Sep) ->
    {nomatch, Cont};
bin_search_inv_n([], Cont, _Sep) ->
    {nomatch, Cont}.

bin_search_str(Bin0, Start, [], SearchCPs) ->
    Compiled = binary:compile_pattern(unicode:characters_to_binary(SearchCPs)),
    bin_search_str_1(Bin0, Start, Compiled, SearchCPs);
bin_search_str(Bin0, Start, Cont, [CP|_]=SearchCPs) ->
    First = binary:compile_pattern(<<CP/utf8>>),
    bin_search_str_2(Bin0, Start, Cont, First, SearchCPs).

bin_search_str_1(Bin0, Start, First, SearchCPs) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, First) of
        nomatch -> {nomatch, byte_size(Bin0), []};
        {Where0, _} ->
            Where = Start+Where0,
            <<Keep:Where/binary, Cs0/binary>> = Bin0,
            case prefix_1(Cs0, SearchCPs) of
                nomatch ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    KeepSz = byte_size(Bin0) - byte_size(Cs),
                    bin_search_str_1(Bin0, KeepSz, First, SearchCPs);
                [] ->
                    {Keep, [Cs0], <<>>};
                Rest ->
                    {Keep, [Cs0], Rest}
            end
    end.

bin_search_str_2(Bin0, Start, Cont, First, SearchCPs) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, First) of
        nomatch -> {nomatch, byte_size(Bin0), Cont};
        {Where0, _} ->
            Where = Start+Where0,
            <<Keep:Where/binary, Cs0/binary>> = Bin0,
            [GC|Cs]=unicode_util:gc(Cs0),
            case prefix_1(stack(Cs0,Cont), SearchCPs) of
                nomatch when is_binary(Cs) ->
                    KeepSz = byte_size(Bin0) - byte_size(Cs),
                    bin_search_str_2(Bin0, KeepSz, Cont, First, SearchCPs);
                nomatch ->
                    {nomatch, Where, stack([GC|Cs],Cont)};
                [] ->
                    {Keep, [Cs0|Cont], <<>>};
                Rest ->
                    {Keep, [Cs0|Cont], Rest}
            end
    end.
-endif.
