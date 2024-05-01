-module(otpbp_string).

-ifndef(HAVE_string__jaro_similarity_2).
% OTP 27.0
-export([jaro_similarity/2]).
-endif.

-ifndef(HAVE_string__jaro_similarity_2).
jaro_similarity(A0, B0) ->
    case {str_to_gcl_and_length(A0), str_to_indexmap(B0)} of
        {{_, 0}, [_|0]} -> 1.0;
        {{_, ALen}, [_|BLen]} when ALen =:= 0; BLen =:= 0 -> 0.0;
        {{A, ALen}, [B|BLen]} ->
            case jaro_match(A, B, max(ALen, BLen) div 2) of
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
