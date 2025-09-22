-module(maps_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

iterator_1_test() ->
    % Small map test
    M0 = #{a => 1, b => 2},
    {K41, V41, I41} = maps:next(maps:iterator(M0)),
    {K42, V42, I42} = maps:next(I41),
    ?assertEqual(none, maps:next(I42)),
    ?assertEqual(lists:sort([{K41, V41}, {K42, V42}]), lists:sort(maps:to_list(M0))),
    ?assertEqual(lists:sort([{K41, V41}, {K42, V42}]), lists:sort(maps:to_list(maps:iterator(M0)))),
    %% Large map test
    M2 = maps:from_list([{{k, I}, I} || I <- lists:seq(1, 200)]),
    ?assertEqual(lists:sort(iter_kv(maps:iterator(M2))), lists:sort(maps:to_list(M2))),
    ?assertEqual(lists:sort(iter_kv(maps:iterator(M2))), lists:sort(maps:to_list(maps:iterator(M2)))),
    %% Larger map test
    M3 = maps:from_list([{{k, I}, I} || I <- lists:seq(1, 10000)]),
    ?assertEqual(lists:sort(iter_kv(maps:iterator(M3))), lists:sort(maps:to_list(M3))),
    ?assertEqual(lists:sort(iter_kv(maps:iterator(M3))), lists:sort(maps:to_list(maps:iterator(M3)))),
    ok.

maps_test() ->
    %% merge_with/3
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10 => 10},
    ?assertEqual(#{1 => {1, 3}, 2 => {3, 2}, 10 => 10},
                 maps:merge_with(fun(1, 1, 3) -> {1, 3};
                                    (2, 3, 2) -> {3, 2}
                                 end,
                                 Small, Large)),
    % Swapping input maps should reverse tuples
    ?assertEqual(#{1 => {3, 1}, 2 => {2, 3}, 10 => 10},
                 maps:merge_with(fun(1, Val1, Val2) -> {Val1, Val2};
                                    (2, Val1, Val2) -> {Val1, Val2}
                                 end,
                                 Large, Small)),
    % Swapping parameters in the output of the fun should also reverse tuples
    ?assertEqual(#{1 => {3, 1}, 2 => {2, 3}, 10 => 10},
                 maps:merge_with(fun(1, Val1, Val2) -> {Val2, Val1};
                                    (2, Val1, Val2) -> {Val2, Val1}
                                 end,
                                 Small, Large)),
    % Should give the same result as maps:merge/2 with the right combiner
    Merge2FromMerge3 = fun(M1, M2) -> maps:merge_with(fun(_, _, Val2) -> Val2 end, M1, M2) end,
    check_map_combiners_same_small(fun maps:merge/2, Merge2FromMerge3, 1),
    check_map_combiners_same_large(fun maps:merge/2, Merge2FromMerge3, 2),
    % Should conceptually compute the same thing as lists:ukey_merge/2 with the right combiner
    MergeFromUKeyMerge = fun(M1, M2) ->
                             % ukeymerge takes from the first when collision
                             maps:from_list(lists:ukeymerge(1,
                                                            lists:sort(maps:to_list(M2)), lists:sort(maps:to_list(M1))))
                         end,
    check_map_combiners_same_small(MergeFromUKeyMerge, Merge2FromMerge3, 3),
    check_map_combiners_same_large(MergeFromUKeyMerge, Merge2FromMerge3, 4),
    % Empty maps
    ?assertEqual(Large, maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, Large, #{})),
    ?assertEqual(Large, maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, #{}, Large)),
    ?assertEqual(#{}, maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, #{}, #{})),
    % Errors
    ?assertError(badarg, maps:merge_with(not_a_fun, #{}, #{})),
    ?assertError({badmap, a}, maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    ?assertError({badmap, b}, maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    ?assertError({badmap, a}, maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    %% intersect/2
    ?assertEqual(#{1 => 3, 2 => 2}, maps:intersect(Small, Large)),
    % Swapping input maps can make a difference
    ?assertEqual(#{1 => 1, 2 => 3}, maps:intersect(Large, Small)),
    % Should conceptually compute the same thing as gb_sets:intersect/2 with the right combiner
    IntersectFromGBSets = fun(M1, M2) ->
                              lists:foldl(fun(Key, SoFar) -> SoFar#{Key => maps:get(Key, M2)} end,
                                          #{},
                                          gb_sets:to_list(gb_sets:intersection(gb_sets:from_list(maps:keys(M1)),
                                                                               gb_sets:from_list(maps:keys(M2)))))
                          end,
    check_map_combiners_same_small(fun maps:intersect/2, IntersectFromGBSets, 11),
    check_map_combiners_same_large(fun maps:intersect/2, IntersectFromGBSets, 13),
    % Empty maps
    ?assertEqual(#{}, maps:intersect(Large, #{})),
    ?assertEqual(#{}, maps:intersect(#{}, Large)),
    ?assertEqual(#{}, maps:intersect(#{}, #{})),
    % Errors
    ?assertError({badmap, a}, maps:intersect(a, #{})),
    ?assertError({badmap, b}, maps:intersect(#{}, b)),
    ?assertError({badmap, a}, maps:intersect(a, b)),
    %% intersect_with/3
    ?assertEqual(#{1 => {1, 3}, 2 => {3, 2}},
                 maps:intersect_with(fun(1, 1, 3) -> {1, 3};
                                        (2, 3, 2) -> {3, 2}
                                     end,
                                     Small, Large)),
    % Swapping input maps should reverse tuples
    ?assertEqual(#{1 => {3, 1}, 2 => {2, 3}},
                 maps:intersect_with(fun(1, Val1, Val2) -> {Val1, Val2};
                                        (2, Val1, Val2) -> {Val1, Val2}
                                     end,
                                     Large, Small)),
    % Swapping parameters in the output of the fun should also reverse tuples
    ?assertEqual(#{1 => {3, 1}, 2 => {2, 3}},
                 maps:intersect_with(fun(1, Val1, Val2) -> {Val2, Val1};
                                        (2, Val1, Val2) -> {Val2, Val1}
                                     end,
                                     Small, Large)),
    % Should give the same result as intersect/2 with the right combiner
    Intersect2FromIntersect3 = fun(M1, M2) -> maps:intersect_with(fun(_, _, Val2) -> Val2 end, M1, M2) end,
    check_map_combiners_same_small(fun maps:intersect/2, Intersect2FromIntersect3, 7),
    check_map_combiners_same_large(fun maps:intersect/2, Intersect2FromIntersect3, 8),
    % Empty maps
    ?assertEqual(#{}, maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, Large, #{})),
    ?assertEqual(#{}, maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, #{}, Large)),
    ?assertEqual(#{}, maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, #{}, #{})),
    % Errors
    ?assertError(badarg, maps:intersect_with(not_a_fun, #{}, #{})),
    ?assertError({badmap, a}, maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    ?assertError({badmap, b}, maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    ?assertError({badmap, a}, maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    %% filtermap/2
    M0 = maps:from_list([{I, I} || I <- lists:seq(1, 30)]),
    Pred = fun(K, _) when K =< 10 -> true;
              (K, _) when K =< 20 -> false;
              (_, V) -> {true, V * V}
           end,
    M3 = maps:filtermap(Pred, M0),
    ?assertMatch(#{1 := 1, 10 := 10, 21 := 21 * 21, 30 := 30 * 30}, M3),
    ?assertNot(maps:is_key(11, M3)),
    ?assertNot(maps:is_key(20, M3)),
    ?assertEqual(M3, maps:filtermap(Pred, maps:iterator(M0))),
    % Errors
    ?assertError({badmap, a}, maps:filtermap(fun(_, _) -> ok end, a)),
    ?assertError(badarg, maps:filtermap(<<>>, #{})),
    % from_keys/2
    Map0 = maps:from_keys(["a", 2, {three}], value),
    ?assertEqual(3, map_size(Map0)),
    ?assertEqual(#{"a" => value, 2 => value, {three} => value}, Map0),
    Map1 = maps:from_keys([1, 2, 2], {complex, value}),
    ?assertEqual(2, map_size(Map1)),
    ?assertEqual(#{1 => {complex, value}, 2 => {complex, value}}, Map1),
    ?assertEqual(0, map_size(maps:from_keys([], value))),
    % Errors
    ?assertError(badarg, maps:from_keys([a|b], value)),
    ?assertError(badarg, maps:from_keys(not_list, value)),
    % foreach/2
    % Errors
    ?assertError({badmap, a}, maps:foreach(fun(_, _) -> ok end, a)),
    ?assertError({badmap, []}, maps:foreach(fun(_, _) -> ok end, [])),
    ?assertError({badmap, {}}, maps:foreach(fun(_, _) -> ok end, {})),
    ?assertError({badmap, 42}, maps:foreach(fun(_, _) -> ok end, 42)),
    ?assertError({badmap, <<>>}, maps:foreach(fun(_, _) -> ok end, <<>>)),
    ?assertError(badarg, maps:foreach(fun() -> ok end, #{})),
    ?assertError(badarg, maps:foreach(fun(_, _, _) -> ok end, #{})),
    ?assertError(badarg, maps:foreach(a, #{})),
    ?assertError(badarg, maps:foreach([], #{})),
    ?assertError(badarg, maps:foreach({}, #{})),
    ?assertError(badarg, maps:foreach(42, #{})),
    ?assertError(badarg, maps:foreach(<<>>, #{})),
    % groups_from_list/2
    ?assertEqual(#{}, maps:groups_from_list(fun erlang:length/1, [])),
    ?assertEqual(#{0 => [2], 1 => [1, 3]}, maps:groups_from_list(fun(X) -> X rem 2 end, [1, 2, 3])),
    % groups_from_list/3
    ?assertEqual(#{3 => ["tna", "tac"], 5 => ["ognid"], 7 => ["olaffub"]},
                 maps:groups_from_list(fun erlang:length/1, fun lists:reverse/1, ["ant", "buffalo", "cat", "dingo"])),
    ok.

check_map_combiners_same_small(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(fun(SizeConstant) ->
                      lists:foreach(fun(SeedMult) ->
                                        RandMap1 = random_map({SizeConstant, 100000 * SeedMult, Seed}),
                                        RandMap2 = random_map({SizeConstant, 200000 * SeedMult, Seed}),
                                        ?assertEqual(MapCombiner1(RandMap1, RandMap2), MapCombiner2(RandMap1, RandMap2))
                                    end,
                                    lists:seq(1, 100))

                  end,
                  lists:seq(1, 10)).

check_map_combiners_same_large(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(fun(SizeConstant) ->
                      RandMap1 = random_map({SizeConstant, SizeConstant, Seed}),
                      RandMap2 = random_map({SizeConstant, SizeConstant, Seed}),
                      ?assertEqual(MapCombiner1(RandMap1, RandMap2), MapCombiner2(RandMap1, RandMap2))
                  end,
                  [1000, 10000]).

random_map({SizeConstant, _, _} = InitSeed) ->
    {Ret, _} = lists:foldl(fun(_, {Map, Seed}) ->
                               rand:uniform_s(Seed),
                               {K, Seed2} = rand:uniform_s(SizeConstant, Seed),
                               {V, Seed3} = rand:uniform_s(SizeConstant * 100, Seed2),
                               {Map#{K => V}, Seed3}
                           end,
                           {#{}, rand:seed_s(exsplus, InitSeed)},
                           lists:seq(1, SizeConstant)),
    Ret.

iter_kv(I) ->
    case maps:next(I) of
        none -> [];
        {K, V, NI} -> [{K, V}|iter_kv(NI)]
    end.
