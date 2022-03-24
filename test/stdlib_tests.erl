-module(stdlib_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

dict_test() ->
    D = dict:from_list([{a, 1}]),
    ?assert(dict:is_empty(dict:new())),
    ?assertNot(dict:is_empty(D)),
    ?assertEqual(dict:take(a, D), {1, dict:new()}),
    ?assertEqual(dict:take(a, dict:new()), error),
    ?assertEqual(dict:take(b, D), error).

orddict_test() ->
    D = orddict:from_list([{3, c}, {1, a}, {2, b}]),
    ?assert(orddict:is_empty(orddict:new())),
    ?assertNot(orddict:is_empty(D)),
    ?assertEqual(orddict:take(1, D), {a, orddict:from_list([{3, c}, {2, b}])}),
    ?assertEqual(orddict:take(1, orddict:new()), error),
    ?assertEqual(orddict:take(4, D), error).

sets_test() ->
    S = sets:from_list([a, 1]),
    ?assert(sets:is_empty(sets:new())),
    ?assertNot(sets:is_empty(S)).

ordsets_test() ->
    S = ordsets:from_list([3,1,2]),
    ?assert(ordsets:is_empty(ordsets:new())),
    ?assertNot(ordsets:is_empty(S)).

gb_trees_test() ->
    D1 = gb_trees:from_orddict(orddict:from_list([{3, c}, {1, a}, {2, b}])),
    ?assertEqual({1, a, gb_trees:iterator_from(2, D1)}, gb_trees:next(gb_trees:iterator(D1))),
    D2 = gb_trees:insert(a, 1, gb_trees:empty()),
    ?assertEqual(gb_trees:take(a, D2), {1, gb_trees:empty()}),
    ?assertEqual(gb_trees:take_any(a, D2), {1, gb_trees:empty()}),
    ?assertEqual(gb_trees:take_any(a, gb_trees:empty()), error),
    ?assertEqual(gb_trees:take_any(b, D2), error).

gb_sets_test() ->
    S = gb_sets:from_ordset(ordsets:from_list([3, 1, 2])),
    ?assertEqual({1, gb_sets:iterator_from(2, S)}, gb_sets:next(gb_sets:iterator(S))).

lists_test() ->
    % search/2
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, []), false),
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, [1,2,3,4,5,6,7,8]), {value, 2}),
    % enumerate/1, enumerate/2
    ?assertEqual([], lists:enumerate([])),
    ?assertEqual([], lists:enumerate(10, [])),
    ?assertEqual([{1, a}, {2, b}, {3, c}], lists:enumerate([a,b,c])),
    ?assertEqual([{10, a}, {11, b}, {12, c}], lists:enumerate(10, [a,b,c])),
    ?assertError(function_clause, lists:enumerate(0)),
    ?assertError(function_clause, lists:enumerate(0, 10)),
    ?assertError(function_clause, lists:enumerate(1.0, [])),
    ?assertError(function_clause, lists:enumerate(1.0, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(<<1>>, [])),
    ?assertError(function_clause, lists:enumerate(<<1>>, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(1, <<1, 2, 3>>)),
    % uniq/1
    ?assertEqual([], lists:uniq([])),
    ?assertEqual(["foo", "bar", "zoo"], lists:uniq(["foo", "foo", "bar", "foo", "zoo", "foo", "bar", "zoo"])),
    ?assertEqual([a, 1, b, 2], lists:uniq([a, a, a, 1, b, 2, a, 2, 1])),
    ?assertEqual([<<"home">>, "home"], lists:uniq([<<"home">>, "home"])),
    ?assertEqual([3.14159, 2.71828, 3.17], lists:uniq([3.14159, 3.14159, 2.71828, 3.17])),
    ?assertEqual([42, 42.0], lists:uniq([42, 42.0, 42, 42.0])),
    % uniq/2
    ?assertEqual([], lists:uniq(fun(X) -> X end, [])),
    ?assertEqual([{42, 1}, {42.0, 99}, {a, 99}],
                 lists:uniq(fun(X) -> element(1, X) end, [{42, 1}, {42.0, 99}, {a, 99}, {a, 1}, {42, 100}])),
    ?assertEqual([1], lists:uniq(fun(_) -> whatever end, lists:seq(1, 10))),
    ok.

maps_test() ->
    %% update_with/3
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    Map = #{key1 => V1, key2 => V2, "key3" => V3},
    Fun = fun(V) -> [V, V, {V, V}] end,
    ?assertMatch(#{key1 := [V1, V1, {V1, V1}]}, maps:update_with(key1, Fun, Map)),
    ?assertMatch(#{key2 := [V2, V2, {V2, V2}]}, maps:update_with(key2, Fun, Map)),
    ?assertMatch(#{"key3" := [V3, V3, {V3, V3}]}, maps:update_with("key3", Fun, Map)),
    ?assertError({badmap, b}, maps:update_with([a, b], a, b)),
    ?assertError(badarg, maps:update_with([a, b], a, #{})),
    ?assertError({badkey, [a, b]}, maps:update_with([a, b], Fun, #{})),
    %% update_with/4
    Init = 3,
    ?assertMatch(#{key1 := [V1, V1, {V1, V1}]}, maps:update_with(key1, Fun, Init, Map)),
    ?assertMatch(#{key2 := [V2, V2, {V2, V2}]}, maps:update_with(key2, Fun, Init, Map)),
    ?assertMatch(#{"key3" := [V3, V3, {V3, V3}]}, maps:update_with("key3", Fun, Init, Map)),
    ?assertMatch(#{key3 := Init}, maps:update_with(key3, Fun, Init, Map)),
    ?assertError({badmap, b}, maps:update_with([a, b], a, Init, b)),
    ?assertError(badarg, maps:update_with([a, b], a, Init, #{})),
    % Disabled for Erlang/OTP < 18
    %?assertError({badmap,a}, maps:size(a)),
    %?assertError({badmap,<<>>}, maps:size(<<>>)),
    %% iterator/1
    % Small map test
    M4 = #{a => 1, b => 2},
    {K41, V41, I41} = maps:next(maps:iterator(M4)),
    {K42, V42, I42} = maps:next(I41),
    ?assertEqual(none, maps:next(I42)),
    ?assertEqual(lists:sort([{K41, V41}, {K42, V42}]), lists:sort(maps:to_list(M4))),
    %% Large map test
    M5 = maps:from_list([{{k, I}, I} || I <- lists:seq(1, 200)]),
    ?assertEqual(lists:sort(iter_kv(maps:iterator(M5))), lists:sort(maps:to_list(M5))),
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
                              lists:foldl(fun(Key, SoFar) -> maps:put(Key, maps:get(Key, M2), SoFar) end,
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
    % maps:iterator/1 requires Erlang/OTP >= 21
    %?assertEqual(M3, maps:filtermap(Pred, maps:iterator(M0))),
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
                               {maps:put(K, V, Map), Seed3}
                           end,
                           {#{}, rand:seed_s(exsplus, InitSeed)},
                           lists:seq(1, SizeConstant)),
    Ret.

iter_kv(I) ->
    case maps:next(I) of
        none -> [];
        {K, V, NI} -> [{K, V}|iter_kv(NI)]
    end.

queue_test() ->
    Q1 = queue:from_list([11, 22, 33, 44]),
    ?assertEqual([11, 22 * 22, 33 * 33], queue:to_list(queue:filtermap(fun(X) when X < 17 -> true;
                                                                          (X) when X > 37 -> false;
                                                                          (X) -> {true, X*X}
                                                                       end, Q1))),
    ?assertEqual([22 * 22, 33 * 33, 44],
                                         queue:to_list(queue:filtermap(fun(X) when X < 17 -> false;
                                                                          (X) when X > 37 -> true;
                                                                          (X) -> {true, X*X}
                                                                       end, Q1))),
    L = lists:seq(1, 2 * 50),
    Q2 = queue:from_list(L),
    ?assertEqual(lists:sum(L), queue:fold(fun(X, A) -> X + A end, 0, Q2)),
    ?assertEqual([X * X || X <- L], lists:reverse(queue:fold(fun(X, A) -> [X * X|A] end, [], Q2))),
    ?assertEqual(false, queue:any(fun(X) -> X > 9 end, queue:from_list([]))),
    AQ = queue:from_list([1, 2, 3]),
    ?assertEqual(true, queue:any(fun(X) -> X > 1 end, AQ)),
    ?assertEqual(false, queue:any(fun(X) -> X < 1 end, AQ)),
    ?assertEqual(true, queue:any(fun(X) -> X < 3 end, AQ)),
    ?assertEqual(false, queue:any(fun(X) -> X > 3 end, AQ)),
    ?assertEqual(true, queue:all(fun(X) -> X > 9 end, queue:from_list([]))),
    ?assertEqual(true, queue:all(fun(X) -> X >= 1 end, AQ)),
    ?assertEqual(false, queue:all(fun(X) -> X>1 end, AQ)),
    ?assertEqual(true, queue:all(fun(X) -> X=<3 end, AQ)),
    ?assertEqual(false, queue:all(fun(X) -> X<3 end, AQ)),
    ?assertEqual([], queue:to_list(queue:delete(x, queue:new()))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete(x, queue:from_list([1, 2, 3])))),
    ?assertEqual([2, 3], queue:to_list(queue:delete(x, queue:from_list([x, 2, 3])))),
    ?assertEqual([1, 3], queue:to_list(queue:delete(x, queue:from_list([1, x, 3])))),
    ?assertEqual([1, 2], queue:to_list(queue:delete(x, queue:from_list([1, 2, x])))),
    ?assertEqual([1, 2, x], queue:to_list(queue:delete(x, queue:from_list([x, 1, 2, x])))),
    ?assertEqual([], queue:to_list(queue:delete_r(x, queue:new()))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete_r(x, queue:from_list([1, 2, 3])))),
    ?assertEqual([2, 3], queue:to_list(queue:delete_r(x, queue:from_list([x, 2, 3])))),
    ?assertEqual([1, 3], queue:to_list(queue:delete_r(x, queue:from_list([1, x, 3])))),
    ?assertEqual([1, 2], queue:to_list(queue:delete_r(x, queue:from_list([1, 2, x])))),
    ?assertEqual([x, 1, 2], queue:to_list(queue:delete_r(x, queue:from_list([x, 1, 2, x])))),
    ?assertEqual([], queue:to_list(queue:delete_with(fun(_) -> false end, queue:new()))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete_with(fun(X) -> X < 0 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([2, 3], queue:to_list(queue:delete_with(fun(X) -> X > 0 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 3], queue:to_list(queue:delete_with(fun(X) -> X > 1 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 2], queue:to_list(queue:delete_with(fun(X) -> X > 2 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete_with(fun(X) -> X > 1 end, queue:from_list([1, 2, 2, 3])))),
    ?assertEqual([], queue:to_list(queue:delete_with_r(fun(_) -> false end, queue:new()))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete_with_r(fun(X) -> X > 9 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 2], queue:to_list(queue:delete_with_r(fun(X) -> X < 9 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 3], queue:to_list(queue:delete_with_r(fun(X) -> X < 3 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([2, 3], queue:to_list(queue:delete_with_r(fun(X) -> X < 2 end, queue:from_list([1, 2, 3])))),
    ?assertEqual([1, 2, 3], queue:to_list(queue:delete_with_r(fun(X) -> X < 3 end, queue:from_list([1, 2, 2, 3])))),
    ok.

proplists_test() ->
    % to_map/1
    ?assertEqual(#{a => true, b => 1, c => 2}, proplists:to_map([a, {b, 1}, {c, 2}, {c, 3}])),
    ?assertEqual(#{}, proplists:to_map([])),
    ?assertEqual(#{a => true, b => true}, proplists:to_map([a, b])),
    ?assertEqual(#{a => true, b => true}, proplists:to_map([b, a])),
    ?assertEqual(#{a => 1, b => true}, proplists:to_map([{a, 1}, b])),
    ?assertEqual(#{a => 1, b => true}, proplists:to_map([b, {a, 1}])),
    ?assertEqual(#{a => 1, b => 2}, proplists:to_map([{a, 1}, {b, 2}])),
    ?assertEqual(#{a => 1, b => 2}, proplists:to_map([{b, 2}, {a, 1}])),
    ?assertEqual(#{b => true}, proplists:to_map(["a", b])),
    ?assertEqual(#{b => true}, proplists:to_map([b, "a"])),
    ?assertEqual(#{b => true}, proplists:to_map([{a}, b])),
    ?assertEqual(#{b => true}, proplists:to_map([b, {a}])),
    ?assertEqual(#{b => true}, proplists:to_map([{a, 1, 2}, b])),
    ?assertEqual(#{b => true}, proplists:to_map([b, {a, 1, 2}])),

    % Ensure that maps:get/3 using the created map yields the same results as proplists:get_value/3 on the original
    % proplist does, and that proplists:get_value/3 on a proplist created from the map yields the same results as
    % proplists:get_value/3 on the original proplist, ie they either all return the same `Value',
    % or they all return the `Default' given as respective third argument.
    Default = make_ref(),
    pm_fold(fun(L1, Acc) ->
                LKs = proplists:get_keys(L1),
                M = proplists:to_map(L1),
                L2 = proplists:from_map(M),
                ?assertEqual(lists:sort(maps:keys(M)), lists:sort(proplists:get_keys(L2))),
                lists:foreach(fun(K) ->
                                  R1 = maps:get(K, M, Default),
                                  R2 = proplists:get_value(K, L1, Default),
                                  R3 = proplists:get_value(K, L2, Default),
                                  ?assert(R1 =:= Default andalso R2 =:= Default andalso R3 =:= Default
                                          orelse R1 =:= R2 andalso R1 =:= R3)
                              end, LKs),
                Acc
            end,
            undefined, [a, b, {a, 1}, {}, {a}, {a, 1, 2}, {c, 1, 2}, "foo"]),

    Fun = fun(M, A) -> [M|A] end,
    ?assertEqual([], pm_fold(Fun, [], [])),
    ?assertEqual([[1]], lists:sort(pm_fold(Fun, [], [1]))),
    ?assertEqual(lists:sort([[1, 2], [2, 1]]), lists:sort(pm_fold(Fun, [], [1, 2]))),
    ?assertEqual(lists:sort([[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]),
                 lists:sort(pm_fold(Fun, [], [1, 2, 3]))),

    Stages = [{aliases, [{a, alias_a}]}, {negations, [{no_b, b}]}, {expand, [{c, [d]}]}],
    M1 = proplists:to_map([], Stages),
    ?assertEqual(M1, #{}),
    ?assertEqual(M1, proplists:to_map(proplists:normalize([], Stages))),
    List = [a, no_b, c],
    M2 = proplists:to_map(List, Stages),
    ?assertEqual(M2, #{alias_a => true, b => false, d => true}),
    ?assertEqual(M2, proplists:to_map(proplists:normalize(List, Stages))),
    ok.

pm_fold(_, _, []) -> [];
pm_fold(Fun, Acc0, L) -> pm_fold(Fun, Acc0, L, []).

pm_fold(Fun, Acc, [], Mut) -> Fun(Mut, Acc);
pm_fold(Fun, Acc, L, Mut) -> lists:foldl(fun(X, AccIn) -> pm_fold(Fun, AccIn, lists:delete(X, L), [X|Mut]) end, Acc, L).

binary_test() ->
    % Vector test imported from the RFC 4648 section 10.
    ?assertEqual(<<>>, binary:encode_hex(<<>>)),
    ?assertEqual(<<"66">>, binary:encode_hex(<<"f">>)),
    ?assertEqual(<<"666F">>, binary:encode_hex(<<"fo">>)),
    ?assertEqual(<<"666F6F">>, binary:encode_hex(<<"foo">>)),
    ?assertEqual(<<"666F6F62">>, binary:encode_hex(<<"foob">>)),
    ?assertEqual(<<"666F6F6261">>, binary:encode_hex(<<"fooba">>)),
    ?assertEqual(<<"666F6F626172">>, binary:encode_hex(<<"foobar">>)),
    ?assertEqual(<<>>, binary:decode_hex(<<>>)),
    ?assertEqual(<<"f">>, binary:decode_hex(<<"66">>)),
    ?assertEqual(<<"fo">>, binary:decode_hex(<<"666F">>)),
    ?assertEqual(<<"foo">>, binary:decode_hex(<<"666F6F">>)),
    ?assertEqual(<<"foob">>, binary:decode_hex(<<"666F6F62">>)),
    ?assertEqual(<<"fooba">>, binary:decode_hex(<<"666F6F6261">>)),
    ?assertEqual(<<"foobar">>, binary:decode_hex(<<"666F6F626172">>)),
    ?assertEqual(<<"fo">>, binary:decode_hex(<<"666f">>)),
    ?assertEqual(<<"foo">>, binary:decode_hex(<<"666f6f">>)),
    ?assertEqual(<<"foob">>, binary:decode_hex(<<"666f6f62">>)),
    ?assertEqual(<<"fooba">>, binary:decode_hex(<<"666f6f6261">>)),
    ?assertEqual(<<"foobar">>, binary:decode_hex(<<"666f6f626172">>)),
    ?assertEqual(<<"foobar">>, binary:decode_hex(<<"666f6F626172">>)),
    ok.
