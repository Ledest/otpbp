-module(stdlib_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

sets_test() ->
    ?assert(sets:is_empty(sets:new())),
    ?assertNot(sets:is_empty(sets:from_list([a, 1]))).

ordsets_test() ->
    ?assert(ordsets:is_empty(ordsets:new())),
    ?assertNot(ordsets:is_empty(ordsets:from_list([3,1,2]))).

lists_test() ->
    % search/2
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, []), false),
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, [1,2,3,4,5,6,7,8]), {value, 2}),
    % enumerate/1, enumerate/2, enumerate/3
    ?assertEqual([], lists:enumerate([])),
    ?assertEqual([], lists:enumerate(10, [])),
    ?assertEqual([], lists:enumerate(-10, [])),
    ?assertEqual([], lists:enumerate(10, 2, [])),
    ?assertEqual([], lists:enumerate(10, -2, [])),
    ?assertEqual([], lists:enumerate(-10, 2, [])),
    ?assertEqual([], lists:enumerate(-10, -2, [])),
    ?assertEqual([{1, a}, {2, b}, {3, c}], lists:enumerate([a, b, c])),
    ?assertEqual([{10, a}, {11, b}, {12, c}], lists:enumerate(10, [a, b, c])),
    ?assertEqual([{-10, a}, {-9, b}, {-8, c}], lists:enumerate(-10, [a, b, c])),
    ?assertEqual([{10, a}, {12, b}, {14, c}], lists:enumerate(10, 2, [a, b, c])),
    ?assertEqual([{10, a}, {8, b}, {6, c}], lists:enumerate(10, -2, [a, b, c])),
    ?assertEqual([{-10, a}, {-12, b}, {-14, c}], lists:enumerate(-10, -2, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(0)),
    ?assertError(function_clause, lists:enumerate(0, 10)),
    ?assertError(function_clause, lists:enumerate(0, 10, 20)),
    ?assertError(function_clause, lists:enumerate(1.0, [])),
    ?assertError(function_clause, lists:enumerate(1.0, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(1.0, 2, [])),
    ?assertError(function_clause, lists:enumerate(1.0, 2, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(1, 2.0, [])),
    ?assertError(function_clause, lists:enumerate(1, 2.0, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(1.0, 2.0, [])),
    ?assertError(function_clause, lists:enumerate(1.0, 2.0, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(<<1>>, [])),
    ?assertError(function_clause, lists:enumerate(<<1>>, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(<<1>>, 2, [])),
    ?assertError(function_clause, lists:enumerate(<<1>>, 2, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(1, <<2>>, [])),
    ?assertError(function_clause, lists:enumerate(1, <<2>>, [a, b, c])),
    ?assertError(function_clause, lists:enumerate(<<1, 2, 3>>)),
    ?assertError(function_clause, lists:enumerate(1, <<1, 2, 3>>)),
    ?assertError(function_clause, lists:enumerate(1, 2, <<1, 2, 3>>)),
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
    % zip/3 fail
    ?assertEqual([], lists:zip([], [], fail)),
    ?assertError(function_clause, lists:zip([a], [], fail)),
    ?assertError(function_clause, lists:zip([], [c], fail)),
    ?assertEqual([{a, c}], lists:zip([a], [c], fail)),
    ?assertError(function_clause, lists:zip([a, b], [c], fail)),
    ?assertError(function_clause, lists:zip([a], [c, d], fail)),
    % zip/3 trim
    ?assertEqual([], lists:zip([], [], trim)),
    ?assertEqual([], lists:zip([a], [], trim)),
    ?assertEqual([], lists:zip([], [c], trim)),
    ?assertEqual([{a, c}], lists:zip([a], [c], trim)),
    ?assertEqual([{a, c}], lists:zip([a, b], [c], trim)),
    ?assertEqual([{a, c}], lists:zip([a], [c, d], trim)),
    % zip/3 pad
    ?assertEqual([], lists:zip([], [], {pad, {x, y}})),
    ?assertEqual([{a, y}], lists:zip([a], [], {pad, {x, y}})),
    ?assertEqual([{x, c}], lists:zip([], [c], {pad, {x, y}})),
    ?assertEqual([{a, c}], lists:zip([a], [c], {pad, {x, y}})),
    ?assertEqual([{a, c}, {b, y}], lists:zip([a, b], [c], {pad, {x, y}})),
    ?assertEqual([{a, c}, {x, d}], lists:zip([a], [c, d], {pad, {x, y}})),
    % zip3/4 fail
    ?assertEqual([], lists:zip3([], [], [], fail)),
    ?assertError(function_clause, lists:zip3([a], [], [], fail)),
    ?assertError(function_clause, lists:zip3([], [c], [], fail)),
    ?assertError(function_clause, lists:zip3([a], [c], [], fail)),
    ?assertError(function_clause, lists:zip3([], [], [e], fail)),
    ?assertError(function_clause, lists:zip3([a], [], [e], fail)),
    ?assertError(function_clause, lists:zip3([], [c], [e], fail)),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c], [e], fail)),
    ?assertError(function_clause, lists:zip3([a, b], [c], [e], fail)),
    ?assertError(function_clause, lists:zip3([a], [c, d], [e], fail)),
    ?assertError(function_clause, lists:zip3([a, b], [c, d], [e], fail)),
    ?assertError(function_clause, lists:zip3([a], [c], [e, f], fail)),
    ?assertError(function_clause, lists:zip3([a, b], [c], [e, f], fail)),
    ?assertError(function_clause, lists:zip3([a], [c, d], [e, f], fail)),
    % zip3/4 trim
    ?assertEqual([], lists:zip3([], [], [], trim)),
    ?assertEqual([], lists:zip3([a], [], [], trim)),
    ?assertEqual([], lists:zip3([], [c], [], trim)),
    ?assertEqual([], lists:zip3([a], [c], [], trim)),
    ?assertEqual([], lists:zip3([], [], [e], trim)),
    ?assertEqual([], lists:zip3([a], [], [e], trim)),
    ?assertEqual([], lists:zip3([], [c], [e], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c], [e], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a, b], [c], [e], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c, d], [e], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a, b], [c, d], [e], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c], [e, f], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a, b], [c], [e, f], trim)),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c, d], [e, f], trim)),
    % zip3/4 pad
    ?assertEqual([], lists:zip3([], [], [], {pad, {x, y, z}})),
    ?assertEqual([{a, y, z}], lists:zip3([a], [], [], {pad, {x, y, z}})),
    ?assertEqual([{x, c, z}], lists:zip3([], [c], [], {pad, {x, y, z}})),
    ?assertEqual([{a, c, z}], lists:zip3([a], [c], [], {pad, {x, y, z}})),
    ?assertEqual([{x, y, e}], lists:zip3([], [], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, y, e}], lists:zip3([a], [], [e], {pad, {x, y, z}})),
    ?assertEqual([{x, c, e}], lists:zip3([], [c], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}], lists:zip3([a], [c], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {b, y, z}], lists:zip3([a, b], [c], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {x, d, z}], lists:zip3([a], [c, d], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {b, d, z}], lists:zip3([a, b], [c, d], [e], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {x, y, f}], lists:zip3([a], [c], [e, f], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {b, y, f}], lists:zip3([a, b], [c], [e, f], {pad, {x, y, z}})),
    ?assertEqual([{a, c, e}, {x, d, f}], lists:zip3([a], [c, d], [e, f], {pad, {x, y, z}})),
    % zipwith/4 fail
    ?assertEqual([], lists:zipwith(fun(A, B) -> A * B end, [], [], fail)),
    ?assertError(function_clause, lists:zipwith(fun(A, B) -> A * B end, [2], [], fail)),
    ?assertError(function_clause, lists:zipwith(fun(A, B) -> A * B end, [], [5], fail)),
    ?assertEqual([2 * 5], lists:zipwith(fun(A, B) -> A * B end, [2], [5], fail)),
    ?assertError(function_clause, lists:zipwith(fun(A, B) -> A * B end, [2, 3], [5], fail)),
    ?assertError(function_clause, lists:zipwith(fun(A, B) -> A * B end, [2], [5, 7], fail)),
    % zipwith/4 trim
    ?assertEqual([], lists:zipwith(fun(A, B) -> A * B end, [], [], trim)),
    ?assertEqual([], lists:zipwith(fun(A, B) -> A * B end, [2], [], trim)),
    ?assertEqual([], lists:zipwith(fun(A, B) -> A * B end, [], [5], trim)),
    ?assertEqual([2 * 5], lists:zipwith(fun(A, B) -> A * B end, [2], [5], trim)),
    ?assertEqual([2 * 5], lists:zipwith(fun(A, B) -> A * B end, [2, 3], [5], trim)),
    ?assertEqual([2 * 5], lists:zipwith(fun(A, B) -> A * B end, [2], [5, 7], trim)),
    % zipwith/4 pad
    ?assertEqual([], lists:zipwith(fun(A, B) -> A * B end, [], [], {pad, {17, 19}})),
    ?assertEqual([2 * 19], lists:zipwith(fun(A, B) -> A * B end, [2], [], {pad, {17, 19}})),
    ?assertEqual([17 * 5], lists:zipwith(fun(A, B) -> A * B end, [], [5], {pad, {17, 19}})),
    ?assertEqual([2 * 5], lists:zipwith(fun(A, B) -> A * B end, [2], [5], {pad, {17, 19}})),
    ?assertEqual([2 * 5, 3 * 19], lists:zipwith(fun(A, B) -> A * B end, [2, 3], [5], {pad, {17, 19}})),
    ?assertEqual([2 * 5, 17 * 7], lists:zipwith(fun(A, B) -> A * B end, [2], [5, 7], {pad, {17, 19}})),
    % zipwith3/5 fail
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [11], fail)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5, 7], [11], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11, 13], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11, 13], fail)),
    ?assertError(function_clause, lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11, 13], fail)),
    % zipwith3/5 trim
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [], trim)),
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [], trim)),
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [], trim)),
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [11], trim)),
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [11], trim)),
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [11], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11, 13], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11, 13], trim)),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11, 13], trim)),
    % zipwith3/5 pad
    ?assertEqual([], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 19 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [], {pad, {17, 19, 23}})),
    ?assertEqual([17 * 5 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [], {pad, {17, 19, 23}})),
    ?assertEqual([17 * 19 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 19 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [], [11], {pad, {17, 19, 23}})),
    ?assertEqual([17 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [], [5], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 3 * 19 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 17 * 7 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 3 * 7 * 23], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5, 7], [11], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 17 * 19 * 13], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5], [11, 13], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 3 * 19 * 13], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2, 3], [5], [11, 13], {pad, {17, 19, 23}})),
    ?assertEqual([2 * 5 * 11, 17 * 7 * 13], lists:zipwith3(fun(A, B, C) -> A * B * C end, [2], [5, 7], [11, 13], {pad, {17, 19, 23}})),
    ok.

maps_test() ->
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

queue_test() ->
    ?assertEqual([11, 22 * 22, 33 * 33], queue:to_list(queue:filtermap(fun(X) when X < 17 -> true;
                                                                          (X) when X > 37 -> false;
                                                                          (X) -> {true, X*X}
                                                                       end,
                                                                       queue:from_list([11, 22, 33, 44])))),
    ?assertEqual([22 * 22, 33 * 33, 44],
                                         queue:to_list(queue:filtermap(fun(X) when X < 17 -> false;
                                                                          (X) when X > 37 -> true;
                                                                          (X) -> {true, X*X}
                                                                       end,
                                                                       queue:from_list([11, 22, 33, 44])))),
    L = lists:seq(1, 2 * 50),
    ?assertEqual(lists:sum(L), queue:fold(fun(X, A) -> X + A end, 0, queue:from_list(L))),
    ?assertEqual([X * X || X <- L], lists:reverse(queue:fold(fun(X, A) -> [X * X|A] end, [], queue:from_list(L)))),
    ?assertEqual(false, queue:any(fun(X) -> X > 9 end, queue:new())),
    ?assertEqual(true, queue:any(fun(X) -> X > 1 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(false, queue:any(fun(X) -> X < 1 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(true, queue:any(fun(X) -> X < 3 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(false, queue:any(fun(X) -> X > 3 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(true, queue:all(fun(X) -> X > 9 end, queue:new())),
    ?assertEqual(true, queue:all(fun(X) -> X >= 1 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(false, queue:all(fun(X) -> X>1 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(true, queue:all(fun(X) -> X=<3 end, queue:from_list([1, 2, 3]))),
    ?assertEqual(false, queue:all(fun(X) -> X<3 end, queue:from_list([1, 2, 3]))),
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
    ?assertEqual([], pm_fold(fun(M, A) -> [M|A] end, [], [])),
    ?assertEqual([[1]], lists:sort(pm_fold(fun(M, A) -> [M|A] end, [], [1]))),
    ?assertEqual(lists:sort([[1, 2], [2, 1]]), lists:sort(pm_fold(fun(M, A) -> [M|A] end, [], [1, 2]))),
    ?assertEqual(lists:sort([[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]),
                 lists:sort(pm_fold(fun(M, A) -> [M|A] end, [], [1, 2, 3]))),
    Stages = [{aliases, [{a, alias_a}]}, {negations, [{no_b, b}]}, {expand, [{c, [d]}]}],
    ?assertEqual(proplists:to_map([], Stages), #{}),
    ?assertEqual(proplists:to_map([], Stages), proplists:to_map(proplists:normalize([], Stages))),
    List = [a, no_b, c],
    ?assertEqual(proplists:to_map(List, Stages), #{alias_a => true, b => false, d => true}),
    ?assertEqual(proplists:to_map(List, Stages), proplists:to_map(proplists:normalize(List, Stages))),
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
    ?assertEqual(<<>>, binary:encode_hex(<<>>, uppercase)),
    ?assertEqual(<<"66">>, binary:encode_hex(<<"f">>, uppercase)),
    ?assertEqual(<<"666F">>, binary:encode_hex(<<"fo">>, uppercase)),
    ?assertEqual(<<"666F6F">>, binary:encode_hex(<<"foo">>, uppercase)),
    ?assertEqual(<<"666F6F62">>, binary:encode_hex(<<"foob">>, uppercase)),
    ?assertEqual(<<"666F6F6261">>, binary:encode_hex(<<"fooba">>, uppercase)),
    ?assertEqual(<<"666F6F626172">>, binary:encode_hex(<<"foobar">>, uppercase)),
    ?assertEqual(<<>>, binary:encode_hex(<<>>, lowercase)),
    ?assertEqual(<<"66">>, binary:encode_hex(<<"f">>, lowercase)),
    ?assertEqual(<<"666f">>, binary:encode_hex(<<"fo">>, lowercase)),
    ?assertEqual(<<"666f6f">>, binary:encode_hex(<<"foo">>, lowercase)),
    ?assertEqual(<<"666f6f62">>, binary:encode_hex(<<"foob">>, lowercase)),
    ?assertEqual(<<"666f6f6261">>, binary:encode_hex(<<"fooba">>, lowercase)),
    ?assertEqual(<<"666f6f626172">>, binary:encode_hex(<<"foobar">>, lowercase)),
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
    % join/2
    ?assertEqual(<<"a, b, c">>, binary:join([<<"a">>, <<"b">>, <<"c">>], <<", ">>)),
    ?assertEqual(<<"a">>, binary:join([<<"a">>], <<", ">>)),
    ?assertEqual(<<>>, binary:join([], <<", ">>)),
    ?assertError(badarg, maps:merge_with(not_a_fun, #{}, #{})),
    ?assertError(badarg, binary:join(<<"">>, ",")),
    ?assertError(badarg, binary:join([""], <<",">>)),
    ?assertError(badarg, binary:join([123], <<",">>)),
    ?assertError(badarg, binary:join(123, <<",">>)),
    ?assertError(badarg, binary:join(#{}, <<",">>)),
    ?assertError(badarg, binary:join(foo, <<",">>)),
    ok.

ets_test() ->
    TabSet = ets:new(foo, [set]),
    ets:insert(TabSet, {key, 42}),
    ?assertEqual(42, ets:lookup_element(TabSet, key, 2, 13)),
    ?assertEqual(13, ets:lookup_element(TabSet, not_key, 2, 13)),
    ?assertError(badarg, ets:lookup_element(TabSet, key, 3, 13)),
    ets:delete(TabSet),
    TabOrderedSet = ets:new(foo, [ordered_set]),
    ets:insert(TabOrderedSet, {key, 42}),
    ?assertEqual(42, ets:lookup_element(TabOrderedSet, key, 2, 13)),
    ?assertEqual(13, ets:lookup_element(TabOrderedSet, not_key, 2, 13)),
    ?assertError(badarg, ets:lookup_element(TabOrderedSet, key, 3, 13)),
    ets:delete(TabOrderedSet),
    TabBag = ets:new(foo, [bag]),
    ets:insert(TabBag, [{key, 42}, {key, 43, 44}]),
    ?assertEqual([42, 43], lists:sort(ets:lookup_element(TabBag, key, 2, 13))),
    ?assertEqual(13, ets:lookup_element(TabBag, not_key, 2, 13)),
    ?assertError(badarg, ets:lookup_element(TabBag, key, 3, 13)),
    ets:delete(TabBag),
    TabDuplicateBag = ets:new(foo, [duplicate_bag]),
    ets:insert(TabDuplicateBag, [{key, 42}, {key, 42},{key, 43, 44}]),
    ?assertEqual([42, 42, 43], lists:sort(ets:lookup_element(TabDuplicateBag, key, 2, 13))),
    ?assertEqual(13, ets:lookup_element(TabDuplicateBag, not_key, 2, 13)),
    ?assertError(badarg, ets:lookup_element(TabDuplicateBag, key, 3, 13)),
    ets:delete(TabDuplicateBag),
    ok.

math_test() ->
    % tau/0
    ?assertEqual(6.2831853071795864, math:tau()),
    ok.

timer_test() ->
    Self = self(),
    % tc/4
    ?assertEqual(ok, case timer:tc(timer, sleep, [500], millisecond) of
                         {Res, ok} when Res < 500 -> {too_early, Res};
                         {Res, ok} when Res > 800 -> {too_late, Res};
                         {_, ok} -> ok
                     end),
    ?assertEqual(ok, try timer:tc(erlang, exit, [foo], second)
                     catch exit:foo -> ok
                     end),
    ?assertMatch({_, Self}, timer:tc(erlang, self, [], second)),
    ok.

-ifndef(MODULE).
timer_apply_test() ->
    Self = self(),
    Msg = make_ref(),
    % apply_after/2,3
    ?assertMatch({ok, _}, timer:apply_after(0, fun erlang:send/2, [self(), {Msg, 2}])),
    ?assertMatch({ok, _}, timer:apply_after(0, fun() -> Self ! {Msg, 3} end)),
    ?assertEqual(ok, get_messes(200, Msg, [2, 3])),
    ?assertMatch({ok, _}, timer:apply_after(100, fun erlang:send/2, [self(), {Msg, 2}])),
    ?assertMatch({ok, _}, timer:apply_after(100, fun() -> Self ! {Msg, 3} end)),
    ?assertEqual(ok, get_messes(200, Msg, [2, 3])),
    ?assertEqual({error, badarg}, timer:apply_after(-1, fun() -> ok end)),
    ?assertEqual({error, badarg}, timer:apply_after(0, foo)),
    ?assertEqual({error, badarg}, timer:apply_after(0, fun(_X) -> ok end)),
    ?assertEqual({error, badarg}, timer:apply_after(-1, fun(_X) -> ok end, [foo])),
    ?assertEqual({error, badarg}, timer:apply_after(0, foo, [])),
    ?assertEqual({error, badarg}, timer:apply_after(0, fun(_X) -> ok end, [])),
    ?assertEqual({error, badarg}, timer:apply_after(0, fun(_X) -> ok end, [foo, bar])),
    ?assertEqual({error, badarg}, timer:apply_after(0, fun(_X) -> ok end, foo)),
    % apply_interval/2,3
    {ok, Ref21} = timer:apply_interval(100, fun erlang:send/2, [self(), {Msg, 2}]),
    {ok, Ref31} = timer:apply_interval(100, fun() -> Self ! {Msg, 3} end),
    ?assertEqual(ok, get_messes(400, Msg, [2, 3], 3)),
    ?assertEqual({ok, cancel}, timer:cancel(Ref21)),
    ?assertEqual({ok, cancel}, timer:cancel(Ref31)),
    ?assertEqual(nor, get_messes(200, Msg, [2, 3])),
    Fn = fun(P, Idx) ->
             P ! {Msg, Idx},
             receive after 200 -> ok end
         end,
    {ok, Ref22} = timer:apply_interval(100, Fn, [self(), 2]),
    {ok, Ref32} = timer:apply_interval(100, fun() -> Fn(Self, 3) end),
    receive after 400 -> ok end,
    ?assertEqual({ok, cancel}, timer:cancel(Ref22)),
    ?assertEqual({ok, cancel}, timer:cancel(Ref32)),
    ?assertEqual(ok, get_messes(200, Msg, [2, 3], 3)),
    ?assertEqual(nor, get_messes(200, Msg, [2, 3])),
    ?assertEqual({error, badarg}, timer:apply_interval(-1, fun() -> ok end)),
    ?assertEqual({error, badarg}, timer:apply_interval(0, foo)),
    ?assertEqual({error, badarg}, timer:apply_interval(0, fun(_X) -> ok end)),
    ?assertEqual({error, badarg}, timer:apply_interval(-1, fun(_X) -> ok end, [foo])),
    ?assertEqual({error, badarg}, timer:apply_interval(0, foo, [])),
    ?assertEqual({error, badarg}, timer:apply_interval(0, fun(_X) -> ok end, [])),
    ?assertEqual({error, badarg}, timer:apply_interval(0, fun(_X) -> ok end, [foo, bar])),
    ?assertEqual({error, badarg}, timer:apply_interval(0, fun(_X) -> ok end, foo)),
    ok.

get_messes(Time, Mess, Indexes) -> get_messes(Time, Mess, Indexes, 1).

get_messes(Time, Mess, Indexes, N) -> get_messes1(Time, Mess, lists:append(lists:duplicate(N, Indexes))).

get_messes1(_, _, []) -> ok;
get_messes1(Time, Mess, Indexes) ->
    receive
        {Mess, Index} -> get_messes1(Time, Mess, lists:delete(Index, Indexes))
    after Time -> nor
    end.
-endif.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
-define(NO_CALENDAR_TEST, true).
-endif.
-endif.
-ifndef(NO_CALENDAR_TEST).
calendar_test() ->
    ?assertEqual("1985-04-12T23:20:50.520Z", test_parse("1985-04-12T23:20:50.52Z", [{unit, millisecond}])),
    ?assertEqual("1985-04-12T23:20:50.520Z", test_parse("1985-04-12t23:20:50.52z", [{unit, millisecond}])),
    ?assertEqual("1985-04-12T21:20:50.520Z", test_parse("1985-04-12T23:20:50.52+02:00", [{unit, millisecond}])),
    ?assertEqual("1985-04-12T23:20:50Z", test_parse("1985-04-12T23:20:50.52Z", [{unit, second}])),
    ?assertEqual("1985-04-12T23:20:50.520Z", test_parse("1985-04-12T23:20:50.52Z", [{unit, millisecond}])),
    ?assertEqual("1985-04-12T23:20:50.520000Z", test_parse("1985-04-12t23:20:50.52z", [{unit, microsecond}])),
    ?assertEqual("1985-04-12 21:20:50.520000000Z",
                 test_parse("1985-04-12 23:20:50.52+02:00", [{unit, nanosecond}, {time_designator, $\s}])),
    ?assertEqual("1985-04-12T23:20:50Z", test_parse("1985-04-12T23:20:50.52Z")),
    ?assertEqual("1996-12-20T00:39:57Z", test_parse("1996-12-19T16:39:57-08:00")),
    ?assertEqual("1991-01-01T00:00:00Z", test_parse("1990-12-31T23:59:60Z")),
    ?assertEqual("1991-01-01T08:00:00Z", test_parse("1990-12-31T23:59:60-08:00")),
    ?assertEqual("1996-12-20T00:39:57Z", test_parse("1996-12-19T16:39:57-08:00")),
    %% The leap second is not handled:
    ?assertEqual("1991-01-01T00:00:00Z", test_parse("1990-12-31T23:59:60Z")),
    ?assertEqual("9999-12-31T23:59:59Z", calendar:system_time_to_rfc3339(253402300799, [{offset, "Z"}])),
    ?assertEqual("9999-12-31T23:59:59.999Z",
                 calendar:system_time_to_rfc3339(253402300799 * 1000 + 999, [{offset, "Z"}, {unit, millisecond}])),
    ?assertEqual("9999-12-31T23:59:59.999999Z",
                 calendar:system_time_to_rfc3339(253402300799 * 1000000 + 999999,
                                                 [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("9999-12-31T23:59:59.999999999Z",
                 calendar:system_time_to_rfc3339(253402300799 * 1000000000 + 999999999,
                                                 [{offset, "Z"}, {unit, nanosecond}])),
    ?assertError({badarg, [253402300799 + 1, [{offset, "Z"}]]},
                 calendar:system_time_to_rfc3339(253402300799 + 1, [{offset, "Z"}])),
    ?assertError({badarg, ["9999-12-31T23:59:60Z", []]}, calendar:rfc3339_to_system_time("9999-12-31T23:59:60Z", [])),
    ?assertError({badarg, [253402300799 * 1000000000 + 999999999 + 1, _]},
                 calendar:system_time_to_rfc3339(253402300799 * 1000000000 + 999999999 + 1,
                                                 [{offset, "Z"}, {unit, nanosecond}])),
    ?assertEqual(253402300799, calendar:rfc3339_to_system_time("9999-12-31T23:59:59Z", [])),
    ?assertEqual("0000-01-01T00:00:00Z", test_parse("0000-01-01T00:00:00.0+00:00")),
    ?assertEqual("9999-12-31T00:00:00Z", test_parse("9999-12-31T00:00:00.0+00:00")),
    ?assertEqual("1584-03-04T00:00:00Z", test_parse("1584-03-04T00:00:00.0+00:00")),
    ?assertEqual("1900-01-01T00:00:00Z", test_parse("1900-01-01T00:00:00.0+00:00")),
    ?assertEqual("2016-01-24T00:00:00Z", test_parse("2016-01-24T00:00:00.0+00:00")),
    ?assertEqual("1970-01-01T00:00:00Z", test_parse("1970-01-01T00:00:00Z")),
    ?assertEqual("1970-01-02T00:00:00Z", test_parse("1970-01-01T23:59:60Z")),
    ?assertEqual("1970-01-02T00:00:00Z", test_parse("1970-01-01T23:59:60.5Z")),
    ?assertEqual("1970-01-02T00:00:00Z", test_parse("1970-01-01T23:59:60.55Z")),
    ?assertEqual("1970-01-02T00:00:00.550Z", test_parse("1970-01-01T23:59:60.55Z", [{unit, millisecond}])),
    ?assertEqual("1970-01-02T00:00:00.550000Z", test_parse("1970-01-01T23:59:60.55Z", [{unit, microsecond}])),
    ?assertEqual("1970-01-02T00:00:00.550000000Z", test_parse("1970-01-01T23:59:60.55Z", [{unit, nanosecond}])),
    ?assertEqual("1970-01-02T00:00:00.999999Z", test_parse("1970-01-01T23:59:60.999999Z", [{unit, microsecond}])),
    ?assertEqual("1970-01-02T00:00:01.000Z", test_parse("1970-01-01T23:59:60.999999Z", [{unit, millisecond}])),
    ?assertEqual("1970-01-01T00:00:00Z", test_parse("1970-01-01T00:00:00+00:00")),
    ?assertEqual("1970-01-01T00:00:00Z", test_parse("1970-01-01T00:00:00-00:00")),
    ?assertEqual("1969-12-31T00:01:00Z", test_parse("1970-01-01T00:00:00+23:59")),
    ?assertEqual("1918-11-11T09:00:00.000000Z", test_parse("1918-11-11T11:00:00+02:00", [{unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:00.000001Z", test_parse("1970-01-01T00:00:00.000001Z", [{unit, microsecond}])),
    STS = erlang:system_time(second),
    ?assertEqual(STS, test_time(STS, [])),
    ?assertEqual(STS, test_time(STS, [{offset, "Z"}])),
    ?assertEqual(STS, test_time(STS, [{offset, "Z"}, {unit, second}])),
    ?assertEqual(STS, test_time(STS, [{offset, "+02:20"}])),
    STMs = erlang:system_time(millisecond),
    ?assertEqual(STMs, test_time(STMs, [{unit, millisecond}])),
    STUs = erlang:system_time(microsecond),
    ?assertEqual(STUs, test_time(STUs, [{unit, microsecond}, {offset, "-02:20"}])),
    TO = 946720800,
    ?assertEqual(TO, calendar:rfc3339_to_system_time("2000-01-01 10:00:00Z", [])),
    ?assertEqual("2000-01-01T10:02:00+00:02", calendar:system_time_to_rfc3339(TO, [{offset, 120}])),
    ?assertEqual("2000-01-01T10:02:00.000+00:02",
                 calendar:system_time_to_rfc3339(TO * 1000, [{offset, 120000}, {unit, millisecond}])),
    ?assertEqual("2000-01-01T10:02:00.000000+00:02",
                 calendar:system_time_to_rfc3339(TO * 1000000, [{offset, 120000000}, {unit, microsecond}])),
    ?assertEqual("2000-01-01T10:02:00.000000000+00:02",
                 calendar:system_time_to_rfc3339(TO * 1000000000, [{offset, 120000000000}, {unit, nanosecond}])),
    ?assertEqual("2000-01-01T09:58:00-00:02", calendar:system_time_to_rfc3339(TO, [{offset, -120}])),
    ?assertEqual("2000-01-01T09:58:00.000-00:02",
                 calendar:system_time_to_rfc3339(TO * 1000, [{offset, -120000}, {unit, millisecond}])),
    ?assertEqual("2000-01-01T09:58:00.000000-00:02",
                 calendar:system_time_to_rfc3339(TO * 1000000, [{offset, -120000000}, {unit, microsecond}])),
    ?assertEqual("2000-01-01T09:58:00.000000000-00:02",
                 calendar:system_time_to_rfc3339(TO * 1000000000, [{offset, -120000000000}, {unit, nanosecond}])),
    ?assertEqual("2000-01-01T09:58:00.000-00:02",
                 calendar:system_time_to_rfc3339(TO * 1000, [{offset, -120000}, {unit, millisecond}])),
    ?assertEqual(543210000, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.54321Z", [{unit, nanosecond}])),
    ?assertEqual(54321000, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.054321Z", [{unit, nanosecond}])),
    ?assertEqual(543210, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.54321Z", [{unit, microsecond}])),
    ?assertEqual(543, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.54321Z", [{unit, millisecond}])),
    ?assertEqual(0, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.000001Z", [{unit, millisecond}])),
    ?assertEqual(1, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.000001Z", [{unit, microsecond}])),
    ?assertEqual(1000, calendar:rfc3339_to_system_time("1970-01-01T00:00:00.000001Z", [{unit, nanosecond}])),
    ?assertEqual(0, calendar:rfc3339_to_system_time("1970-01-01Q00:00:00.00049Z", [{unit, millisecond}])),
    ?assertEqual(1, calendar:rfc3339_to_system_time("1970-01-01Q00:00:00.0005Z", [{unit, millisecond}])),
    ?assertEqual(6543210, calendar:rfc3339_to_system_time("1970-01-01T00:00:06.54321Z", [{unit, microsecond}])),
    ?assertEqual(298815132000000, calendar:rfc3339_to_system_time("1979-06-21T12:12:12Z", [{unit, microsecond}])),
    ?assertEqual(-1613826000000000, calendar:rfc3339_to_system_time("1918-11-11T11:00:00Z", [{unit, microsecond}])),
    ?assertEqual(-1613833200000000,
                 calendar:rfc3339_to_system_time("1918-11-11T11:00:00+02:00", [{unit, microsecond}])),
    ?assertEqual(-1613833200000000, calendar:rfc3339_to_system_time("1918-11-11T09:00:00Z", [{unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:00.000000Z",
                 calendar:system_time_to_rfc3339(0, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:01Z", calendar:system_time_to_rfc3339(1, [{offset, "Z"}, {unit, second}])),
    ?assertEqual("1970-01-01T00:00:00.001Z", calendar:system_time_to_rfc3339(1, [{offset, "Z"}, {unit, millisecond}])),
    ?assertEqual("1970-01-01T00:00:00.000001Z",
                 calendar:system_time_to_rfc3339(1, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:00.000000001Z",
                 calendar:system_time_to_rfc3339(1, [{offset, "Z"}, {unit, nanosecond}])),
    ?assertEqual("1970-01-01T00:00:01.000000Z",
                 calendar:system_time_to_rfc3339(1000000, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:00.543210Z",
                 calendar:system_time_to_rfc3339(543210, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1970-01-01T00:00:00.543Z",
                 calendar:system_time_to_rfc3339(543, [{offset, "Z"}, {unit, millisecond}])),
    ?assertEqual("1970-01-01T00:00:00.543210000Z",
                 calendar:system_time_to_rfc3339(543210000, [{offset, "Z"}, {unit, nanosecond}])),
    ?assertEqual("1970-01-01T00:00:06.543210Z",
                 calendar:system_time_to_rfc3339(6543210, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1979-06-21T12:12:12.000000Z",
                 calendar:system_time_to_rfc3339(298815132000000, [{offset, "Z"}, {unit, microsecond}])),
    ?assertEqual("1918-11-11T13:00:00.000000Z",
                 calendar:system_time_to_rfc3339(-1613818800000000, [{offset, "Z"}, {unit, microsecond}])),
    ok.

test_parse(String) -> test_parse(String, []).

test_parse(String, Options) -> calendar:system_time_to_rfc3339(calendar:rfc3339_to_system_time(String, Options), [{offset, "Z"}|Options]).

test_time(Time, Options) -> calendar:rfc3339_to_system_time(calendar:system_time_to_rfc3339(Time, Options), Options).
-endif.
