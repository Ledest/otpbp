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
    ?assertEqual(lists:droplast([1,2,3]), [1,2]),
    ?assertEqual(lists:droplast([1]), []),
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, []), false),
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, [1,2,3,4,5,6,7,8]), {value, 2}).

edlin_test() ->
    ?assertEqual(edlin:current_chars({line, 1, {"abcd", "xy"}, test}), "dcbaxy").
