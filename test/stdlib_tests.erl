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
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, []), false),
    ?assertEqual(lists:search(fun(E) -> E rem 2 =:= 0 end, [1,2,3,4,5,6,7,8]), {value, 2}).

maps_test() ->
    % update_with/3
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    Map = #{ key1 => V1, key2 => V2, "key3" => V3 },
    Fun = fun(V) -> [V,V,{V,V}] end,
    ?assertMatch(#{key1 := [V1,V1,{V1,V1}]}, maps:update_with(key1,Fun,Map)),
    ?assertMatch(#{key2 := [V2,V2,{V2,V2}]}, maps:update_with(key2,Fun,Map)),
    ?assertMatch(#{"key3" := [V3,V3,{V3,V3}]}, maps:update_with("key3",Fun,Map)),
    ?assertError({badmap,b}, maps:update_with([a,b],a,b)),
    ?assertError(badarg, maps:update_with([a,b],a,#{})),
    ?assertError({badkey,[a,b]}, maps:update_with([a,b],Fun,#{})),
    % update_with/4
    Init = 3,
    ?assertMatch(#{key1 := [V1,V1,{V1,V1}]}, maps:update_with(key1,Fun,Init,Map)),
    ?assertMatch(#{key2 := [V2,V2,{V2,V2}]}, maps:update_with(key2,Fun,Init,Map)),
    ?assertMatch(#{"key3" := [V3,V3,{V3,V3}]}, maps:update_with("key3",Fun,Init,Map)),
    ?assertMatch(#{key3 := Init}, maps:update_with(key3,Fun,Init,Map)),
    ?assertError({badmap,b}, maps:update_with([a,b],a,Init,b)),
    ?assertError(badarg, maps:update_with([a,b],a,Init,#{})),
    % Disabled for Erlang/OTP < 18
    %?assertError({badmap,a}, maps:size(a)),
    %?assertError({badmap,<<>>}, maps:size(<<>>)),
    ok.

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
    ok.
