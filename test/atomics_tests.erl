-module(atomics_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

%% Test based on erts/emulator/test/atomics_SUITE.erl

bad_test() ->
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:new(0, [])),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:new(10, [bad])),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:new(10, [{signed, bad}])),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:new(10, [{signed, true}, bad])),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:new(10, [{signed, false}|bad])),
    Ref = atomics:new(10, []),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(1742, 7)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(<<>>, 7)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(Ref, -1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(Ref, 0)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(Ref, 11)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:get(Ref, 7.0)).

signed_test() ->
    Size = 10,
    Ref = atomics:new(Size, []),
    Info = atomics:info(Ref),
    ?assertEqual(Size, maps:get(size, Info)),
    Memory = maps:get(memory, Info),
    ?assert(Memory > Size * 8),
    ?assert(Memory < Size * 8 + 100),
    signed_do(Ref, Size).

signed_do(_Ref, 0) -> ok;
signed_do(Ref, Ix) ->
    ?assertEqual(0, atomics:get(Ref, Ix)),
    ?assertEqual(ok, atomics:put(Ref, Ix, 3)),
    ?assertEqual(ok, atomics:add(Ref, Ix, 14)),
    ?assertEqual(17, atomics:get(Ref, Ix)),
    ?assertEqual(20, atomics:add_get(Ref, Ix, 3)),
    ?assertEqual(-3, atomics:add_get(Ref, Ix, -23)),
    ?assertEqual(17, atomics:add_get(Ref, Ix, 20)),
    ?assertEqual(ok, atomics:sub(Ref, Ix, 4)),
    ?assertEqual(13, atomics:get(Ref, Ix)),
    ?assertEqual(-7, atomics:sub_get(Ref, Ix, 20)),
    ?assertEqual(3, atomics:sub_get(Ref, Ix, -10)),
    ?assertEqual(3, atomics:exchange(Ref, Ix, 666)),
    ?assertEqual(ok, atomics:compare_exchange(Ref, Ix, 666, 777)),
    ?assertEqual(777, atomics:compare_exchange(Ref, Ix, 666, -666)),
    signed_do(Ref, Ix - 1).

unsigned_test() ->
    Size = 10,
    Ref = atomics:new(Size, [{signed, false}]),
    Info = atomics:info(Ref),
    ?assertEqual(Size, maps:get(size, Info)),
    Memory = maps:get(memory, Info),
    ?assert(Memory > Size * 8),
    ?assert(Memory < Size * 8 + 100),
    unsigned_do(Ref, Size).

unsigned_do(_Ref, 0) -> ok;
unsigned_do(Ref, Ix) ->
    ?assertEqual(0, atomics:get(Ref, Ix)),
    ?assertEqual(ok, atomics:put(Ref, Ix, 3)),
    ?assertEqual(ok, atomics:add(Ref, Ix, 14)),
    ?assertEqual(17, atomics:get(Ref, Ix)),
    ?assertEqual(20, atomics:add_get(Ref, Ix, 3)),
    ?assertEqual(ok, atomics:sub(Ref, Ix, 7)),
    ?assertEqual(13, atomics:get(Ref, Ix)),
    ?assertEqual(3, atomics:sub_get(Ref, Ix, 10)),
    ?assertEqual(3, atomics:exchange(Ref, Ix, 666)),
    ?assertEqual(ok, atomics:compare_exchange(Ref, Ix, 666, 777)),
    ?assertEqual(777, atomics:compare_exchange(Ref, Ix, 666, 888)),
    unsigned_do(Ref, Ix - 1).

unsigned_limits_test() ->
    Bits = 64,
    Max = (1 bsl Bits) - 1,
    Min = 0,
    Ref = atomics:new(1, [{signed, false}]),
    Info = atomics:info(Ref),
    ?assertEqual(Max, maps:get(max, Info)),
    ?assertEqual(Min, maps:get(min, Info)),
    ?assertEqual(0, atomics:get(Ref, 1)),
    ?assertEqual(ok, atomics:add(Ref, 1, Max)),
    ?assertEqual(Min, atomics:add_get(Ref, 1, 1)),
    ?assertEqual(Max, atomics:sub_get(Ref, 1, 1)),
    atomics:put(Ref, 1, Max),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:add(Ref, 1, Max + 1)),
    IncrMin = -(1 bsl (Bits-1)),
    ?assertEqual(ok, atomics:put(Ref, 1, -IncrMin)),
    ?assertEqual(ok, atomics:add(Ref, 1, IncrMin)),
    ?assertEqual(0, atomics:get(Ref, 1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:add(Ref, 1, IncrMin - 1)).

signed_limits_test() ->
    Bits = 64,
    Max = (1 bsl (Bits - 1)) - 1,
    Min = -(1 bsl (Bits - 1)),
    Ref = atomics:new(1, [{signed, true}]),
    Info = atomics:info(Ref),
    ?assertEqual(Max, maps:get(max, Info)),
    ?assertEqual(Min, maps:get(min, Info)),
    ?assertEqual(0, atomics:get(Ref, 1)),
    ?assertEqual(ok, atomics:add(Ref, 1, Max)),
    ?assertEqual(Min, atomics:add_get(Ref, 1, 1)),
    ?assertEqual(Max, atomics:sub_get(Ref, 1, 1)),
    IncrMax = (Max bsl 1) bor 1,
    ?assertEqual(ok, atomics:put(Ref, 1, 0)),
    ?assertEqual(ok, atomics:add(Ref, 1, IncrMax)),
    ?assertEqual(-1, atomics:get(Ref, 1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:add(Ref, 1, IncrMax + 1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch atomics:add(Ref, 1, Min - 1)).
