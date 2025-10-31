-module(array_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

-define(LEAFSIZE, 10).
-define(NODESIZE, ?LEAFSIZE).

from_test() ->
    Seq = fun({N, Max}) when N =< Max -> {N, {N + 1, Max}};
             ({_, _}) -> done
          end,
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE * N0,
    N2 = ?NODESIZE * N1,
    N3 = ?NODESIZE * N2,
    N4 = ?NODESIZE * N3,
    ?assertEqual(0, array:size(array:from(Seq, {1, 0}))),
    ?assertEqual(false, array:is_fix(array:from(Seq, {1, 0}))),
    ?assertEqual(1, array:size(array:from(Seq, {1, 1}))),
    ?assertEqual(false, array:is_fix(array:from(Seq, {1, 1}))),
    ?assertEqual(lists:seq(1, N0 - 1), array:to_list(array:from(Seq, {1, N0 - 1}))),
    ?assertEqual(lists:seq(1, N0), array:to_list(array:from(Seq, {1, N0}))),
    ?assertEqual(lists:seq(1, N0 + 1), array:to_list(array:from(Seq, {1, N0 + 1}))),
    ?assertEqual(lists:seq(1, N0 + 2), array:to_list(array:from(Seq, {1, N0 + 2}))),
    ?assertEqual(lists:seq(1, N2 - 1), array:to_list(array:from(Seq, {1, N2 - 1}))),
    ?assertEqual(lists:seq(1, N2), array:to_list(array:from(Seq, {1, N2}))),
    ?assertEqual(lists:seq(1, N2 + 1), array:to_list(array:from(Seq, {1, N2 + 1}))),
    ?assertEqual(lists:seq(0, N3), array:to_list(array:from(Seq, {0, N3}))),
    ?assertEqual(lists:seq(0, N4), array:to_list(array:from(Seq, {0, N4}))),
    ?assertEqual(N1, array:size(array:from(Seq, {1, N1}))),
    ?assertError(badarg, array:from(fun(A) -> A end, foo)),
    ?assertError(badarg, array:from(no_fun, foo)),
    ok.

import_export_test() ->
    %% Some examples of usages
    FloatBin = <<<<N:32/float-native>> || N <- lists:seq(1, 20000)>>,
    ?assertEqual(FloatBin,
                 array:foldl(fun(_K, V, Acc) when is_binary(Acc) -> <<Acc/binary, V:32/float-native>> end,
                             <<>>,
                             array:from(fun(<<N:32/float-native, Rest/binary>>) -> {N, Rest};
                                           (<<>>) -> done
                                        end,
                                        FloatBin))),
    RGBBin = <<<<N:8, N:8, N:8>> || N <- lists:seq(1, 256)>>,
    ?assertEqual(RGBBin,
                 array:foldl(fun(_K, {R,G,B}, Acc) -> <<Acc/binary, R:8, G:8, B:8>> end,
                             <<>>,
                             array:from(fun(<<R:8,G:8,B:8, Rest/binary>>) -> {{R,G,B}, Rest};
                                           (<<>>) -> done
                                        end,
                                        RGBBin))),
    ok.
