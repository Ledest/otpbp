-module(otpbp_maps).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_maps__update_with_3).
% OTP 19.0
-export([update_with/3]).
-endif.
-ifndef(HAVE_maps__update_with_4).
% OTP 19.0
-export([update_with/4]).
-endif.
-ifndef(HAVE_maps__take_2).
% OTP 19.0
-export([take/2]).
-endif.
-ifndef(HAVE_maps__iterator_1).
% OTP 21.0
-export([iterator/1]).
-endif.
-ifndef(HAVE_maps__next_1).
% OTP 21.0
-export([next/1]).
-endif.
-ifndef(HAVE_maps__merge_with_3).
% OTP 24.0
-export([merge_with/3]).
-endif.
-ifndef(HAVE_maps__intersect_with_3).
% OTP 24.0
-export([intersect_with/3]).
-endif.
-ifndef(HAVE_maps__intersect_2).
% OTP 24.0
-export([intersect/2]).
-endif.
-ifndef(HAVE_maps__filtermap_2).
% OTP 24.0
-export([filtermap/2]).
-endif.
-ifndef(HAVE_maps__from_keys_2).
% OTP 24.0
-export([from_keys/2]).
-endif.

-ifdef(HAVE_MAP_SYNTAX_6).
-define(PUT(K, V, M), maps:put(K, V, M)).
-define(UPDATE(K, V, M), maps:update(K, V, M)).
-define(UPDATE_WITH(K, F, I, M, V),
        case maps:find(K, M) of
            {ok, V} -> maps:update(K, F(V), M);
            error -> maps:put(K, I, M)
        end).
-define(UPDATE_WITH(K, F, M, V),
        case maps:find(K, M) of
            {ok, V} -> maps:update(K, F(V), M);
            error -> error({badkey, K}, [K, F, M])
        end).
-else.
-define(PUT(K, V, M), M#{K => V}).
-define(UPDATE(K, V, M), M#{K := V}).
-define(UPDATE_WITH(K, F, I, M, V),
        case M of
            #{K := V} -> maps:update(K, F(V), M);
            #{} -> M#{K => I}
        end).
-define(UPDATE_WITH(K, F, M, V),
        case M of
            #{K := V} -> maps:update(K, F(V), M);
            #{} -> error({badkey, K}, [K, F, M])
        end).
-endif.

-ifndef(HAVE_maps__update_with_4).
update_with(K, Fun, Init, M) ->
    is_map(M) orelse error({badmap, M}, [K, Fun, Init, M]),
    is_function(Fun, 1) orelse error(badarg, [K, Fun, Init, M]),
    ?UPDATE_WITH(K, Fun, Init, M, V).
-endif.

-ifndef(HAVE_maps__update_with_3).
update_with(K, Fun, M) ->
    is_map(M) orelse error({badmap, M}, [K, Fun, M]),
    is_function(Fun, 1) orelse error(badarg, [K, Fun, M]),
    ?UPDATE_WITH(K, Fun, M, V).
-endif.

-ifndef(HAVE_maps__take_2).
-ifdef(HAVE_MAP_SYNTAX_6).
take(K, M) ->
    is_map(M) orelse error({badmap, M}, [K, M]),
    case maps:find(K, M) of
        {ok, V} -> {V, maps:remove(K, M)};
        error -> error
    end.
-else.
take(K, M) ->
    case M of
        #{K := V} -> {V, maps:remove(K, M)};
        #{} -> error;
        _ -> error({badmap, M}, [K, M])
    end.
-endif.
-endif.

-ifndef(HAVE_maps__iterator_1).
iterator(M) when is_map(M) -> [0|M];
iterator(M) -> error({badmap, M}, [M]).
-endif.

-ifndef(HAVE_maps__next_1).
next({_K, _V, _I} = KVI) -> KVI;
next([0|Map]) when is_map(Map) -> erts_internal:map_next(0, Map, iterator);
next(none) -> none;
next(Iter) -> error(badarg, [Iter]).
-endif.

-ifndef(HAVE_maps__merge_with_3).
merge_with(C, M1, M2) ->
    is_function(C, 3) orelse error(badarg, [C, M1, M2]),
    is_map(M1) orelse error({badmap, M1}, [C, M1, M2]),
    is_map(M2) orelse error({badmap, M2}, [C, M1, M2]),
    maps:fold(fun(K, V2, A) -> maps:update_with(K, fun(V1) -> C(K, V1, V2) end, V2, A) end, M1, M2).
-endif.

-ifndef(HAVE_maps__intersect_with_3).
-ifdef(HAVE_MAP_SYNTAX_6).
intersect_with(C, M1, M2) ->
    is_function(C, 3) orelse error(badarg, [C, M1, M2]),
    is_map(M1) orelse error({badmap, M1}, [C, M1, M2]),
    is_map(M2) orelse error({badmap, M2}, [C, M1, M2]),
    maps:fold(fun(K, V2, A) ->
                  case maps:find(K, M1) of
                      {ok, V1} -> maps:put(K, C(K, V1, V2), A);
                      _error -> maps:remove(K, A)
                  end
              end,
              M2, M2).
-else.
intersect_with(C, M1, M2) ->
    is_function(C, 3) orelse error(badarg, [C, M1, M2]),
    is_map(M1) orelse error({badmap, M1}, [C, M1, M2]),
    is_map(M2) orelse error({badmap, M2}, [C, M1, M2]),
    maps:fold(fun(K, V2, A) ->
                  case M1 of
                      #{K := V1} -> A#{K := C(K, V1, V2)};
                      _ -> maps:remove(K, A)
                  end
              end,
              M2, M2).
-endif.
-endif.

-ifndef(HAVE_maps__intersect_2).
intersect(M1, M2) ->
    is_map(M1) orelse error({badmap, M1}, [M1, M2]),
    is_map(M2) orelse error({badmap, M2}, [M1, M2]),
    maps:with(maps:keys(M1), M2).
-endif.

-ifndef(HAVE_maps__filtermap_2).
-ifdef(HAVE_maps__iterator_1).
filtermap(Fun, Map) when is_function(Fun, 2) ->
    case Map of
        #{} -> filter_map(Fun, Map);
        [0|M] when is_map(M) -> filter_map(Fun, M);
        [I|M] when is_integer(I), is_map(M) -> filter_iterator(Fun, Map);
        {_, _, _} -> filter_iterator(Fun, Map);
        none -> #{};
        _ -> error({badmap, Map}, [Fun, Map])
    end;
filtermap(Fun, Map) -> error(badarg, [Fun, Map]).

filter_iterator(Pred, Iter) ->
    maps:from_list(maps:fold(fun(K, V, A) ->
                                 case Pred(K, V) of
                                     true -> [{K, V}|A];
                                     {true, NewV} -> [{K, NewV}|A];
                                     false -> A
                                 end
                             end, [], Iter)).
-else.
filtermap(Fun, Map) when is_function(Fun, 2) ->
    case Map of
        #{} -> filter_map(Fun, Map);
        [0|M] when is_map(M) -> filter_map(Fun, M);
        none -> #{};
        _ -> error({badmap, Map}, [Fun, Map])
    end;
filtermap(Fun, Map) -> error(badarg, [Fun, Map]).
-endif.
filter_map(Pred, Map) ->
    maps:fold(fun(K, V, A) ->
                  case Pred(K, V) of
                      true -> A;
                      {true, NewV} -> ?UPDATE(K, NewV, A);
                      false -> maps:remove(K, A)
                  end
              end, Map, Map).
-endif.
-ifndef(HAVE_maps__from_keys_2).
from_keys(Keys, Value) when is_list(Keys) ->
    try [{Key, Value} || Key <- Keys] of
        L -> maps:from_list(L)
    catch
        error:_ -> error(badarg, [Keys, Value]);
        C:R -> erlang:C(R)
    end;
from_keys(Keys, Value) -> error(badarg, [Keys, Value]).
-endif.
