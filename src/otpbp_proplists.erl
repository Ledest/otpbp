-module(otpbp_proplists).

-ifndef(HAVE_proplists__to_map_1).
% OTP 24.0
-export([to_map/1]).
-endif.
-ifndef(HAVE_proplists__to_map_2).
% OTP 24.0
-export([to_map/2]).
-ifdef(HAVE_proplists__to_map_1).
-import(proplists, [to_map/1]).
-endif.
-endif.

-ifndef(HAVE_proplists__to_map_1).
-ifdef(HAVE_MAP_SYNTAX_6).
-define(PUT(K, V, M), maps:put(K, V, M)).
-else.
-define(PUT(K, V, M), M#{K => V}).
-endif.
to_map(List) ->
    lists:foldr(fun({K, V}, M) -> ?PUT(K, V, M);
                   % if tuples with arity /= 2 appear before atoms or tuples with arity == 2, get_value/2,3 returns early
                   (T, M) when tuple_size(T) =/= 0 -> maps:remove(element(1, T), M);
                   (K, M) when is_atom(K) -> ?PUT(K, true, M);
                   (_, M) -> M
                end, #{}, List).
-endif.

-ifndef(HAVE_proplists__to_map_2).
to_map(List, Stages) -> to_map(apply_stages(List, Stages)).

apply_stages(L, [{aliases, As}|Xs]) -> apply_stages(proplists:substitute_aliases(As, L), Xs);
apply_stages(L, [{expand, Es}|Xs]) -> apply_stages(proplists:expand(Es, L), Xs);
apply_stages(L, [{negations, Ns}|Xs]) -> apply_stages(proplists:substitute_negations(Ns, L), Xs);
apply_stages(L, []) -> L.
-endif.
