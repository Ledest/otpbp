-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__iterator_from_2).
-export([iterator_from/2]).
-endif.

-ifndef(HAVE_gb_trees__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_gb_trees__take_any_2).
-export([take_any/2]).
-endif.

-ifndef(HAVE_gb_trees__iterator_from_2).
iterator_from(S, {_, T}) ->  iterator_from(S, T, []).

iterator_from(S, {K, _, _, T}, As) when K < S -> iterator_from(S, T, As);
iterator_from(_, {_, _, nil, _} = T, As) -> [T|As];
iterator_from(S, {_, _, L, _} = T, As) -> iterator_from(S, L, [T|As]);
iterator_from(_, nil, As) -> As.
-endif.

-ifndef(HAVE_gb_trees__take_2).
take(Key, Tree) -> {gb_trees:get(Key, Tree), gb_trees:delete(Key, Tree)}.
-endif.

-ifndef(HAVE_gb_trees__take_any_2).
take_any(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, V} -> {V, gb_trees:delete(Key, Tree)};
        none -> error
    end.
-endif.
