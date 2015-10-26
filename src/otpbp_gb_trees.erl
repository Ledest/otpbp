-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__iterator_from_2).
-export([iterator_from/2]).
-endif.

-ifndef(HAVE_gb_trees__iterator_from_2).
iterator_from(S, {_, T}) ->  iterator_from(S, T, []).

iterator_from(S, {K, _, _, T}, As) when K < S -> iterator_from(S, T, As);
iterator_from(_, {_, _, nil, _} = T, As) -> [T|As];
iterator_from(S, {_, _, L, _} = T, As) -> iterator_from(S, L, [T|As]);
iterator_from(_, nil, As) -> As.
-endif.
