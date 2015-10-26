-module(otpbp_gb_sets).

-ifndef(HAVE_gb_sets__iterator_from_2).
-export([iterator_from/2]).
-endif.

-ifndef(HAVE_gb_sets__iterator_from_2).
iterator_from(S, {_, T}) -> iterator_from(S, T, []).

iterator_from(S, {K, _, T}, As) when K < S -> iterator_from(S, T, As);
iterator_from(_, {_, nil, _} = T, As) -> [T|As];
iterator_from(S, {_, L, _} = T, As) -> iterator_from(S, L, [T|As]);
iterator_from(_, nil, As) -> As.
-endif.
