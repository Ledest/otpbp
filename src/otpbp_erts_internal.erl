-module(otpbp_erts_internal).

-ifndef(HAVE_erts_internal__map_next_3).
% OTP 21.0
-export([map_next/3]).
-endif.

-ifndef(HAVE_erts_internal__map_next_3).
% OTP 21.0
map_next(0, M, iterator) when is_map(M) -> lists:foldr(fun({K, V}, A) -> {K, V, A} end, none, maps:to_list(M));
map_next(0, M, L) when is_list(L) -> lists:reverse(maps:fold(fun(K, V, A) -> [{K, V}|A] end, [], M), L).
-endif.
