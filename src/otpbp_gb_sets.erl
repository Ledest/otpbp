-module(otpbp_gb_sets).

-ifndef(HAVE_gb_sets__map_2).
% OTP 27.0
-export([map/2]).
-endif.
-ifndef(HAVE_gb_sets__filtermap_2).
% OTP 27.0
-export([filtermap/2]).
-endif.
-ifndef(HAVE_gb_sets__is_equal_2).
% OTP 27.0
-export([is_equal/2]).
-endif.
-ifndef(HAVE_gb_sets__iterator_2).
% OTP 27.0
-export([iterator/2]).
-endif.
-ifndef(HAVE_gb_sets__iterator_from_3).
% OTP 27.0
-export([iterator_from/3]).
-endif.
-ifndef(HAVE_gb_sets__larger_2).
% OTP 27.0
-export([larger/2]).
-endif.
-ifndef(HAVE_gb_sets__smaller_2).
% OTP 27.0
-export([smaller/2]).
-endif.
-ifndef(HAVE_gb_sets__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_gb_sets__map_2).
map(F, {_, T}) when is_function(F, 1) -> gb_sets:from_list(map(T, F, [])).

map({K, S, B}, F, L) -> map(S, F, [F(K)|map(B, F, L)]);
map(nil, _F, L) -> L.
-endif.

-ifndef(HAVE_gb_sets__filtermap_2).
filtermap(F, {_, T}) when is_function(F, 1) -> gb_sets:from_list(filtermap(T, F, [])).

filtermap({K, S, B}, F, L) ->
    N = filtermap(B, F, L),
    filtermap(S, F,
              case F(K) of
                  true -> [K|N];
                  {true, V} -> [V|N];
                  false -> N
              end);
filtermap(nil, _F, L) -> L.
-endif.

-ifndef(HAVE_gb_sets__is_equal_2).
is_equal(S1, S2) ->
    gb_sets:is_set(S1) andalso gb_sets:is_set(S2) orelse error(function_clause, [S1, S2]),
    S1 == S2 orelse gb_sets:size(S1) =:= gb_sets:size(S2) andalso gb_sets:is_subset(S1, S2).
-endif.

-ifndef(HAVE_gb_sets__iterator_2).
iterator(S, ordered) -> gb_sets:iterator(S);
iterator({_, T}, reversed) -> {reversed, iterator_r(T, [])}.

iterator_r({_, _, nil} = T, L) -> [T|L];
iterator_r({_, _, R} = T, L) -> iterator_r(R, [T|L]);
iterator_r(nil, L) -> L.
-endif.

-ifndef(HAVE_gb_sets__iterator_from_3).
iterator_from(E, S, ordered) -> gb_sets:iterator_from(E, S);
iterator_from(E, {_, T}, reversed) -> {reversed, iterator_from_r(E, T, [])}.

iterator_from_r(E, {K, T, _}, L) when K > E -> iterator_from_r(E, T, L);
iterator_from_r(_, {_, _, nil} = T, L) -> [T|L];
iterator_from_r(E, {_, _, R} = T, L) -> iterator_from_r(E, R, [T|L]);
iterator_from_r(_, nil, L) -> L.
-endif.

-ifndef(HAVE_gb_sets__larger_2).
larger(K, {_, T}) -> larger_(K, T).

larger_(_K, nil) -> none;
larger_(K, {K1, S, _L}) when K < K1 ->
    case larger_(K, S) of
        none -> {found, K1};
        F -> F
    end;
larger_(K, {_K, _S, L}) -> larger_(K, L).
-endif.

-ifndef(HAVE_gb_sets__smaller_2).
smaller(K, {_, T}) -> smaller_(K, T).

smaller_(_K, nil) -> none;
smaller_(K, {K1, _S, L}) when K > K1 ->
    case smaller_(K, L) of
        none -> {found, K1};
        F -> F
    end;
smaller_(K, {_K, S, _L}) -> smaller_(K, S).
-endif.

-ifndef(HAVE_sets__foreach_2).
foreach(F, S) when is_function(F, 1) ->
    gb_sets:fold(fun(E, _) ->
                     F(E),
                     ok
                 end,
                 ok, S).
-endif.
