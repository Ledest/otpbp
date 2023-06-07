-module(otpbp_gb_sets).

-ifndef(HAVE_gb_sets__map_2).
% OTP 26.1
-export([map/2]).
-endif.
-ifndef(HAVE_gb_sets__filtermap_2).
% OTP 26.1
-export([filtermap/2]).
-endif.

-ifndef(HAVE_gb_sets__map_2).
map(F, {_, T}) when is_function(F, 1) -> gb_sets:from_list(map(T, F, [])).

map({Key, Small, Big}, F, L) -> map(Small, F, [F(Key)|map(Big, F, L)]);
map(nil, _F, L) -> L.
-endif.

-ifndef(HAVE_gb_sets__filtermap_2).
filtermap(F, {_, T}) when is_function(F, 1) -> gb_sets:from_list(filtermap(T, F, [])).

filtermap({Key, Small, Big}, F, L) ->
    N = filtermap(Big, F, L),
    filtermap(Small, F,
              case F(Key) of
                  true -> [Key|N];
                  {true, Val} -> [Val|N];
                  false -> N
              end);
filtermap(nil, _F, L) -> L.
-endif.
