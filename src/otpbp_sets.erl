-module(otpbp_sets).

-ifndef(HAVE_sets__is_empty_1).
% OTP 21.0
-export([is_empty/1]).
-endif.
-ifndef(HAVE_sets__from_list_2).
% OTP 24.0
-export([from_list/2]).
-endif.
-ifndef(HAVE_sets__new_1).
% OTP 24.0
-export([new/1]).
-endif.
-ifndef(HAVE_sets__map_2).
% OTP 26.1
-export([map/2]).
-endif.
-ifndef(HAVE_sets__filtermap_2).
% OTP 26.1
-export([filtermap/2]).
-endif.

-ifndef(HAVE_sets__is_empty_1).
is_empty(S) -> sets:size(S) =:= 0.
-endif.

-ifndef(HAVE_sets__from_list_2).
from_list(List, [{version, 1}]) -> sets:from_list(List).
-endif.

-ifndef(HAVE_sets__new_1).
new([{version, 1}]) -> sets:new().
-endif.

-ifndef(HAVE_sets__map_2).
map(F, S) when is_function(F, 1) ->
    sets:is_set(S) orelse error(function_clause, [F, S]),
    sets:fold(fun(E, Acc) -> sets:add_element(F(E), Acc) end, sets:new(), S).
-endif.

-ifndef(HAVE_sets__filtermap_2).
filtermap(F, S) when is_function(F, 1) ->
    sets:is_set(S) orelse error(function_clause, [F, S]),
    sets:fold(fun(E, A) ->
                  case F(E) of
                      true -> sets:add_element(E, A);
                      {true, E1} -> sets:add_element(E1, A);
                      false -> A
                  end
              end, sets:new(), S).
-endif.
