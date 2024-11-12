-module(otpbp_erl_anno).

-ifndef(HAVE_erl_anno__set_end_location_2).
% OTP 27.2
-export([set_end_location/2]).
-endif.

-ifndef(HAVE_erl_anno__set_end_location_2).
set_end_location(Location, {L, C} = Anno) when is_integer(L), L >= 0, is_integer(C), C >= 1 ->
    set_end_location1(Location, [{location, Anno}]);
set_end_location(Location, Anno) when is_integer(Anno), Anno >= 0 -> set_end_location1(Location, [{location, Anno}]);
set_end_location(Location, [_|_] = Anno) -> set_end_location1(Location, Anno);
set_end_location(Location, Anno) -> error(badarg, [Location, Anno]).

set_end_location1({L, C} = Location, Anno) when is_integer(L), L >= 0, is_integer(C), C >= 1 ->
    set_end_location2(Location, Anno);
set_end_location1(Location, Anno) when is_integer(Location), Location >= 0 -> set_end_location2(Location, Anno);
set_end_location1(Location, Anno) -> error(badarg, [Location, Anno]).

set_end_location2(Location, Anno) -> lists:keystore(end_location, 1, Anno, {end_location, Location}).
-endif.
