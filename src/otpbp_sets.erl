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

-ifndef(HAVE_sets__is_empty_1).
is_empty(S) -> sets:size(S) =:= 0.
-endif.

-ifndef(HAVE_sets__from_list_2).
from_list(List, [{version, 1}]) -> sets:from_list(List).
-endif.

-ifndef(HAVE_sets__new_1).
new([{version, 1}]) -> sets:new().
-endif.
