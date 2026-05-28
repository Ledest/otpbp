-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__from_list_1).
% OTP 29.0
-export([from_list/1]).
-endif.
-ifndef(HAVE_gb_trees__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_gb_trees__from_list_1).
from_list(L) -> gb_trees:from_orddict(orddict:from_list(L)).
-endif.

-ifndef(HAVE_dict__foreach_2).
foreach(F, T) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, gb_trees:to_list(T)).
-endif.
