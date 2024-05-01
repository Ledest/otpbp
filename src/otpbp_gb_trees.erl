-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_dict__foreach_2).
foreach(F, T) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, gb_trees:to_list(T)).
-endif.
