-module(otpbp_orddict).

-ifndef(HAVE_orddict__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_orddict__foreach_2).
foreach(F, D) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, orddict:to_list(D)).
-endif.
