-module(otpbp_dict).

-ifndef(HAVE_dict__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_dict__foreach_2).
foreach(F, D) when is_function(F, 2) ->
    dict:fold(fun(K, V, _) ->
                  F(K, V),
                  ok
              end,
              ok, D).
-endif.
