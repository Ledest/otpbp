-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__take_2).
% OTP 20.0
-export([take/2]).
-endif.
-ifndef(HAVE_gb_trees__take_any_2).
% OTP 20.0
-export([take_any/2]).
-endif.
-ifndef(HAVE_gb_trees__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_gb_trees__take_2).
take(Key, Tree) -> {gb_trees:get(Key, Tree), gb_trees:delete(Key, Tree)}.
-endif.

-ifndef(HAVE_gb_trees__take_any_2).
take_any(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, V} -> {V, gb_trees:delete(Key, Tree)};
        none -> error
    end.
-endif.

-ifndef(HAVE_dict__foreach_2).
foreach(F, T) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, gb_trees:to_list(T));
foreach(F, T) -> error(badarg, [F, T]).
-endif.
