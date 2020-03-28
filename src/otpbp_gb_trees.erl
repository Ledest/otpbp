-module(otpbp_gb_trees).

-ifndef(HAVE_gb_trees__take_2).
% OTP 20.0
-export([take/2]).
-endif.

-ifndef(HAVE_gb_trees__take_any_2).
% OTP 20.0
-export([take_any/2]).
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
