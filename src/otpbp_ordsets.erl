-module(otpbp_ordsets).

-ifndef(HAVE_ordsets__is_empty_1).
% OTP 21.0
-export([is_empty/1]).
-endif.
-ifndef(HAVE_ordsets__map_2).
% OTP 26.1
-export([map/2]).
-endif.
-ifndef(HAVE_ordsets__filtermap_2).
% OTP 26.1
-export([filtermap/2]).
-endif.
-ifndef(HAVE_ordsets__is_equal_2).
% OTP 26.1
-export([is_equal/2]).
-endif.
-ifndef(HAVE_ordsets__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_ordsets__is_empty_1).
is_empty(S) -> S =:= [].
-endif.

-ifndef(HAVE_ordsets__map_2).
map(F, S) -> ordsets:from_list(lists:map(F, S)).
-endif.

-ifndef(HAVE_ordsets__filtermap_2).
filtermap(F, S) -> ordsets:from_list(lists:filtermap(F, S)).
-endif.

-ifndef(HAVE_ordsets__is_equal_2).
is_equal(S1, S2) when is_list(S1), is_list(S2) -> S1 == S2.
-endif.

-ifndef(HAVE_ordsets__foreach_2).
foreach(F, S) when is_function(F, 1) ->
    ordsets:is_set(S) orelse error(function_clause, [F, S]),
    lists:foreach(F, ordsets:to_list(S)).
-endif.
