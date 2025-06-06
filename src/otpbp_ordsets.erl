-module(otpbp_ordsets).

-ifndef(HAVE_ordsets__map_2).
% OTP 27.0
-export([map/2]).
-endif.
-ifndef(HAVE_ordsets__filtermap_2).
% OTP 27.0
-export([filtermap/2]).
-endif.
-ifndef(HAVE_ordsets__is_equal_2).
% OTP 27.0
-export([is_equal/2]).
-endif.
-ifndef(HAVE_ordsets__foreach_2).
-export([foreach/2]).
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
foreach(F, S) when is_function(F, 1) -> lists:foreach(F, ordsets:to_list(S)).
-endif.
