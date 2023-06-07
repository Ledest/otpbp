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

-ifndef(HAVE_ordsets__is_empty_1).
is_empty(S) -> S =:= [].
-endif.

-ifndef(HAVE_ordsets__map_2).
map(F, S) -> ordsets:from_list(lists:map(F, S)).
-endif.

-ifndef(HAVE_ordsets__filtermap_2).
filtermap(F, S) -> ordsets:from_list(lists:filtermap(F, S)).
-endif.
