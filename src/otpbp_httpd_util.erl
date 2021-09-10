-module(otpbp_httpd_util).

-ifndef(HAVE_httpd_util__flatlength_1).
% OTP < 26.0
-export([flatlength/1]).
-endif.
-ifndef(HAVE_httpd_util__integer_to_hexlist_1).
% OTP < 26.0
-export([integer_to_hexlist/1]).
-endif.
-ifndef(HAVE_httpd_util__strip_1).
% OTP < 26.0
-export([strip/1]).
-endif.
-ifndef(HAVE_httpd_util__suffix_1).
% OTP < 26.0
-export([suffix/1]).
-endif.

-ifndef(HAVE_httpd_util__flatlength_1).
flatlength(L) when is_list(L) -> erlang:iolist_size(L).
-endif.

-ifndef(HAVE_httpd_util__integer_to_hexlist_1).
integer_to_hexlist(I) when is_integer(I) -> http_util:integer_to_hexlist(I).
-endif.

-ifndef(HAVE_httpd_util__strip_1).
strip([H|T]) when H =:= $\s; H =:= $\t -> strip(T);
strip(L) -> rstrip(L).

rstrip([H|T]) ->
    case rstrip(T) of
        [] when H =:= $\s; H =:= $\t -> [];
        L -> [H|L]
    end;
rstrip([]) -> [];
rstrip(_) -> error(badarg).
-endif.

-ifndef(HAVE_httpd_util__suffix_1).
suffix(P) ->
    case filename:extension(P) of
        [$.|E] -> E;
        [] -> ""
    end.
-endif.
