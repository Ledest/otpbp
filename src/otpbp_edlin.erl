-module(otpbp_edlin).

-ifndef(HAVE_edlin__current_chars_1).
-export([current_chars/1]).
-endif.

-ifndef(HAVE_edlin__current_chars_1).
current_chars({line, _, {Bef, Aft}, _}) -> lists:reverse(Bef, Aft).
-endif.
