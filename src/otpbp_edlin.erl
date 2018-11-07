-module(otpbp_edlin).

-ifndef(HAVE_edlin__current_chars_1).
-export([current_chars/1]).
-endif.
-ifndef(HAVE_edlin__start_2).
-export([start/2]).
-endif.

-ifndef(HAVE_edlin__current_chars_1).
current_chars({line, _, {Bef, Aft}, _}) -> unicode:characters_to_list(lists:reverse(Bef, Aft)).
-endif.

-ifndef(HAVE_edlin__start_2).
start(Pbs, _) -> edlin:start(Pbs).
-endif.
