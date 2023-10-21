-module(otpbp_edlin).

-ifndef(HAVE_edlin__keymap_0).
% OTP 26.1
-export([keymap/0]).
-endif.

-ifndef(HAVE_edlin__keymap_0).
keymap() -> get(key_map).
-endif.
