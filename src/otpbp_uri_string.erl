-module(otpbp_uri_string).

-ifndef(HAVE_uri_string__is_host_1).
-export([is_host/1]).
-endif.
-ifndef(HAVE_uri_string__is_path_1).
-export([is_path/1]).
-endif.

-define(SUB_DELIM, "!$&'()*+,;=").

-ifndef(HAVE_uri_string__is_host_1).
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
is_host(C) -> lists:member(C, ":" ?SUB_DELIM) orelse is_unreserved(C).
-endif.

-ifndef(HAVE_uri_string__is_path_1).
-ifndef(NEED_is_pchar_1).
-define(NEED_is_pchar_1, true).
-endif.
is_path(C) -> C =:= $/ orelse is_pchar(C).
-endif.

-ifdef(NEED_is_pchar_1).
-ifndef(NEED_is_unreserved_1).
-define(NEED_is_unreserved_1, true).
-endif.
is_pchar(C) -> lists:member(C, "%:@" ?SUB_DELIM) orelse is_unreserved(C).
-endif.

-ifdef(NEED_is_unreserved_1).
-ifndef(NEED_is_alpha_1).
-define(NEED_is_alpha_1, true).
-endif.
-ifndef(NEED_is_digit_1).
-define(NEED_is_digit_1, true).
-endif.
is_unreserved(C) -> lists:member(C, "-._~") orelse is_alpha(C) orelse is_digit(C).
-endif.

-ifdef(NEED_is_alpha_1).
is_alpha(C) -> C >= $A andalso C =< $Z orelse C >= $a andalso C =< $z.
-endif.

-ifdef(NEED_is_digit_1).
is_digit(C) -> C >= $0 andalso C =< $9.
-endif.
