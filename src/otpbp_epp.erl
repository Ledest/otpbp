-module(otpbp_epp).

-ifndef(HAVE_epp__parse_file_2).
-export([parse_file/2]).
-endif.

-ifndef(HAVE_epp__parse_file_2).
parse_file(Ifile, Options) ->
    epp:parse_file(Ifile, proplists:get_value(includes, Options, []), proplists:get_value(macros, Options, [])).
-endif.
