-module(otpbp_erl_compile).

-ifndef(HAVE_erl_compile__compile_cmdline_0).
-export([compile_cmdline/0]).
-endif.

-ifndef(HAVE_epp__parse_file_2).
compile_cmdline() -> erl_compile:compile_cmdline(init:get_plain_arguments()).
-endif.
