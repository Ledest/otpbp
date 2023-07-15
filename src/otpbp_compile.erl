-module(otpbp_compile).

-ifndef(HAVE_compile__iofile_1).
% OTP < 24.0
-export([iofile/1]).
-endif.

-ifndef(HAVE_compile__iofile_1).
iofile(F) when is_atom(F) -> iofile(atom_to_list(F));
iofile(F) -> {filename:dirname(F), filename:basename(F, ".erl")}.
-endif.
