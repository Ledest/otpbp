-module(otpbp_compile).

-ifndef(HAVE_compile__iofile_1).
% OTP < 24.0
-export([iofile/1]).
-endif.

-ifndef(HAVE_compile__iofile_1).
iofile(File) ->
    F = if
            is_atom(File) -> atom_to_list(File);
            true -> File
        end,
    {filename:dirname(F), filename:basename(F, ".erl")}.
-endif.
