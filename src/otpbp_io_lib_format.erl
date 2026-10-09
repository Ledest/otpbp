-module(otpbp_io_lib_format).

-ifndef(HAVE_io_lib__build_bin_1).
% OTP 28.0
-export([build_bin/1]).
-endif.
-ifndef(HAVE_io_lib__build_bin_2).
% OTP 28.0
-export([build_bin/2]).
-endif.

-ifndef(HAVE_io_lib__build_bin_1).
build_bin(Cs) -> unicode:characters_to_binary(io_lib_format:build(Cs)).
-endif.
-ifndef(HAVE_io_lib__build_bin_2).
build_bin(Cs, Options) -> unicode:characters_to_binary(io_lib_format:build(Cs, Options)).
-endif.
