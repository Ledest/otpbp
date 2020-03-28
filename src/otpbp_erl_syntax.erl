-module(otpbp_erl_syntax).

-ifndef(HAVE_erl_syntax__record_access_2).
% OTP < 18.0
-export([record_access/2]).
-endif.

-ifndef(HAVE_erl_syntax__record_access_2).
-spec record_access(Argument::erl_syntax:syntaxTree(), Field::erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
record_access(Argument, Field) -> erl_syntax:record_access(Argument, erl_syntax:atom(any), Field).
-endif.
