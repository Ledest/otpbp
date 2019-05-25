-module(otpbp_erl_syntax).

-ifndef(HAVE_erl_syntax__char_literal_2).
-export([char_literal/2]).
-endif.
-ifndef(HAVE_erl_syntax__string_literal_2).
-export([string_literal/2]).
-endif.
-ifndef(HAVE_erl_syntax__record_access_2).
-export([record_access/2]).
-endif.

-ifndef(HAVE_erl_syntax__char_literal_2).
char_literal(Node, latin1) -> erl_syntax:char_literal(Node).
-endif.

-ifndef(HAVE_erl_syntax__string_literal_2).
string_literal(Node, latin1) -> erl_syntax:string_literal(Node).
-endif.

-ifndef(HAVE_erl_syntax__record_access_2).
-spec record_access(Argument::erl_syntax:syntaxTree(), Field::erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
record_access(Argument, Field) -> erl_syntax:record_access(Argument, erl_syntax:atom(any), Field).
-endif.
