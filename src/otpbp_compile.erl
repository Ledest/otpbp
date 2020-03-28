-module(otpbp_compile).

-ifndef(HAVE_compile__env_compiler_options_0).
% OTP 19.0
-export([env_compiler_options/0]).
-endif.

-ifndef(HAVE_compile__env_compiler_options_0).
-define(ERL_COMPILER_OPTIONS, "ERL_COMPILER_OPTIONS").
env_compiler_options() ->
    case os:getenv(?ERL_COMPILER_OPTIONS) of
        [_|_] = Str -> case erl_scan:string(Str) of
                           {ok, Tokens, _} -> case erl_parse:parse_term(Tokens ++ [{dot, erl_anno:new(1)}]) of
                                                  {ok, List} when is_list(List) -> List;
                                                  {ok, Term} -> [Term];
                                                  {error, _Reason} ->
                                                      io:put_chars("Ignoring bad term in " ?ERL_COMPILER_OPTIONS "\n"),
                                                      []
                                              end;
                           {error, {_, _, _Reason}, _} ->
                               io:put_chars("Ignoring bad term in " ?ERL_COMPILER_OPTIONS "\n"),
                               []
                       end;
        _ -> []
    end.
-endif.
