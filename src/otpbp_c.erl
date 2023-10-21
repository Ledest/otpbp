-module(otpbp_c).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_c__lm_0).
% OTP 20.0
-export([lm/0]).
-endif.
-ifndef(HAVE_c__erlangrc_1).
% OTP 21.0
-export([erlangrc/1]).
-endif.

-ifndef(HAVE_c__lm_0).
lm() -> lists:map(fun c:l/1, c:mm()).
-endif.

-ifndef(HAVE_c__erlangrc_1).
erlangrc([Home|_] = Paths) when is_list(Home) ->
    case file:path_eval(Paths, ".erlang") of
        {error, {Line, _Mod, _Term} =  E} = Error ->
            error_logger:error_msg("file:path_eval(~tp,.erlang): error on line ~p: ~ts~n",
                                   [Paths, Line, file:format_error(E)]),
            Error;
        {error, E} = Error when E =/= enoent ->
            error_logger:error_msg("file:path_eval(~tp,.erlang): ~ts~n", [Paths, file:format_error(E)]),
            Error;
        Other -> Other
    end.
-endif.
