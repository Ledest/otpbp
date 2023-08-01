-module(otpbp_error_logger).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_error_logger__limit_term_1).
% OTP 20.0
-export([limit_term/1]).
-endif.
-ifndef(HAVE_error_logger__get_format_depth_0).
% OTP 20.0
-export([get_format_depth/0]).
-endif.

-ifdef(HAVE_error_logger__get_format_depth_0).
-import(error_logger, [get_format_depth/0]).
-endif.

-ifndef(HAVE_error_logger__limit_term_1).
-spec limit_term(term()) -> term().
limit_term(Term) ->
    case get_format_depth() of
        unlimited -> Term;
        D -> io_lib:limit_term(Term, D)
    end.
-endif.

-ifndef(HAVE_error_logger__get_format_depth_0).
get_format_depth() ->
    case application:get_env(kernel, error_logger_format_depth) of
        {ok, Depth} when is_integer(Depth) -> max(10, Depth);
        undefined -> unlimited
    end.
-endif.
