-module(otpbp_error_handler).

-ifndef(HAVE_error_handler__raise_undef_exception_3).
-export([raise_undef_exception/3]).
-endif.

-ifndef(HAVE_error_handler__raise_undef_exception_3).
raise_undef_exception(Module, Func, Args) ->
    try
        error(undef)
    catch
        error:undef -> erlang:raise(error, undef, [{Module, Func, Args, []}|tl(erlang:get_stacktrace())])
    end.
-endif.
