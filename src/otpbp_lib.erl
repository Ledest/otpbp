-module(otpbp_lib).

-ifndef(HAVE_lib__nonl_1).
-export([nonl/1]).
-endif.
-ifndef(HAVE_lib__send_2).
-export([send/2]).
-endif.
-ifndef(HAVE_lib__sendw_2).
-export([sendw/2]).
-endif.
-ifndef(HAVE_lib__flush_receive_0).
-export([flush_receive/0]).
-endif.
-ifndef(HAVE_lib__error_message_2).
-export([error_message/2]).
-endif.
-ifndef(HAVE_lib__progname_0).
-export([progname/0]).
-endif.
-ifndef(HAVE_lib__eval_str_1).
-export([eval_str/1]).
-endif.

-ifndef(HAVE_lib__nonl_1).
nonl([$\n|T]) -> nonl(T);
nonl([H|T]) -> [H|nonl(T)];
nonl([]) -> [].
-endif.

-ifndef(HAVE_lib__send_2).
send(To, Msg) -> To ! Msg.
-endif.

-ifndef(HAVE_lib__sendw_2).
sendw(To, Msg) ->
    To ! {self(), Msg},
    receive
        Reply -> Reply
    end.
-endif.

-ifndef(HAVE_lib__flush_receive_0).
flush_receive() ->
    receive
        _ -> flush_receive()
    after 0 -> ok
    end.
-endif.

-ifndef(HAVE_lib__error_message_2).
error_message(Format, Args) -> io:format("** ~ts **\n", [io_lib:format(Format, Args)]).
-endif.

-ifndef(HAVE_lib__progname_0).
progname() ->
    case init:get_argument(progname) of
        {ok, [[Prog]]} -> list_to_atom(Prog);
        _ -> no_prog_name
    end.
-endif.

-ifndef(HAVE_lib__eval_str_1).
eval_str(Str) when is_list(Str) ->
    case erl_scan:tokens([], Str, 0) of
        {more, _} -> {error, "Incomplete form (missing .<cr>)??"};
        {done, {ok, Toks, _}, Rest} ->
            case lists:all(fun(C) -> C =:= $\s orelse C =:= $\n orelse C =:= $\t end, Rest) andalso
                 erl_parse:parse_exprs(Toks) of
                {ok, Exprs} -> case catch erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
                                   {value, Val, _} -> {ok, Val};
                                   Other -> {error, result("*** eval: ~p", [Other])}
                               end;
                {error, {_Line, Mod, Args}} -> {error, result("*** ~ts", [Mod:format_error(Args)])};
                false -> {error, result("Non-white space found after end-of-form :~ts", [Rest])}
            end
    end;
eval_str(Bin) when is_binary(Bin) -> eval_str(binary_to_list(Bin)).

result(F, D) -> lists:flatten(io_lib:format(F, D)).
-endif.
