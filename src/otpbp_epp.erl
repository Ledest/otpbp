-module(otpbp_epp).

-ifndef(HAVE_epp__scan_file_2).
% OTP 24.0
-export([scan_file/2]).
-endif.
-ifndef(HAVE_epp__scan_file_1).
% OTP 24.0
-export([scan_file/1]).
-endif.

-ifndef(HAVE_epp__scan_file_2).
-ifdef(HAVE_epp__scan_file_1).
-import(epp, [scan_file/1]).
-endif.
-endif.

-ifndef(HAVE_epp__scan_file_2).
scan_file(File, Options) ->
    case epp:open([{name, File}, extra|Options]) of
        {ok, Epp, Extra} ->
            Forms = scan_file(Epp),
            epp:close(Epp),
            {ok, Forms, Extra};
        {error, _} = E -> E
    end.
-endif.

-ifndef(HAVE_epp__scan_file_1).
scan_file(Epp) ->
    case epp:scan_erl_form(Epp) of
        {ok, Toks} -> [Toks|scan_file(Epp)];
        {error, _} = E -> [E|scan_file(Epp)];
        {eof, _} = E -> [E]
    end.
-endif.
