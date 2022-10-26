-module(otpbp_user).

-ifndef(HAVE_user__interfaces_1).
% OTP >= 26.0
-export([take/2]).
-endif.

-ifndef(HAVE_user__interfaces_1).
interfaces(User) ->
    case process_info(User, dictionary) of
        {dictionary, Dict} ->
            case lists:keysearch(shell, 1, Dict) of
                {value, {shell, Shell} = Sh} when is_pid(Shell) -> [Sh];
                _ -> []
            end;
        _ -> []
    end.
-endif.
