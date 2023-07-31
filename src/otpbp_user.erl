-module(otpbp_user).

-ifndef(HAVE_user__interfaces_1).
% OTP >= 26.0
-export([interfaces/1]).
-endif.

-ifndef(HAVE_user__interfaces_1).
interfaces(User) ->
    case process_info(User, dictionary) of
        {dictionary, Dict} ->
            case lists:keyfind(shell, 1, Dict) of
                {_shell, Shell} = Sh when is_pid(Shell) -> [Sh];
                _ -> []
            end;
        _ -> []
    end.
-endif.
