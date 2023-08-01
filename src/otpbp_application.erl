-module(otpbp_application).

-ifdef(HAVE_application_controller__set_env_2).
-ifndef(HAVE_application__set_env_2).
% OTP 21.3
-export([set_env/2]).
-endif.
-ifndef(HAVE_application__set_env_1).
% OTP 21.3
-export([set_env/1]).
-endif.

-ifndef(HAVE_application__set_env_1).
-ifdef(HAVE_application__set_env_2).
-import(application, [set_env/2]).
-endif.
-endif.
-endif.

-ifndef(HAVE_application_controller__get_supervisor_2).
% OTP 26.0
-export([get_supervisor/1]).
-endif.

-ifdef(HAVE_application_controller__set_env_2).
-ifndef(HAVE_application__set_env_2).
set_env(Config, Opts) when is_list(Config), is_list(Opts) ->
    case application_controller:set_env(Config, Opts) of
        ok -> ok;
        {error, Msg} -> error({badarg, Msg}, [Config, Opts])
    end.
-endif.

-ifndef(HAVE_application__set_env_1).
set_env(Config) when is_list(Config) -> set_env(Config, []).
-endif.
-endif.

-ifndef(HAVE_application_controller__get_supervisor_2).
% OTP 25.3
get_supervisor(Application) when is_atom(Application) ->
    case application_controller:get_master(Application) of
        undefined -> undefined;
        Master ->
            case application_master:get_child(Master) of
                {Root, _App} -> {ok, Root};
                _error -> undefined
            end
    end.
-endif.
