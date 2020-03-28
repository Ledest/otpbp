-module(otpbp_application).

-ifndef(HAVE_application__set_env_2).
-ifdef(HAVE_application_controller__set_env_2).
% OTP 21.3
-export([set_env/2]).
-ifndef(NEED_application__set_env_2).
-define(NEED_application__set_env_2, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_application__set_env_1).
-ifdef(HAVE_application__set_env_2).
% OTP 21.3
-export([set_env/1]).
-import(application, [set_env/2]).
-else.
-ifdef(HAVE_application_controller__set_env_2).
% OTP 21.3
-export([set_env/1]).
-ifndef(NEED_application__set_env_2).
-define(NEED_application__set_env_2, true).
-endif.
-endif.
-endif.
-endif.

-ifdef(NEED_application__set_env_2).
set_env(Config, Opts) when is_list(Config), is_list(Opts) ->
    case application_controller:set_env(Config, Opts) of
        ok -> ok;
        {error, Msg} -> error({badarg, Msg}, [Config, Opts])
    end.
-endif.

-ifndef(HAVE_application__set_env_1).
-ifdef(HAVE_application_controller__set_env_2).
set_env(Config) when is_list(Config) -> set_env(Config, []).
-endif.
-endif.
