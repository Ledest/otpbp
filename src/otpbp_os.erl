-module(otpbp_os).

-ifndef(HAVE_os__env_0).
% OTP 24.0
-export([env/0]).
-endif.
-ifndef(HAVE_os__list_env_vars_0).
% OTP 21.0 - 23.x
-export([list_env_vars/0]).
-endif.

-ifndef(HAVE_os__list_env_vars_0).
-ifdef(HAVE_os__env_0).
-import(os, [env/0]).
-endif.
-endif.

-ifndef(HAVE_os__list_env_vars_0).
list_env_vars() -> env().
-endif.

-ifndef(HAVE_os__env_0).
-ifdef(HAVE_os__list_env_vars_0).
env() -> os:list_env_vars().
-else.
env() ->
    lists:map(fun(E) ->
                  {N, [$=|V]} = lists:splitwith(fun(C) -> C =/= $= end, E),
                  {N, V}
              end, os:getenv()).
-endif.
-endif.
