-module(otpbp_gen_server).

-ifndef(HAVE_gen_server__system_get_state_1).
-export([system_get_state/1]).
-endif.
-ifndef(HAVE_gen_server__system_replace_state_2).
-export([system_replace_state/2]).
-endif.

-ifndef(HAVE_gen_server__system_get_state_1).
system_get_state([_Name, State, _Mod, _Time]) -> {ok, State}.
-endif.

-ifndef(HAVE_gen_server__system_replace_state_2).
system_replace_state(StateFun, [Name, State, Mod, Time]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, Mod, Time]}.
-endif.
