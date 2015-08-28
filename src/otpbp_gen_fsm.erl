-module(otpbp_gen_fsm).

-ifndef(HAVE_gen_fsm__system_get_state_1).
-export([system_get_state/1]).
-endif.
-ifndef(HAVE_gen_fsm__system_replace_state_2).
-export([system_replace_state/2]).
-endif.

-ifndef(HAVE_gen_fsm__system_get_state_1).
system_get_state([_Name, StateName, StateData, _Mod, _Time]) -> {ok, {StateName, StateData}}.
-endif.

-ifndef(HAVE_gen_fsm__system_replace_state_2).
system_replace_state(StateFun, [Name, StateName, StateData, Mod, Time]) ->
    Result = {NStateName, NStateData} = StateFun({StateName, StateData}),
    {ok, Result, [Name, NStateName, NStateData, Mod, Time]}.
-endif.
