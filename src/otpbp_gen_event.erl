-module(otpbp_gen_event).

-ifndef(HAVE_gen_event__system_get_state_1).
-export([system_get_state/1]).
-ifndef(NEED_record__handler).
-define(NEED_record__handler, true).
-endif.
-endif.
-ifndef(HAVE_gen_event__system_replace_state_2).
-export([system_replace_state/2]).
-ifndef(NEED_record__handler).
-define(NEED_record__handler, true).
-endif.
-endif.

-ifdef(NEED_record__handler).
-record(handler, {module             :: atom(),
                  id = false,
                  state,
                  supervised = false :: 'false' | pid()}).
-endif.

-ifndef(HAVE_gen_event__system_get_state_1).
system_get_state([_ServerName, MSL, _Hib]) ->
    {ok, [{Mod, Id, State} || #handler{module = Mod, id = Id, state = State} <- MSL]}.
-endif.

-ifndef(HAVE_gen_event__system_replace_state_2).
system_replace_state(StateFun, [ServerName, MSL, Hib]) ->
    {NMSL, NStates} = lists:unzip([begin
                                       Cur = {Mod, Id, State},
                                       try
                                           NState = {Mod, Id, NS} = StateFun(Cur),
                                           {HS#handler{state = NS}, NState}
                                       catch
                                           _:_ -> {HS, Cur}
                                       end
                                   end || #handler{module = Mod, id = Id, state = State} = HS <- MSL]),
    {ok, NStates, [ServerName, NMSL, Hib]}.
-endif.
