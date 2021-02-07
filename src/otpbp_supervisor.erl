-module(otpbp_supervisor).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_supervisor__get_callback_module_1).
-ifndef(NEED_supervisor__state_record).
-define(NEED_supervisor__state_record, true).
-endif.
% OTP 18.3
-export([get_callback_module/1]).
-endif.
-ifndef(HAVE_supervisor__format_status_2).
-ifndef(NEED_supervisor__state_record).
-define(NEED_supervisor__state_record, true).
-endif.
% OTP 19.0
-export([format_status/2]).
-endif.

-ifdef(NEED_supervisor__state_record).
-record(state, {name,
                strategy,
                children = [],
                dynamics,
                intensity :: non_neg_integer(),
                period :: pos_integer(),
                restarts = [],
                module,
                args}).
-endif.

-ifndef(HAVE_supervisor__get_callback_module_1).
get_callback_module(Pid) ->
    {status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(Pid),
    case lists:keyfind(supervisor, 1, Misc) of
        {_, [{"Callback", Mod}]} -> Mod;
        _ ->
            [_Header, _Data, {data, [{"State", #state{module = Module}}]}|_] = Misc,
            Module
    end.
-endif.

-ifndef(HAVE_supervisor__format_status_2).
format_status(terminate, [_PDict, State]) -> State;
format_status(_, [_PDict, #state{module = Module} = State]) ->
    [{data, [{"State", State}]}, {supervisor, [{"Callback", Module}]}].
-endif.
