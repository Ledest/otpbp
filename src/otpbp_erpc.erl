-module(otpbp_erpc).

-ifdef(HAVE_erlang__spawn_request_abandon_1).
-ifndef(HAVE_erpc__check_response_3).
% OTP 25.0
-export([check_response/3]).
-endif.
-ifndef(HAVE_erpc__receive_response_3).
% OTP 25.0
-export([receive_response/3]).
-endif.
-ifndef(HAVE_erpc__reqids_add_3).
% OTP 25.0
-export([reqids_add/3]).
-endif.
-ifndef(HAVE_erpc__reqids_size_1).
% OTP 25.0
-export([reqids_size/1]).
-endif.
-ifndef(HAVE_erpc__reqids_to_list_1).
% OTP 25.0
-export([reqids_to_list/1]).
-endif.
-ifndef(HAVE_erpc__send_request_6).
% OTP 25.0
-export([send_request/6]).
-endif.
-ifndef(HAVE_erpc__wait_response_3).
% OTP 25.0
-export([wait_response/3]).
-endif.

-compile({parse_transform, otpbp_pt}).

-ifndef(HAVE_erpc__check_response_3).
check_response(_Msg, ReqIdCol, Del) when map_size(ReqIdCol) =:= 0, is_boolean(Del) -> no_request;
check_response({spawn_reply, ReqId, error, Reason}, ReqIdCol, Del)
  when is_reference(ReqId), is_map_key(ReqId, ReqIdCol), is_boolean(Del) ->
    collection_result(spawn_reply, ReqId, Reason, ReqIdCol, true, Del);
check_response({'DOWN', ReqId, process, _Pid, Reason}, ReqIdCol, Del)
  when is_reference(ReqId), is_map_key(ReqId, ReqIdCol), is_boolean(Del) ->
    collection_result(down, ReqId, Reason, ReqIdCol, true, Del);
check_response(_Msg, ReqIdCol, Del) ->
    is_map(ReqIdCol) andalso is_boolean(Del) orelse error({erpc, badarg}),
    no_response.

-ifndef(NEED_collection_result_6).
-define(NEED_collection_result_6, true).
-endif.
-endif.

-ifndef(HAVE_erpc__receive_response_3).
receive_response(ReqIdCol, WT, Del) when map_size(ReqIdCol) =:= 0, is_boolean(Del) ->
    _ = timeout_value(WT),
    no_request;
receive_response(ReqIdCol, Tmo, Del) when is_map(ReqIdCol), is_boolean(Del) ->
    Timeout = timeout_value(Tmo),
    receive
        {spawn_reply, ReqId, error, Reason} when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(spawn_reply, ReqId, Reason, ReqIdCol, false, Del);
        {'DOWN', ReqId, process, _Pid, Reason} when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(down, ReqId, Reason, ReqIdCol, false, Del)
    after Timeout ->
        try
            maps:foreach(fun abandon/2, ReqIdCol)
        catch
            throw:badarg -> error({erpc, badarg})
        end,
        error({erpc, timeout})
    end;
receive_response(_, _, _) -> error({erpc, badarg}).

abandon(ReqId, [Res|_Label]) when is_reference(ReqId), is_reference(Res) ->
    spawn_request_abandon(ReqId) orelse demonitor(ReqId, [info]) orelse
        receive
            {spawn_reply, ReqId, error, _} -> ok;
            {'DOWN', ReqId, process, _, _} -> ok
        after 0 -> ok
        end;
abandon(_, _) -> throw(badarg).

-ifndef(NEED_timeout_value_1).
-define(NEED_timeout_value_1, true).
-endif.
-ifndef(NEED_collection_result_6).
-define(NEED_collection_result_6, true).
-endif.
-endif.

-ifndef(HAVE_erpc__reqids_add_3).
reqids_add([Res|ReqId], Label, ReqIdCollection)
  when is_reference(Res), is_reference(ReqId), not is_map_key(ReqId, ReqIdCollection) ->
    ReqIdCollection#{ReqId => [Res|Label]};
reqids_add(_, _, _) -> error({erpc, badarg}).
-endif.

-ifndef(HAVE_erpc__reqids_size_1).
reqids_size(ReqIdCollection) ->
    try
        maps:size(ReqIdCollection)
    catch
        _:_ -> error({erpc, badarg})
    end.
-endif.

-ifndef(HAVE_erpc__reqids_to_list_1).
reqids_to_list(ReqIdCollection) when is_map(ReqIdCollection) ->
    try
        maps:fold(fun(ReqId, [Res|Label], Acc) when is_reference(ReqId), is_reference(Res) -> [{[Res|ReqId], Label}|Acc];
                     (_, _, _) -> throw(badarg)
                  end,
                  [], ReqIdCollection)
    catch
        throw:badarg -> error({erpc, badarg})
    end;
reqids_to_list(_) -> error({erpc, badarg}).
-endif.

-ifndef(HAVE_erpc__wait_response_3).
wait_response(ReqIdCol, WT, Del) when map_size(ReqIdCol) =:= 0, is_boolean(Del) ->
    _ = timeout_value(WT),
    no_request;
wait_response(ReqIdCol, WT, Del) when is_map(ReqIdCol), is_boolean(Del) ->
    Timeout = timeout_value(WT),
    receive
        {spawn_reply, ReqId, error, Reason} when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(spawn_reply, ReqId, Reason, ReqIdCol, true, Del);
        {'DOWN', ReqId, process, _Pid, Reason} when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(down, ReqId, Reason, ReqIdCol, true, Del)
    after Timeout -> no_response
    end;
wait_response(_, _, _) -> error({erpc, badarg}).

-ifndef(NEED_timeout_value_1).
-define(NEED_timeout_value_1, true).
-endif.
-ifndef(NEED_collection_result_6).
-define(NEED_collection_result_6, true).
-endif.
-endif.

-ifndef(HAVE_erpc__send_request_6).
send_request(N, M, F, A, L, C) when is_atom(N), is_atom(M), is_atom(F), is_list(A), is_map(C) ->
    Res = make_ref(),
    C#{spawn_request(N, erpc, execute_call, [Res, M, F, A], [{reply, error_only}, monitor]) => [Res|L]};
send_request(_N, _M, _F, _A, _L, _C) -> error({erpc, badarg}).
-endif.

-ifdef(NEED_collection_result_6).
collection_result(Type, ReqId, ResultReason, ReqIdCol, WrapResponse, Delete) ->
    case reqid(ReqId, ReqIdCol, Delete) of
        {[Res|Label], NewReqIdCol} when is_reference(Res) ->
            try result(Type, ReqId, Res, ResultReason) of
                Result -> {wrap(WrapResponse, Result), Label, NewReqIdCol}
            catch
                Class:Reason -> erlang:Class({Reason, Label, NewReqIdCol})
            end;
        _ -> error({erpc, badarg})
    end.

-compile({inline, wrap/2}).
wrap(true, R) -> {response, R};
wrap(_false, R) -> R.

-compile({inline, reqid/3}).
reqid(ReqId, ReqIdCol, true) -> maps:take(ReqId, ReqIdCol);
reqid(ReqId, ReqIdCol, false) -> {maps:get(ReqId, ReqIdCol), ReqIdCol}.

-compile({inline, result/4}).
result(down, _ReqId, Res, {Res, return, Return}) -> Return;
result(down, _ReqId, Res, {Res, throw, Throw}) -> throw(Throw);
result(down, _ReqId, Res, {Res, exit, Exit}) -> exit({exception, Exit});
result(down, _ReqId, Res, {Res, error, Error, Stack}) -> error({exception, Error, Stack});
result(down, _ReqId, Res, {Res, error, {erpc, _} = ErpcErr}) -> error(ErpcErr);
result(down, _ReqId, _Res, noconnection) -> error({erpc, noconnection});
result(down, _ReqId, _Res, Reason) -> exit({signal, Reason});
result(spawn_reply, _ReqId, _Res, Reason) -> error({erpc, Reason}).
-endif.

-ifdef(NEED_timeout_value_1).
-define(MAX_INT_TIMEOUT, 4294967295).

timeout_value({abs, Timeout}) when is_integer(Timeout) ->
    if
        Timeout >= 0, Timeout =< ?MAX_INT_TIMEOUT -> Timeout;
        true ->
            case Timeout - erlang:monotonic_time(millisecond) of
                TMO when TMO > ?MAX_INT_TIMEOUT -> error({erpc, badarg});
                TMO -> max(TMO, 0)
            end
    end;
timeout_value(Timeout) ->
    Timeout =:= infinity orelse error({erpc, badarg}),
    Timeout.
-endif.
-endif.
