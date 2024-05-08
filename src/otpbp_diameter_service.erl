-module(otpbp_diameter_service).

-ifndef(HAVE_diameter_service__which_connections_0).
% OTP 27.0
-export([which_connections/0]).
-endif.
-ifndef(HAVE_diameter_service__which_connections_1).
% OTP 27.0
-export([which_connections/1]).
-endif.
-ifndef(HAVE_diameter_service__which_watchdogs_0).
% OTP 27.0
-export([which_watchdogs/0]).
-endif.
-ifndef(HAVE_diameter_service__which_watchdogs_1).
% OTP 27.0
-export([which_watchdogs/1]).
-endif.

-ifndef(HAVE_diameter_service__which_watchdogs_0).
-ifdef(HAVE_diameter_service__which_watchdogs_1).
-import(diameter_service, [which_watchdogs/1]).
-endif.
-endif.
-ifndef(HAVE_diameter_service__which_connections_0).
-ifdef(HAVE_diameter_service__which_connections_1).
-import(diameter_service, [which_connections/1]).
-endif.
-endif.

-ifndef(HAVE_diameter_service__which_watchdogs_1).
-ifndef(NEED_record_watchdog).
-define(NEED_record_watchdog, true).
-endif.
-endif.
-ifndef(HAVE_diameter_service__which_connections_1).
-ifndef(NEED_record_watchdog).
-define(NEED_record_watchdog, true).
-endif.
-endif.

-ifndef(HAVE_diameter_service__which_connections_0).
which_connections() -> which_connections([SvcName || {SvcName, _} <- diameter_service:services()], []).

which_connections([], Acc) -> lists:reverse(Acc);
which_connections([SvcName|Services], Acc) ->
    which_connections(Services,
                      case which_connections(SvcName) of
                          [] -> Acc;
                          Conns -> [{SvcName, Conns}|Acc]
                      end).
-endif.

-ifdef(NEED_record_watchdog).
-include_lib("diameter/include/diameter.hrl").

-define(WD_INITIAL, initial).
-define(WD_OKAY, okay).
-define(WD_SUSPECT, suspect).
-define(WD_DOWN, down).
-define(WD_REOPEN, reopen).

-type wd_state() :: ?WD_INITIAL | ?WD_OKAY | ?WD_SUSPECT | ?WD_DOWN | ?WD_REOPEN.
-type match(T) :: T | '_' | '$1' | '$2'.

-record(watchdog, {pid  :: match(pid()) | undefined,
                   type :: match(connect | accept),
                   ref :: match(reference()),
                   options :: match([diameter:transport_opt()]),
                   state = ?WD_INITIAL :: match(wd_state()),
                   started = diameter_lib:now(),
                   peer = false :: match(boolean() | pid())}).
-endif.

-ifndef(HAVE_diameter_service__which_connections_1).
which_connections(SvcName) ->
    case ets:lookup(diameter_service, SvcName) of
        [S] when element(1, S) =:= state ->
            case element(6, S) of
                {PT, _, _} -> connections_info(element(5, S), PT);
                _ -> []
            end;
        [S] ->
            case tuple_to_list(S) of
                [state, _, _, WDT, {PT, _, _}|_] -> connections_info(WDT, PT);
                _ -> []
            end;
        [] -> []
    end.

-record(peer, {pid :: pid(),
               apps :: match([{0..16#FFFFFFFF, diameter:app_alias()}] | [diameter:app_alias()]),
               caps :: match(#diameter_caps{}),
               started = diameter_lib:now(),
               watchdog :: match(pid()|undefined)}).

connections_info(WDT, PT) ->
    try ets:tab2list(WDT) of
        L -> connections_info(PT, L, [])
    catch
        error: badarg -> []
    end.

connections_info(_PT, [], Acc) -> lists:reverse(Acc);
connections_info(PT, [WD|WDs], Acc) -> connections_info(PT, WDs, [connection_info(PT, WD)|Acc]).

-compile({inline, connection_info/2}).
connection_info(PT, #watchdog{pid = Pid, type = Type, ref = Ref, state = State, started = Started, peer = TPid}) ->
    connection_info(PT, TPid, State,
                    #{wd => #{ref => Ref, pid => Pid, type => Type, state => State,
                              uptime => diameter_lib:now_diff(Started)}}).

-compile({inline, connection_info/4}).
connection_info(PT, TPid, State, Info) when is_pid(TPid), State =/= ?WD_DOWN ->
    try ets:lookup(PT, TPid) of
        [#peer{pid = PPid, started = Started}] -> connection_info(PPid, Started, Info);
        [] -> Info
    catch
        error: badarg -> []
    end;
connection_info(_PT, _PPid, _State, Info) -> Info.

-compile({inline, connection_info/3}).
connection_info(PPid, Started, Info) ->
    Uptime = diameter_lib:now_diff(Started),
    {_, PD} = process_info(PPid, dictionary),
    {_, {TPid, {_Type, TMod, _Cfg}}} = lists:keyfind({diameter_peer_fsm, start}, 1, PD),
    {_, TD} = process_info(TPid, dictionary),
    {_, Data} = lists:keyfind({TMod, info}, 1, TD),
    try TMod:info(Data) of
        TInfo ->
            Info#{peer => #{pid => PPid, uptime => Uptime},
                  sockname => proplists:get_value(socket, TInfo),
                  peername => proplists:get_value(peer, TInfo)}
    catch
        _:_ -> Info#{peer => #{pid => PPid, uptime => Uptime}}
    end.
-endif.

-ifndef(HAVE_diameter_service__which_watchdogs_0).
which_watchdogs() -> which_watchdogs(diameter_service:services(), []).

which_watchdogs([], Acc) -> lists:flatten(lists:reverse(Acc));
which_watchdogs([{SvcName, _}|Services], Acc) ->
    which_watchdogs(Services,
                    case which_watchdogs(SvcName) of
                        WDs when is_list(WDs) -> [[WD#{service => SvcName} || WD <- WDs]|Acc];
                        undefined -> Acc
                    end).
-endif.


-ifndef(HAVE_diameter_service__which_watchdogs_1).
which_watchdogs(SvcName) ->
    case ets:lookup(diameter_service, SvcName) of
        [S] when element(1, S) =:= state ->
            lists:map(fun(#watchdog{pid = Pid, type = Type, ref = Ref, state = State, started = Started, peer = Peer}) ->
                          #{pid => Pid, ref => Ref, type => Type, state => State,
                            uptime => diameter_lib:now_diff(Started), peer => Peer}
                      end,
                      ets:tab2list(element(5, S)));
        [] -> undefined
    end.
-endif.
