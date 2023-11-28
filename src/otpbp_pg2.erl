-module(otpbp_pg2).

-ifndef(HAVE_pg2__create_1).
% OTP < 24.0
-export([create/1]).
-endif.
-ifndef(HAVE_pg2__delete_1).
% OTP < 24.0
-export([delete/1]).
-endif.
-ifndef(HAVE_pg2__get_closest_pid_1).
% OTP < 24.0
-export([get_closest_pid/1]).
-endif.
-ifndef(HAVE_pg2__get_local_members_1).
% OTP < 24.0
-export([get_local_members/1]).
-endif.
-ifndef(HAVE_pg2__get_members_1).
% OTP < 24.0
-export([get_members/1]).
-endif.
-ifndef(HAVE_pg2__join_2).
% OTP < 24.0
-export([join/2]).
-endif.
-ifndef(HAVE_pg2__leave_2).
% OTP < 24.0
-export([leave/2]).
-endif.
-ifndef(HAVE_pg2__start_0).
% OTP < 24.0
-export([start/0]).
-endif.
-ifndef(HAVE_pg2__start_link_0).
% OTP < 24.0
-export([start_link/0]).
-endif.
-ifndef(HAVE_pg2__which_groups_0).
% OTP < 24.0
-export([which_groups/0]).
-endif.

-ifndef(HAVE_pg2__create_1).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__delete_1).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__get_closest_pid_1).
-ifdef(HAVE_pg2__get_local_members_1).
-import(pg2, [get_local_members/1]).
-endif.
-ifdef(HAVE_pg2__get_members_1).
-import(pg2, [get_members/1]).
-endif.
-endif.
-ifndef(HAVE_pg2__get_local_members_1).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__get_members_1).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__join_2).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__leave_2).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.
-ifndef(HAVE_pg2__which_groups_0).
-ifdef(HAVE_pg2__start_0).
-import(pg2, [start/0]).
-endif.
-endif.

-ifndef(HAVE_pg2__init_1).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).

-define(GEN_SERVER_MODULE, ?MODULE).
-else.
-define(GEN_SERVER_MODULE, pg2).
-endif.

-define(SERVICE, pg2).
-record(local_member, {name, pid}).

-ifndef(HAVE_pg2__create_1).
create(Name) ->
    start(),
    ets:member(pg2_table, {group, Name}) orelse
        global:trans({{?SERVICE, Name}, self()}, fun() -> gen_server:multi_call(?SERVICE, {create, Name}) end),
    ok.
-endif.

-ifndef(HAVE_pg2__delete_1).
delete(Name) ->
    start(),
    global:trans({{?SERVICE, Name}, self()}, fun() -> gen_server:multi_call(?SERVICE, {delete, Name}) end),
    ok.
-endif.

-ifndef(HAVE_pg2__get_closest_pid_1).
get_closest_pid(Name) ->
    case get_local_members(Name) of
        [Pid] -> Pid;
        [] ->
            case get_members(Name) of
                [] -> {error, {no_process, Name}};
                Members -> random_element(Members)
            end;
        Members when is_list(Members) -> random_element(Members);
        Else -> Else
    end.

random_element(List) -> lists:nth(abs(erlang:monotonic_time() bxor erlang:unique_integer()) rem length(List) + 1, List).
-endif.

-ifndef(HAVE_pg2__get_local_members_1).
get_local_members(Name) ->
    start(),
    case ets:member(pg2_table, {group, Name}) of
        true -> local_group_members(Name);
        _false -> {error, {no_such_group, Name}}
    end.

-compile({inline, local_group_members/1}).
local_group_members(Name) ->
    [P || [Pid] <- ets:match(pg2_table, {#local_member{name = Name, pid = '$1'}}), P <- member_in_group(Pid, Name)].

-ifndef(NEED_member_in_group_2).
-define(NEED_member_in_group_2, true).
-endif.
-endif.

-ifndef(HAVE_pg2__get_members_1).
get_members(Name) ->
    start(),
    case ets:member(pg2_table, {group, Name}) of
        true -> group_members(Name);
        _false -> {error, {no_such_group, Name}}
    end.

-ifndef(NEED_group_members_1).
-define(NEED_group_members_1, true).
-endif.
-endif.

-ifndef(HAVE_pg2__join_2).
join(Name, Pid) -> act(Name, Pid, join).

-ifndef(NEED_act_3).
-define(NEED_act_3, true).
-endif.
-endif.

-ifndef(HAVE_pg2__leave_2).
leave(Name, Pid) -> act(Name, Pid, leave).

-ifndef(NEED_act_3).
-define(NEED_act_3, true).
-endif.
-endif.

-ifndef(HAVE_pg2__start_0).
start() ->
    case whereis(?SERVICE) of
        undefined ->
            supervisor:start_child(kernel_safe_sup,
                                   #{id => pg2, start => {?GEN_SERVER_MODULE, start_link, []}, shutdown => 1000});
        Pid -> {ok, Pid}
    end.
-endif.

-ifndef(HAVE_pg2__start_link_0).
start_link() -> gen_server:start_link({local, ?SERVICE}, ?GEN_SERVER_MODULE, [], []).
-endif.

-ifndef(HAVE_pg2__which_groups_0).
which_groups() ->
    start(),
    all_groups().

-ifndef(NEED_all_groups_0).
-define(NEED_all_groups_0, true).
-endif.
-endif.

-ifndef(HAVE_pg2__init_1).
-record(state, {}).
-record(group, {name}).
-record(ref, {v}).

init([]) ->
    ok = net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
                     {?SERVICE, N} ! {new_pg2, node()},
                     self() ! {nodeup, N}
                  end, nodes()),
    pg2_table = ets:new(pg2_table, [ordered_set, protected, named_table]),
    {ok, #state{}}.

handle_call({create, Name}, _From, S) ->
    assure_group(Name),
    {reply, ok, S};
handle_call({join, Name, Pid}, _From, S) ->
    ets:member(pg2_table, {group, Name}) andalso join_group(Name, Pid),
    {reply, ok, S};
handle_call({leave, Name, Pid}, _From, S) ->
    ets:member(pg2_table, {group, Name}) andalso leave_group(Name, Pid),
    {reply, ok, S};
handle_call({delete, Name}, _From, S) ->
    lists:foreach(fun(Pid) -> leave_group(Name, Pid) end, group_members(Name)),
    true = ets:delete(pg2_table, #group{name = Name}),
    {reply, ok, S};
handle_call(Request, From, S) ->
    error_logger:warning_msg("The pg2 server received an unexpected message:\nhandle_call(~tp, ~tp, _)\n",
                             [Request, From]),
    {noreply, S}.

handle_cast({exchange, _Node, Names}, S) ->
    lists:foreach(fun([Name, Members]) ->
                      assure_group(Name) andalso lists:foreach(fun(P) -> join_group(Name, P) end,
                                                               Members -- group_members(Name));
                     (_) -> ok
                  end,
                  Names),
    {noreply, S};
handle_cast(_, S) -> {noreply, S}.

-record(pid, {pid, name}).

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S) ->
    [{_, Pid}] = ets:lookup(pg2_table, #ref{v = MonitorRef}),
    Names = all_matches({#pid{pid = Pid, name = '$1'}}),
    _ = [leave_group(Name, P) || Name <- Names, P <- member_in_group(Pid, Name)],
    lists:foreach(fun(Name) -> gen_server:abcast(nodes(), ?SERVICE, {del_member, Name, Pid}) end, Names),
    {noreply, S};
handle_info({T, Node}, S) when T =:= nodeup; T =:= new_pg2 ->
    gen_server:cast({?SERVICE, Node}, {exchange, node(), [[G, group_members(G)] || G <- all_groups()]}),
    {noreply, S};
handle_info(_, S) -> {noreply, S}.

terminate(_Reason, _S) -> ets:delete(pg2_table).

assure_group(Name) ->
    Key = #group{name = Name},
    ets:member(pg2_table, Key) orelse true =:= ets:insert(pg2_table, {Key}).

-record(member, {name, pid}).

join_group(Name, Pid) ->
    Ref_Pid = #ref{v = Pid},
    try ets:update_counter(pg2_table, Ref_Pid, {4, 1}) of
        _ -> true
    catch
        _:_ ->
            {RPid, Ref} = case lists:member(node(Pid), nodes([this, visible])) of
                              true -> {Pid, monitor(process, Pid)};
                              _false ->
                                  spawn_monitor(fun() ->
                                                    Ref = monitor(process, Pid),
                                                    receive
                                                        {'DOWN', Ref, process, Pid, _Info} -> exit(normal)
                                                    end
                                                end)
                          end,
            true = ets:insert(pg2_table, [{Ref_Pid, RPid, Ref, 1}, {#ref{v = Ref}, Pid}])
    end,
    Member = #member{name = Name, pid = Pid},
    try
        ets:update_counter(pg2_table, Member, {2, 1})
    catch _:_ ->
        true = ets:insert(pg2_table, [{Member, 1}, {#pid{pid = Pid, name = Name}}]),
        node(Pid) =:= node() andalso ets:insert(pg2_table, {#local_member{name = Name, pid = Pid}})
    end.

leave_group(Name, Pid) ->
    Member = #member{name = Name, pid = Pid},
    try ets:update_counter(pg2_table, Member, {2, -1}) of
        N ->
            N =:= 0 orelse
                begin
                true = ets:delete(pg2_table, #pid{pid = Pid, name = Name}),
                node(Pid) =:= node() andalso ets:delete(pg2_table, #local_member{name = Name, pid = Pid}),
                true = ets:delete(pg2_table, Member)
                end,
            Ref_Pid = #ref{v = Pid},
            case ets:update_counter(pg2_table, Ref_Pid, {4, -1}) of
                0 ->
                    [{#ref{}, RPid, Ref, 0}] = ets:lookup(pg2_table, Ref_Pid),
                    true = ets:delete(pg2_table, #ref{v = Ref}),
                    true = ets:delete(pg2_table, Ref_Pid),
                    true = demonitor(Ref, [flush]),
                    RPid =:= Pid orelse exit(RPid, kill);
                _ -> ok
            end
    catch _:_ -> ok
    end.

-ifndef(NEED_member_in_group_2).
-define(NEED_member_in_group_2, true).
-endif.
-ifndef(NEED_all_matches_1).
-define(NEED_all_matches_1, true).
-endif.
-ifndef(NEED_all_groups_0).
-define(NEED_all_groups_0, true).
-endif.
-ifndef(NEED_group_members_1).
-define(NEED_group_members_1, true).
-endif.
-endif.

-ifdef(NEED_act_3).
act(Name, Pid, Action) when is_pid(Pid) ->
    start(),
    case ets:member(pg2_table, {group, Name}) of
        false -> {error, {no_such_group, Name}};
        true ->
            global:trans({{?SERVICE, Name}, self()},
                         fun() -> gen_server:multi_call(?SERVICE, {Action, Name, Pid}) end),
            ok
    end.
-endif.

-ifdef(NEED_group_members_1).
group_members(Name) ->
    [P || [P, N] <- ets:match(pg2_table, {#member{name = Name, pid = '$1'}, '$2'}), _ <- lists:seq(1, N)].
-endif.

-ifdef(NEED_member_in_group_2).
member_in_group(Pid, Name) ->
    case ets:lookup(pg2_table, #member{name = Name, pid = Pid}) of
        [] -> [];
        [{#member{}, N}] -> lists:duplicate(N, Pid)
    end.
-endif.

-ifdef(NEED_all_groups_0).
all_groups() -> all_matches({#group{name = '$1'}}).

-ifndef(NEED_all_matches_1).
-define(NEED_all_matches_1, true).
-endif.
-endif.

-ifdef(NEED_all_matches_1).
all_matches(Spec) -> [N || [N] <- ets:match(pg2_table, Spec)].
-endif.
