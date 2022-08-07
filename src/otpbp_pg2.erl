%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(otpbp_pg2).

-ifndef(HAVE_pg2__start_0).
-export([create/1,
         delete/1,
         join/2,
         leave/2,
         get_members/1,
         get_local_members/1,
         get_closest_pid/1,
         which_groups/0]).
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%% As of R13B03 monitors are used instead of links.

%%%
%%% Exported functions
%%%

-define(SERVICE, pg2).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?SERVICE}, ?MODULE, [], []).

-spec start() -> {ok, pid()} | {error, any()}.
start() -> ensure_started().

-type name() :: any().

-spec create(Name :: name()) -> ok.
create(Name) ->
    _ = ensure_started(),
    ets:member(pg2_table, {group, Name}) orelse
        global:trans({{?SERVICE, Name}, self()}, fun() -> gen_server:multi_call(?SERVICE, {create, Name}) end),
    ok.

-spec delete(Name :: name()) -> ok.
delete(Name) ->
    _ = ensure_started(),
    global:trans({{?SERVICE, Name}, self()}, fun() -> gen_server:multi_call(?SERVICE, {delete, Name}) end),
    ok.

-spec join(Name, Pid :: pid()) -> ok | {error, {no_such_group, Name}} when Name :: name().
join(Name, Pid) -> act(Name, Pid, join).

-spec leave(Name, Pid :: pid()) -> ok | {error, {no_such_group, Name}} when Name :: name().
leave(Name, Pid) -> act(Name, Pid, leave).

act(Name, Pid, Action) when is_pid(Pid) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        false -> {error, {no_such_group, Name}};
        true ->
            global:trans({{?SERVICE, Name}, self()},
                         fun() -> gen_server:multi_call(?SERVICE, {Action, Name, Pid}) end),
            ok
    end.

-spec get_members(Name) -> [pid()] | {error, {no_such_group, Name}} when Name :: name().
get_members(Name) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        true -> group_members(Name);
        false -> {error, {no_such_group, Name}}
    end.

-spec get_local_members(Name) -> [pid()] | {error, {no_such_group, Name}} when Name :: name().
get_local_members(Name) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        true -> local_group_members(Name);
        false -> {error, {no_such_group, Name}}
    end.

-spec which_groups() -> [Name :: name()].
which_groups() ->
    _ = ensure_started(),
    all_groups().

-spec get_closest_pid(Name :: name()) ->  pid() | {error, {no_process, Name} | {no_such_group, Name}}
        when Name :: name().
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

%%%
%%% Callback functions from gen_server
%%%

-record(state, {}).

-type state() :: #state{}.

-spec init(Arg :: []) -> {ok, state()}.
init([]) ->
    Ns = nodes(),
    ok = net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
                     {?SERVICE, N} ! {new_pg2, node()},
                     self() ! {nodeup, N}
                  end, Ns),
    pg2_table = ets:new(pg2_table, [ordered_set, protected, named_table]),
    {ok, #state{}}.

-spec handle_call({create, Name} | {delete, Name} | {join, Name, Pid :: pid()} | {leave, Name, Pid :: pid()},
                  From :: {pid(), Tag :: any()}, State :: state()) -> {reply, ok, state()}
        when Name :: name().
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
    delete_group(Name),
    {reply, ok, S};
handle_call(Request, From, S) ->
    error_logger:warning_msg("The pg2 server received an unexpected message:\n"
                             "handle_call(~tp, ~tp, _)\n",
                             [Request, From]),
    {noreply, S}.

-spec handle_cast({exchange, node(), Names :: [[name(),...]]} | {del_member, name(), pid()},
                  State :: state()) -> {noreply, state()}.
handle_cast({exchange, _Node, Names}, S) ->
    store(Names),
    {noreply, S};
handle_cast(_, S) ->
    %% Ignore {del_member, Name, Pid}.
    {noreply, S}.

-spec handle_info(Tuple :: tuple(), State :: state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S) ->
    member_died(MonitorRef),
    {noreply, S};
handle_info({T, Node}, S) when T =:= nodeup; T =:= new_pg2 ->
    gen_server:cast({?SERVICE, Node}, {exchange, node(), all_members()}),
    {noreply, S};
handle_info(_, S) -> {noreply, S}.

-spec terminate(Reason :: any(), State :: state()) -> ok.
terminate(_Reason, _S) ->
    true = ets:delete(pg2_table),
    ok.

%%%
%%% Local functions
%%%

-record(member, {name, pid}).
-record(local_member, {name, pid}).
-record(group, {name}).
-record(pid, {pid, name}).
-record(ref, {v}).

%%% One ETS table, pg2_table, is used for bookkeeping. The type of the
%%% table is ordered_set, and the fast matching of partially
%%% instantiated keys is used extensively.
%%%
%%% {#group{name = Name}}
%%%    Process group Name.
%%% {#ref{v = Pid}, RPid, MonitorRef, Counter}
%%% {#ref{v = MonitorRef}, Pid}
%%%    Each process has one monitor. Sometimes a process is spawned to
%%%    monitor the pid (RPid). Counter is incremented when the Pid joins
%%%    some group.
%%% {#member{name = Name, pid = Pid}, GroupCounter}
%%% {#local_member{name = Name, pid = Pid}}
%%%    Pid is a member of group Name, GroupCounter is incremented when the
%%%    Pid joins the group Name.
%%% {#pid{pid = Pid, name = Name}}
%%%    Pid is a member of group Name.

store(List) ->
    _ = [(assure_group(Name) andalso [join_group(Name, P) || P <- Members -- group_members(Name)]) || [Name, Members] <- List],
    ok.

assure_group(Name) ->
    Key = #group{name = Name},
    ets:member(pg2_table, Key) orelse true =:= ets:insert(pg2_table, {Key}).

delete_group(Name) ->
    lists:foreach(fun(Pid) -> leave_group(Name, Pid) end, group_members(Name)),
    true = ets:delete(pg2_table, #group{name = Name}),
    ok.

member_died(Ref) ->
    [{#ref{}, Pid}] = ets:lookup(pg2_table, #ref{v = Ref}),
    Names = member_groups(Pid),
    _ = [leave_group(Name, P) || Name <- Names, P <- member_in_group(Pid, Name)],
    %% Kept for backward compatibility with links. Can be removed, eventually.
    lists:foreach(fun(Name) -> gen_server:abcast(nodes(), ?SERVICE, {del_member, Name, Pid}) end, Names),
    ok.

join_group(Name, Pid) ->
    Ref_Pid = #ref{v = Pid},
    try ets:update_counter(pg2_table, Ref_Pid, {4, +1}) of
        _ -> true
    catch _:_ ->
        {RPid, Ref} = do_monitor(Pid),
        true = ets:insert(pg2_table, [{Ref_Pid, RPid, Ref, 1}, {#ref{v = Ref}, Pid}])
    end,
    Member = #member{name = Name, pid = Pid},
    try
        ets:update_counter(pg2_table, Member, {2, +1})
    catch _:_ ->
        true = ets:insert(pg2_table, [{Member, 1}, {#pid{pid = Pid, name = Name}}]),
        node(Pid) =:= node() andalso ets:insert(pg2_table, {#local_member{name = Name, pid = Pid}})
    end.

leave_group(Name, Pid) ->
    Member = #member{name = Name, pid = Pid},
    try ets:update_counter(pg2_table, Member, {2, -1}) of
        N ->
            ensure_delete_group(Name, Pid, Member, N),
            Ref_Pid = #ref{v = Pid},
            case ets:update_counter(pg2_table, Ref_Pid, {4, -1}) of
                0 ->
                    [{#ref{}, RPid, Ref, 0}] = ets:lookup(pg2_table, Ref_Pid),
                    true = ets:delete(pg2_table, #ref{v = Ref}),
                    true = ets:delete(pg2_table, Ref_Pid),
                    true = erlang:demonitor(Ref, [flush]),
                    kill_monitor_proc(RPid, Pid);
                _ -> ok
            end
    catch _:_ -> ok
    end.

ensure_delete_group(_Name, _Pid, _Member, 0) -> true;
ensure_delete_group(Name, Pid, Member, _) ->
    true = ets:delete(pg2_table, #pid{pid = Pid, name = Name}),
    node(Pid) =:= node() andalso ets:delete(pg2_table, #local_member{name = Name, pid = Pid}),
    true = ets:delete(pg2_table, Member).

all_members() -> [[G, group_members(G)] || G <- all_groups()].

group_members(Name) ->
    [P || [P, N] <- ets:match(pg2_table, {#member{name = Name, pid = '$1'}, '$2'}), _ <- lists:seq(1, N)].

local_group_members(Name) ->
    [P || [Pid] <- ets:match(pg2_table, {#local_member{name = Name, pid = '$1'}}), P <- member_in_group(Pid, Name)].

member_in_group(Pid, Name) ->
    case ets:lookup(pg2_table, #member{name = Name, pid = Pid}) of
        [] -> [];
        [{#member{}, N}] -> lists:duplicate(N, Pid)
    end.

member_groups(Pid) -> all_matches({#pid{pid = Pid, name = '$1'}}).

all_groups() -> all_matches({#group{name = '$1'}}).

all_matches(Spec) -> [N || [N] <- ets:match(pg2_table, Spec)].

ensure_started() ->
    case whereis(?SERVICE) of
        undefined ->
            supervisor:start_child(kernel_safe_sup,
                                   {pg2, {?MODULE, start_link, []}, permanent, 1000, worker, [?MODULE]});
        Pg2Pid -> {ok, Pg2Pid}
    end.

kill_monitor_proc(Pid, Pid) -> ok;
kill_monitor_proc(RPid, _Pid) -> exit(RPid, kill).

%% When/if erlang:monitor() returns before trying to connect to the
%% other node this function can be removed.
do_monitor(Pid) ->
    case lists:member(node(Pid), nodes([this, visible])) of
        true ->
            %% Assume the node is still up
            {Pid, monitor(process, Pid)};
        false ->
            spawn_monitor(fun() ->
                              Ref = monitor(process, Pid),
                              receive
                                  {'DOWN', Ref, process, Pid, _Info} -> exit(normal)
                              end
                          end)
    end.
-endif.
