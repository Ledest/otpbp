-module(otpbp_graph).

-ifndef(HAVE_graph__new_0).
%% Basic functionality
-export([new/0, new/1,
         info/1,
         no_edges/1,
         source_vertices/1,
         sink_vertices/1,
         add_vertex/1, add_vertex/2, add_vertex/3,
         add_edge/3, add_edge/4,
         del_edge/2,
         del_edges/2, del_edges/3,
         del_vertex/2,
         del_vertices/2,
         edges/1, edges/2, edges/3,
         has_vertex/2,
         has_edge/2, has_edge/3,
         has_path/3,
         del_path/3,
         get_path/3,
         get_cycle/2,
         get_short_path/3,
         get_short_cycle/2,
         in_degree/2,
         in_edges/2,
         in_neighbours/2,
         no_vertices/1,
         out_degree/2,
         out_edges/2,
         out_neighbours/2,
         vertex/2, vertex/3,
         vertices/1,
         vertices_with_labels/1,
         fold_vertices/3]).

%% Utilities
-export([components/1,
         strong_components/1,
         cyclic_strong_components/1,
         reachable/2,
         reachable_via_neighbours/2,
         reaching/2,
         reaching_via_neighbours/2,
         topsort/1,
         is_acyclic/1,
         roots/1,
         arborescence_root/1,
         is_arborescence/1,
         is_tree/1,
         loop_vertices/1,
         subgraph/2, subgraph/3,
         condensation/1,
         preorder/1, preorder/2,
         postorder/1, postorder/2,
         reverse_postorder/1,
         reverse_postorder/2]).

-export_type([graph/0, graph_type/0, graph_cyclicity/0, vertex/0, edge/0, label/0]).

-type edge_map() :: #{vertex() => ordsets:ordset(vertex())}.
-type vertice_map() :: #{vertex() => label()}.

-record(graph, {vs = #{} :: vertice_map(),
                in_es = #{} :: edge_map(),
                out_es = #{} :: edge_map(),
                cyclic = true :: boolean(),
                next_vid = 0 :: non_neg_integer()}).

-type graph() :: #graph{}.
-type vertex() :: term().
-type label() :: term().
-type edge() :: {vertex(), vertex(), label()}.
-type graph_cyclicity() :: acyclic | cyclic.
-type graph_type() :: graph_cyclicity().

-spec new() -> graph().
new() -> #graph{}.

-spec new(Options::[graph_type()]) -> graph().
new(Options) when is_list(Options) -> new(Options, #graph{}).

new([cyclic|Opts], G) -> new(Opts, G#graph{cyclic = true});
new([acyclic|Opts], G) -> new(Opts, G#graph{cyclic = false});
new([], G) -> G;
new(_, _) -> error(badarg).

-spec info(graph()) -> [{'cyclicity', graph_cyclicity()}].
info(#graph{cyclic = true}) -> [{cyclicity, cyclic}];
info(#graph{cyclic = false}) -> [{cyclicity, acyclic}].

-spec add_vertex(G::graph()) -> {vertex(), graph()}.
add_vertex(#graph{next_vid = V} = G) -> {V, add_vertex(G#graph{next_vid = V + 1}, V)}.

-spec add_vertex(G::graph(), V::vertex()) -> graph().
add_vertex(G, V) -> add_vertex(G, V, []).

-spec add_vertex(G::graph(), V::vertex(), L::label()) -> graph().
add_vertex(#graph{vs = Vs} = G, V, L) -> G#graph{vs = Vs#{V => L}}.

-spec del_vertex(G::graph(), V::vertex()) -> graph().
del_vertex(#graph{vs = Vs, in_es = InEsMap, out_es = OutEsMap} = G, V) ->
    G#graph{vs = maps:remove(V, Vs),
            in_es = lists:foldl(fun({_, To, _} = E, A) -> edge_map_del(To, E, A) end,
                                maps:remove(V, InEsMap), maps:get(V, OutEsMap, [])),
            out_es = lists:foldl(fun({From, _, _} = E, A) -> edge_map_del(From, E, A) end,
                                 maps:remove(V, OutEsMap), maps:get(V, InEsMap, []))}.

-spec del_vertices(G::graph(), Vs::[vertex()]) -> graph().
del_vertices(G, Vs) -> lists:foldl(fun(V, A) -> del_vertex(A, V) end, G, Vs).

-spec vertex(graph(), V::vertex()) -> label().
vertex(#graph{vs = Vs}, V) -> maps:get(V, Vs).

-spec vertex(graph(), V::vertex(), Default::label()) -> label().
vertex(#graph{vs = Vs}, V, Default) -> maps:get(V, Vs, Default).

-spec no_vertices(graph()) -> non_neg_integer().
no_vertices(#graph{vs = Vs}) -> map_size(Vs).

-spec vertices(graph()) -> [vertex()].
vertices(#graph{vs = Vs}) -> maps:keys(Vs).

-spec vertices_with_labels(graph()) -> [{vertex(), label()}].
vertices_with_labels(#graph{vs = Vs}) -> maps:to_list(Vs).

-spec source_vertices(graph()) -> [vertex()].
source_vertices(#graph{vs = Vs, in_es = InEsMap}) -> vertices(Vs, InEsMap).

-spec sink_vertices(graph()) -> [vertex()].
sink_vertices(#graph{vs = Vs, out_es = OutEsMap}) -> vertices(Vs, OutEsMap).

vertices(Vs, EsMap) -> lists:filter(fun(V) -> maps:get(V, EsMap, []) =:= [] end, maps:keys(Vs)).

-spec in_degree(graph(), V::vertex()) -> non_neg_integer().
in_degree(#graph{in_es = InEsMap}, V) -> length(maps:get(V, InEsMap, [])).

-spec in_neighbours(graph(), V::vertex()) -> [vertex()].
in_neighbours(#graph{in_es = InEsMap}, V) -> lists:map(fun({From, _, _}) -> From end, maps:get(V, InEsMap, [])).

-spec in_edges(graph(), V::vertex()) -> [edge()].
in_edges(#graph{in_es = InEsMap}, V) -> maps:get(V, InEsMap, []).

-spec out_degree(graph(), V::vertex()) -> non_neg_integer().
out_degree(#graph{out_es = OutEsMap}, V) -> length(maps:get(V, OutEsMap, [])).

-spec out_neighbours(graph(), V::vertex()) -> [vertex()].
out_neighbours(#graph{out_es = OutEsMap}, V) -> lists:map(fun({_, To, _}) -> To end, maps:get(V, OutEsMap, [])).

-spec out_edges(graph(), V::vertex()) -> [edge()].
out_edges(#graph{out_es = OutEsMap}, V) -> maps:get(V, OutEsMap, []).

-spec add_edge(C::graph(), V1::vertex(), V2::vertex()) -> graph().
add_edge(G, V1, V2) -> add_edge(G, V1, V2, []).

-spec add_edge(G::graph(), V1::vertex(), V2::vertex(), L::label()) -> graph().
add_edge(#graph{cyclic = true} = G, From, To, Label) -> do_add_edge(G, From, To, Label);
add_edge(#graph{cyclic = false} = G, From, To, Label) -> acyclic_add_edge(G, From, To, Label).

do_add_edge(#graph{vs = Vs, in_es = InEsMap, out_es = OutEsMap} = G, From, To, Label) ->
    maps:is_key(From, Vs) orelse error({bad_vertex, From}),
    maps:is_key(To, Vs) orelse error({bad_vertex, To}),
    Name = {From, To, Label},
    G#graph{in_es = edge_map_add(To, Name, InEsMap), out_es = edge_map_add(From, Name, OutEsMap)}.

edge_map_add(V, E, EsMap) -> EsMap#{V => ordsets:add_element(E, maps:get(V, EsMap, []))}.

acyclic_add_edge(_G, From, From, _Label) -> error({bad_edge, {From, From}});
acyclic_add_edge(G, From, To, Label) ->
    case get_path(G, To, From) of
        false ->
            false = has_path(G, To, From),  % assert - remove me
            do_add_edge(G, From, To, Label);
        _ ->
            true = has_path(G, To, From),  % assert - remove me
            error({bad_edge, {From,To}})
    end.

-spec del_edge(G::graph(), E::edge()) -> graph().
del_edge(#graph{in_es = InEsMap, out_es = OutEsMap} = G, {From, To, _} = E) ->
    G#graph{in_es = edge_map_del(To, E, InEsMap), out_es = edge_map_del(From, E, OutEsMap)}.

edge_map_del(V, E, EsMap) ->
    case EsMap of
        #{V := Es} -> EsMap#{V := lists:delete(E, Es)};
        #{} -> EsMap
    end.

-spec del_edges(C::graph(), Es::[edge()]) -> graph().
del_edges(G, Es) when is_list(Es) -> lists:foldl(fun(E, A) -> del_edge(A, E) end, G, Es).

-spec del_edges(G::graph(), V1::vertex(), V2::vertex()) -> graph().
del_edges(G, V1, V2) -> del_edges(G, V1, V2, out_edges(G, V1)).

del_edges(G, V1, V2, [{V1, V2, _} = E|Es]) -> del_edges(del_edge(G, E), V1, V2, Es);
del_edges(G, V1, V2, [_|Es]) -> del_edges(G, V1, V2, Es);
del_edges(G, _, _, []) -> G.

-spec no_edges(graph()) -> non_neg_integer().
no_edges(#graph{out_es = OutEsMap}) -> maps:fold(fun(_, Es, A) -> A + length(Es) end, 0, OutEsMap).

-spec edges(graph()) -> [edge()].
edges(#graph{out_es = OutEsMap}) -> maps:fold(fun(_, Es, Acc) -> Es ++ Acc end, [], OutEsMap).

-spec edges(graph(), V::vertex()) -> [edge()].
edges(#graph{in_es = InEsMap, out_es = OutEsMap}, V) -> maps:get(V, OutEsMap, []) ++ maps:get(V, InEsMap, []).

-spec edges(graph(), V1::vertex(), V2::vertex()) -> ordsets:ordset(edge()).
edges(#graph{out_es = OutEsMap}, V1, V2) ->
    case OutEsMap of
       #{V1 := Es} -> [E || {Va, Vb, _} = E <- Es, Va =:= V1, Vb =:= V2];
       #{} -> []
   end.

-spec has_edge(graph(), E::edge()) -> boolean().
has_edge(#graph{out_es = OutEsMap}, {V1, _, _} = E) ->
    case OutEsMap of
       #{V1 := Es} -> ordsets:is_element(E, Es);
       #{} -> false
   end.

-spec has_edge(graph(), V1::vertex(), V2::vertex()) -> boolean().
has_edge(#graph{out_es = OutEsMap}, V1, V2) ->
    case OutEsMap of
       #{V1 := Es} -> has_edge_(Es, V1, V2);
       #{} -> false
   end.

has_edge_([{V1, V2, _L}|_], V1, V2) -> true;
has_edge_([_|Es], V1, V2) -> has_edge_(Es, V1, V2);
has_edge_([], _, _) -> false.

-spec fold_vertices(graph(), Fun::fun((vertex(), label(), any()) -> any()), Acc::any()) -> any().
fold_vertices(#graph{vs = Vs}, Fun, Acc) -> maps:fold(Fun, Acc, Vs).

-spec has_vertex(graph(), V::vertex()) -> boolean().
has_vertex(#graph{vs = Vs}, V) -> maps:is_key(V, Vs).

-spec has_path(G::graph(), From::vertex(), To::vertex()) -> boolean().
has_path(G, From, To) ->
    Seen = sets_new(),
    L = [From],
    try has_path(L, To, G, Seen) of
        _ -> false
    catch
        throw:true -> true
    end.

has_path([To|_], To, _G, _Seen) -> throw(true);
has_path([V|Vs], To, G, Seen) ->
    case sets:is_element(V, Seen) of
        true -> has_path(Vs, To, G, Seen);
        false -> has_path(Vs, To, G, has_path(out_neighbours(G, V), To, G, sets:add_element(V, Seen)))
    end;
has_path([], _To, _G, Seen) -> Seen.

-spec del_path(G::graph(), V1::vertex(), V2::vertex()) -> graph().
del_path(G, V1, V2) ->
    case get_path(G, V1, V2) of
        false -> G;
        Path -> del_path(del_path_edges(G, Path), V1, V2)
    end.

del_path_edges(G, [V1, V2|Vs]) -> del_path_edges(del_edges(G, V1, V2), [V2|Vs]);
del_path_edges(G, _) -> G.

-spec get_cycle(G::graph(), V::vertex()) -> [vertex(), ...] | false.
get_cycle(G, V) ->
    VL = [V],
    case one_path(out_neighbours(G, V), V, [], VL, VL, 2, G, 1) of
        false -> lists:member(V, out_neighbours(G, V)) andalso VL;
        Vs -> Vs
    end.

-spec get_path(G::graph(), V1::vertex(), V2::vertex()) -> [vertex(), ...] | false.
get_path(G, V1, V2) ->
    V1L = [V1],
    one_path(out_neighbours(G, V1), V2, [], V1L, V1L, 1, G, 1).

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G, Counter) when Counter < Prune ->
    one_path(Ws, W, Cont, Xs, Ps, Prune, G, Counter);
one_path([W|_], W, _, _, Ps, _, _, _) -> lists:reverse(Ps, [W]);
one_path([V|Vs], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case lists:member(V, Xs) of
        true -> one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter);
        false -> one_path(out_neighbours(G, V), W, [{Vs, Ps}|Cont], [V|Xs], [V|Ps], Prune, G, Counter + 1)
    end;
one_path([], W, [{Vs, Ps}|Cont], Xs, _, Prune, G, Counter) -> one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter - 1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

-spec get_short_cycle(G::graph(), V::vertex()) -> [vertex(), ...] | false.
get_short_cycle(G, V) -> get_short_path(G, V, V).

-spec get_short_path(G::graph(), V1::vertex(), V2::vertex()) -> [vertex(), ...] | false.
get_short_path(G, V1, V2) -> spath(queue_out_neighbours(V1, G, queue:new()), G, V2, add_vertex(new(), V1)).

spath(Q, G, Sink, T) ->
    case queue:out(Q) of
        {{value, {V1, Sink, _Label}}, _Q1} -> follow_path(V1, T, [Sink]);
        {{value, {V1, V2, _Label}}, Q1} ->
            case has_vertex(T, V2) of
                false -> spath(queue_out_neighbours(V2, G, Q1), G, Sink, add_edge(add_vertex(T, V2), V2, V1));
                true -> spath(Q1, G, Sink, T)
            end;
        {empty, _Q1} -> false
    end.

follow_path(V, T, P) ->
    P1 = [V|P],
    case out_neighbours(T, V) of
        [N] -> follow_path(N, T, P1);
        [] -> P1
    end.

queue_out_neighbours(V, G, Q) -> lists:foldl(fun queue:in/2, Q, out_edges(G, V)).

%% ------------------------------------------------------------------------
%% Graph utilities

%%% Operations on directed (and undirected) graphs.
%%%
%%% Implementation based on Launchbury, John: Graph Algorithms with a Functional Flavour,
%%% in Jeuring, Johan, and Meijer, Erik (Eds.):
%%% Advanced Functional Programming, Lecture Notes in Computer Science 925, Springer Verlag, 1995.

%%
%%  Exported functions
%%

-spec components(G::graph()) -> [[vertex()]].
components(G) -> revpreorders(G, fun inout/3).

-spec strong_components(G::graph()) -> [[vertex()]].
strong_components(G) -> revpreorders(G, fun in/3, reverse_postorder(G, vertices(G))).

-spec cyclic_strong_components(G::graph()) -> [[vertex()]].
cyclic_strong_components(G) -> remove_singletons(strong_components(G), G, []).

-spec reachable(G::graph(), Vs::[vertex()]) -> [vertex()].
reachable(G, Vs) when is_list(Vs) -> lists:append(revpreorders(G, fun out/3, Vs, first)).

-spec reachable_via_neighbours(G::graph(), Vs::[vertex()]) -> [vertex()].
reachable_via_neighbours(G, Vs) when is_list(Vs) -> lists:append(revpreorders(G, fun out/3, Vs, not_first)).

-spec reaching(G::graph(), Vs::[vertex()]) -> [vertex()].
reaching(G, Vs) when is_list(Vs) -> lists:append(revpreorders(G, fun in/3, Vs, first)).

-spec reaching_via_neighbours(G::graph(), Vs::[vertex()]) -> [vertex()].
reaching_via_neighbours(G, Vs) when is_list(Vs) -> lists:append(revpreorders(G, fun in/3, Vs, not_first)).

-spec topsort(G::graph()) -> [vertex()].
topsort(G) -> reverse_postorder(G).

-spec is_acyclic(G::graph()) -> boolean().
is_acyclic(G) -> cyclic_strong_components(G) =:= [].

-spec roots(G::graph()) -> [vertex()].
roots(G) ->
    lists:foldr(fun(V, A) ->
                    case in_degree(G, V) of
                        0 -> [V|A];
                        _ -> A
                    end
                end,
                [X || [X|_] <- cyclic_strong_components(G)], vertices(G)).

-spec arborescence_root(G::graph()) -> no | {yes, vertex()}.
arborescence_root(G) ->
    case no_vertices(G) - no_edges(G) of
        1 ->
            F = fun(V, Z) ->
                    case in_degree(G, V) of
                        1 -> Z;
                        0 when Z =:= [] -> [V]
                    end
                end,
            try lists:foldl(F, [], vertices(G)) of
                [Root] -> {yes, Root};
                _ -> no
            catch
                _:_ -> no
            end;
        _ -> no
    end.

-spec is_arborescence(G::graph()) -> boolean().
is_arborescence(G) -> arborescence_root(G) =/= no.

-spec is_tree(G::graph()) -> boolean().
is_tree(G) ->
    case components(G) of
        [_] -> no_edges(G) =:= no_vertices(G) - 1;
        _ -> false
    end.

-spec loop_vertices(G::graph()) -> [vertex()].
loop_vertices(G) -> [V || V <- vertices(G), is_reflexive_vertex(G, V)].

-spec subgraph(G::graph(), Vs::[vertex()]) -> graph().
subgraph(G, Vs) -> subgraph(G, Vs, []).

-spec subgraph(G::graph(), Vs::[vertex()], Options::[{type, inherit|[graph_type()]}|{keep_labels, boolean()}]) ->
          graph().
subgraph(G, Vs, Options) ->
    try
        subgraph_opts(G, Vs, Options)
    catch
        throw:badarg -> error(badarg)
    end.

-spec condensation(G::graph()) -> graph().
condensation(G) ->
    SCs = strong_components(G),
    %% Each component is assigned a number.
    %% V2I: from vertex to number.
    %% I2C: from number to component.
    {_, V2I, I2C} = lists:foldl(fun(SC, {N, V2I, I2C}) ->
                                    {N + 1, lists:foldl(fun(V, Map) -> Map#{V => N} end, V2I, SC), I2C#{N => SC}}
                                end,
                                {1, #{}, #{}}, SCs),
    lists:foldl(fun(SC, SCG) -> condense(SC, G, SCG, V2I, I2C) end, subgraph_opts(G, [], []), SCs).

-spec preorder(G::graph()) -> [vertex()].
preorder(G) -> preorder(G, roots(G)).

-spec preorder(G::graph(), Vs::[vertex()]) -> [vertex()].
preorder(G, Vs) ->
    {_, Acc} = pretraverse_(Vs, fun out/3, G, sets_new(), [], []),
    lists:reverse(lists:append(Acc)).

-spec postorder(G::graph()) -> [vertex()].
postorder(G) -> postorder(G, roots(G)).

-spec postorder(G::graph(), Vs::[vertex()]) -> [vertex()].
postorder(G, Vs) ->
    {Acc, _} = posttraverse(Vs, G, sets_new(), []),
    lists:reverse(Acc).

-spec reverse_postorder(G::graph()) -> [vertex()].
reverse_postorder(G) -> reverse_postorder(G, roots(G)).

-spec reverse_postorder(G::graph(), Vs::[vertex()]) -> [vertex()].
reverse_postorder(G, Vs) ->
    {L, _} = posttraverse(Vs, G, sets_new(), []),
    L.

%%
%%  Local functions
%%

revpreorders(G, SF) -> revpreorders(G, SF, vertices(G)).

revpreorders(G, SF, Vs) -> revpreorders(G, SF, Vs, first).

%% returns a list of reverse preorder traversals using the given Vs as starting points
%% (if a starting point V has already become visited by a previous traversal it will not be included again)
revpreorders(G, SF, Vs, HandleFirst) ->
    {_, LL} = lists:foldl(fun(V, {T0, LL}) -> pretraverse(HandleFirst, V, SF, G, T0, LL) end, {sets_new(), []}, Vs),
    LL.

pretraverse(first, V, SF, G, T, LL) -> pretraverse_([V], SF, G, T, [], LL);
pretraverse(not_first, V, SF, G, T, LL) ->
    %% used by reachable_neighbours/2 and reaching_neighbours/2
    case sets:is_element(V, T) of
        false -> pretraverse_(SF(G, V, []), SF, G, T, [], LL);
        true  -> {T, LL}
    end.

%% generic preorder traversal loop; given a starting set Vs of vertices of G, T tracks seen vertices,
%% the SF function queues up neighbour vertexes, and the resulting list Rs of reached vertices
%% (in reverse preorder) is prepended onto LL unless it is empty
pretraverse_([V|Vs], SF, G, T, Rs, LL) ->
    case sets:is_element(V, T) of
        false -> pretraverse_(SF(G, V, Vs), SF, G, sets:add_element(V, T), [V|Rs], LL);
        true ->  pretraverse_(Vs, SF, G, T, Rs, LL)
    end;
pretraverse_([], _SF, _G, T, [], LL) -> {T, LL};
pretraverse_([], _SF, _G, T, Rs, LL) -> {T, [Rs|LL]}.

%% similar to pretraverse_ but accumulates onto a single list, in reverse postorder, and only for out-edges
posttraverse([V|Vs], G, T0, Acc0) ->
    case sets:is_element(V, T0) of
        false ->
            {Acc1, T2} = posttraverse(out(G, V, []), G, sets:add_element(V, T0), Acc0),
            posttraverse(Vs, G, T2, [V|Acc1]);
        true -> posttraverse(Vs, G, T0, Acc0)
    end;
posttraverse([], _G, T, Acc) -> {Acc, T}.

in(G, V, Vs) -> in_neighbours(G, V) ++ Vs.

out(G, V, Vs) -> out_neighbours(G, V) ++ Vs.

inout(G, V, Vs) -> in(G, V, out(G, V, Vs)).

remove_singletons([[V] = C|Cs], G, L) ->
    remove_singletons(Cs, G,
                      case is_reflexive_vertex(G, V) of
                          true -> [C|L];
                          false -> L
                      end);
remove_singletons([C|Cs], G, L) -> remove_singletons(Cs, G, [C|L]);
remove_singletons([], _G, L) -> L.

is_reflexive_vertex(G, V) -> lists:member(V, out_neighbours(G, V)).

subgraph_opts(G, Vs, Opts) -> subgraph_opts(Opts, inherit, true, G, Vs).

subgraph_opts([{type, Type}|Opts], _Type0, Keep, G, Vs) when Type =:= inherit; is_list(Type) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([{keep_labels, Keep}|Opts], Type, _Keep0, G, Vs) when is_boolean(Keep) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([], inherit, Keep, G, Vs) ->
    {_, {_, Cyclicity}} = lists:keysearch(cyclicity, 1, info(G)),
    subgraph(G, Vs, [Cyclicity], Keep);
subgraph_opts([], Type, Keep, G, Vs) -> subgraph(G, Vs, Type, Keep);
subgraph_opts(_, _Type, _Keep, _G, _Vs) -> throw(badarg).

subgraph(G, Vs, Type, Keep) ->
    try new(Type) of
        SG0 ->
            SG1 = lists:foldl(fun(V, SG) -> subgraph_vertex(V, G, SG, Keep) end, SG0, Vs),
            lists:foldl(fun(V, SGv) ->
                            lists:foldl(fun(E, SG) -> subgraph_edge(E, SG, Keep) end, SGv, out_edges(G, V))
                        end,
                        SG1, vertices(SG1))
    catch
        error:badarg -> throw(badarg)
    end.

subgraph_vertex(V, G, SG, Keep) ->
    case has_vertex(G, V) of
        false -> SG;
        true when not Keep -> add_vertex(SG, V);
        true when Keep -> add_vertex(SG, V, vertex(G, V))
    end.

subgraph_edge({V1, V2, Label}, SG, Keep) ->
    case has_vertex(SG, V2) of
        false -> SG;
        true when not Keep -> add_edge(SG, V1, V2);
        true when Keep -> add_edge(SG, V1, V2, Label)
    end.

condense(SC, G, SCG, V2I, I2C) ->
    maps:fold(fun(I, true, SCG0) ->
                  case I2C of
                      #{I := SC} -> add_vertex(SCG0, SC);
                      #{I := C} -> add_edge(add_vertex(SCG0, C), SC, C)
                  end
              end,
              add_vertex(SCG, SC),
              lists:foldl(fun(V, T) ->
                              lists:foldl(fun(Neighbour, A) -> A#{maps:get(Neighbour, V2I) => true} end,
                                          T, out_neighbours(G, V))
                          end,
                          #{}, SC)).

-compile({inline, sets_new/0}).
-if(defined(HAVE_sets__new_1) andalso ?OTP_RELEASE < 28).
sets_new() -> sets:new([{version, 2}]).
-else.
sets_new() -> sets:new().
-endif.
-endif.
