-module(graph_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

opts_test() ->
    Template = [{v1, [v2]}, {v2, [v3]}, {v3, [v4]}, {v4, []}],
    graph:add_edge(build_graph([], Template), v4, v1, []),
    graph:add_edge(build_graph([cyclic], Template), v4, v1, []),
    G6 = build_graph([acyclic], Template),
    ?assertEqual(acyclic, info(G6, cyclicity)),
    ?assertError({bad_edge, {v4, v1}}, graph:add_edge(G6, v4, v1)),
    ok.

degree_test() ->
    G = build_graph([], [{x1, []}, {x2, [x1]}, {x3, [x1, x2]}, {x4, [x1, x2, x3]}, {x5, [x1, x2, x3, x4]}]),
    %% out degree
    ?assertEqual(0, graph:out_degree(G, x1)),
    ?assertEqual(1, graph:out_degree(G, x2)),
    ?assertEqual(2, graph:out_degree(G, x3)),
    ?assertEqual(3, graph:out_degree(G, x4)),
    ?assertEqual(4, graph:out_degree(G, x5)),
    %% out neighbours
    ?assertEqual([], check(graph:out_neighbours(G, x1), [])),
    ?assertEqual([], check(graph:out_neighbours(G, x2), [x1])),
    ?assertEqual([], check(graph:out_neighbours(G, x3), [x1, x2])),
    ?assertEqual([], check(graph:out_neighbours(G, x4), [x1, x2, x3])),
    ?assertEqual([], check(graph:out_neighbours(G, x5), [x1, x2, x3, x4])),

    %% in degree
    ?assertEqual(4, graph:in_degree(G, x1)),
    ?assertEqual(3, graph:in_degree(G, x2)),
    ?assertEqual(2, graph:in_degree(G, x3)),
    ?assertEqual(1, graph:in_degree(G, x4)),
    ?assertEqual(0, graph:in_degree(G, x5)),
    %% in neighbours
    ?assertEqual([], check(graph:in_neighbours(G, x1), [x2, x3, x4, x5])),
    ?assertEqual([], check(graph:in_neighbours(G, x2), [x3, x4, x5])),
    ?assertEqual([], check(graph:in_neighbours(G, x3), [x4, x5])),
    ?assertEqual([], check(graph:in_neighbours(G, x4), [x5])),
    ?assertEqual([], check(graph:in_neighbours(G, x5), [])),
    ok.

path_test() ->
    G = build_graph([], [{x1, [x2, x3]}, {x2, [x4]}, {x3, [x4]}, {x4, [x5, x6]}, {x5, [x7]}, {x6, [x7]}]),
    {G1, Vi} = case graph:get_path(G, x1, x7) of
                   [x1, x2, x4, x5, x7] -> {graph:del_vertex(G, x5), x6};
                   [x1, x2, x4, x6, x7] -> {graph:del_vertex(G, x6), x5};
                   [x1, x3, x4, x5, x7] -> {graph:del_vertex(G, x5), x6};
                   [x1, x3, x4, x6, x7] -> {graph:del_vertex(G, x6), x5}
               end,
    {G2, Vj} = case graph:get_path(G1, x1, x7) of
                   [x1, x2, x4, Vi, x7] -> {graph:del_vertex(G1,x2), x3};
                   [x1, x3, x4, Vi, x7] -> {graph:del_vertex(G1,x3), x2}
               end,
    ?assertEqual([x1, Vj, x4, Vi, x7], graph:get_path(G2, x1, x7)),
    G3 = graph:del_vertex(G2, Vj),
    ?assertNot(graph:get_path(G3, x1, x7)),
    ?assertEqual([], check(graph:vertices(G3), [x1, x4, Vi, x7])),
    ok.

cycle_test() ->
    G = build_graph([],
                    [{x1, [x2, x3]},
                     {x2, [x4]},
                     {x3, [x4]},
                     {x4, [x5, x6]},
                     {x5, [x7]},
                     {x6, [x7, x8]},
                     {x8, [x3, x8]}]),
    ?assertNot(graph:get_cycle(G, x1)),
    ?assertNot(graph:get_cycle(G, x2)),
    ?assertNot(graph:get_cycle(G, x5)),
    ?assertNot(graph:get_cycle(G, x7)),
    ?assertEqual([x3, x4, x6, x8, x3], graph:get_cycle(G, x3)),
    ?assertEqual([x4, x6, x8, x3, x4], graph:get_cycle(G, x4)),
    ?assertEqual([x6, x8, x3, x4, x6], graph:get_cycle(G, x6)),
    ?assertEqual([x8, x3, x4, x6, x8], graph:get_cycle(G, x8)),
    ?assertEqual([x8], graph:get_cycle(graph:del_vertex(G, x4), x8)),
    ok.

vertices_test() ->
    G = build_graph([], [{x, []}, {y, []}]),
    ?assertEqual([], check(graph:vertices(G), [x, y])),
    ?assertEqual([], graph:vertices(graph:del_vertices(G, [x, y]))),
    ok.

edges_test() ->
    G = build_graph([], [{x, [{exy, y}, {exx, x}]}, {y, [{eyx, x}]}]),
    ?assertEqual([], check(labels(graph:edges(G)), [exy, eyx, exx])),
    ?assertEqual([], check(labels(graph:out_edges(G, x)), [exy, exx])),
    ?assertEqual([], check(labels(graph:in_edges(G, x)), [eyx, exx])),
    ?assertEqual([], check(labels(graph:out_edges(G, y)), [eyx])),
    ?assertEqual([], check(labels(graph:in_edges(G, y)), [exy])),
    G1 = del_edges(G, [exy, eyx, does_not_exist]),
    ?assertEqual([exx], labels(graph:edges(G1))),
    ?assertEqual([], check(labels(graph:out_edges(G1, x)), [exx])),
    ?assertEqual([], check(labels(graph:in_edges(G1, x)), [exx])),
    ?assertEqual([], check(labels(graph:out_edges(G1, y)), [])),
    ?assertEqual([], check(labels(graph:in_edges(G1, y)), [])),
    G2 = graph:del_vertices(G1, [x,y]),
    ?assertEqual([], graph:edges(G2)),
    ?assertEqual([], graph:vertices(G2)),
    ok.

del_edges(G, Ls) ->
    lists:foldl(fun({_, _, L} = E, Dg) ->
                    case lists:member(L, Ls) of
                        false -> Dg;
                        true -> graph:del_edge(Dg, E)
                    end
                end,
                G, lists:usort(graph:edges(G))).

data_test() ->
    G = build_graph([], [{x, [{exy, y}]}, {y, []}]),
    ?assertEqual([], graph:vertex(G, x)),
    ?assertEqual([], graph:vertex(G, y)),
    ?assertEqual([{x, y, exy}], graph:edges(G)),
    G4 = graph:add_vertex(graph:add_vertex(graph:add_edge(graph:add_edge(G, x, y, label_1), x, y, label_2), x, {any}),
                          y, '_'), % not a wildcard
    ?assertEqual({any}, graph:vertex(G4, x)),
    ?assertEqual('_', graph:vertex(G4, y)),
    ?assertEqual([{x, y, exy}, {x, y, label_1}, {x, y, label_2}], lists:sort(graph:edges(G4))),
    G5 = graph:del_edge(G4, {x, y, label_1}),
    ?assertEqual([{x, y, exy}, {x, y, label_2}], lists:sort(graph:edges(G5))),
    ?assert(sane(G5)),
    ok.

vertex_names_test() ->
    G = graph:new([acyclic]),
    G3 = graph:add_edge(graph:add_vertex(graph:add_vertex(G, 'A'), '_'), 'A', '_'),
    %% Link 'A' -> '_'
    ?assertEqual(1, graph:out_degree(G3, 'A')),
    ?assertEqual(1, graph:in_degree(G3, '_')),
    ?assertEqual(0, graph:out_degree(G3, '_')),
    ?assertEqual(0, graph:in_degree(G3, 'A')),
    ?assertEqual(['_'], graph:out_neighbours(G3, 'A')),
    ?assertEqual(['A'], graph:in_neighbours(G3, '_')),
    ?assertEqual([], graph:out_neighbours(G3, '_')),
    ?assertEqual([], graph:in_neighbours(G3, 'A')),
    ?assertEqual([{'A', '_', []}], graph:out_edges(G3, 'A')),
    ?assertEqual([{'A', '_', []}], graph:in_edges(G3, '_')),
    ?assertEqual([], graph:out_edges(G3, '_')),
    ?assertEqual([], graph:in_edges(G3, 'A')),
    %% Reverse the edge
    G5 = graph:add_edge(graph:del_edge(G3, {'A', '_', []}), '_', 'A'),
    ?assertEqual(1, graph:out_degree(G5, '_')),
    ?assertEqual(1, graph:in_degree(G5, 'A')),
    ?assertEqual(0, graph:out_degree(G5, 'A')),
    ?assertEqual(0, graph:in_degree(G5, '_')),
    ?assertEqual(['A'], graph:out_neighbours(G5, '_')),
    ?assertEqual(['_'], graph:in_neighbours(G5, 'A')),
    ?assertEqual([], graph:out_neighbours(G5, 'A')),
    ?assertEqual([], graph:in_neighbours(G5, '_')),
    ?assertEqual([{'_', 'A', []}], graph:out_edges(G5, '_')),
    ?assertEqual([{'_', 'A', []}], graph:in_edges(G5, 'A')),
    ?assertEqual([], graph:out_edges(G5, 'A')),
    ?assertEqual([], graph:in_edges(G5, '_')),
    ok.

otp_3522_test() ->
    G0 = build_graph([acyclic], [{x, []}]),
    ?assertError({bad_edge, {x, x}}, graph:add_edge(G0, x, x)),
    G = graph:new(),
    ?assertEqual(0, graph:no_vertices(G)),
    ?assertEqual(0, graph:no_edges(G)),
    {V1, G1} = graph:add_vertex(G),
    {V2, G3} = graph:add_vertex(graph:add_vertex(G1, '$vid')),
    G5 = graph:add_edge(graph:add_edge(G3, V1, V2, l1), V1, V2),
    ?assertEqual(3, graph:no_vertices(G5)),
    ?assertEqual(2, graph:no_edges(G5)),
    ?assertEqual(cyclic, info(G5, cyclicity)),
    ?assertEqual([], check(labels(graph:in_edges(G5, V2)), [l1, []])),
    ?assertEqual([], check(labels(graph:out_edges(G5, V1)), [l1, []])),
    ?assertEqual([], check(graph:vertices(G5), [V1,V2,'$vid'])),
    ?assertEqual([], check(labels(graph:edges(G5)), [[], l1])),
    ?assert(sane(G5)),
    ok.

otp_3630_test() ->
    G = build_graph([], [{x, [{exy, y}, {exx, x}]}, {y, [{eyy, y},{eyx, x}]}]),
    ?assertEqual([x, y], graph:get_path(G, x, y)),
    ?assertEqual([y, x], graph:get_path(G, y, x)),
    ?assertEqual([x, x], graph:get_short_path(G, x, x)),
    ?assertEqual([y, y], graph:get_short_path(G, y, y)),
    G1 = build_graph([],
                     [{1, [{12, 2}, {13, 3}, {11, 1}]},
                      {2, [{23, 3}]},
                      {3, [{34, 4}, {35, 5}]},
                      {4, [{45, 5}]},
                      {5, [{56, 6}, {57, 7}]},
                      {6, [{67, 7}]},
                      {7, [{71, 1}]}]),
    ?assertEqual([1, 3, 5, 7], graph:get_short_path(G1, 1, 7)),
    ?assertEqual([3, 5, 7, 1, 3], graph:get_short_cycle(G1, 3)),
    ?assertEqual([1, 1], graph:get_short_cycle(G1, 1)),
    ?assert(sane(graph:add_edge(graph:add_vertex(graph:add_vertex(graph:new([acyclic]), 0.0), 0), 0.0, 0))),
    ok.

otp_8066_test() ->
    {V1, D1} = graph:add_vertex(graph:new()),
    {V2, D2} = graph:add_vertex(D1),
    D3 = graph:add_edge(D2, V1, V2),
    [V1, V2] = graph:get_path(D3, V1, V2),
    ?assert(sane(D3)),
    D4 = graph:del_path(D3, V1, V2),
    ?assert(sane(D4)),
    ?assertNot(graph:get_path(D4, V1, V2)),
    graph:del_path(D4, V1, V2),
    D6 = graph:add_edge(graph:add_edge(graph:add_edge(D3, V1, V2), V1, V1), V2, V2),
    [V1, V2] = graph:get_path(D6, V1, V2),
    ?assert(sane(D6)),
    D7 = graph:del_path(D6, V1, V2),
    ?assertNot(graph:get_short_path(D7, V2, V1)),
    ?assert(sane(D7)),
    ?assertNot(graph:get_path(D7, V1, V2)),
    graph:del_path(D7, V1, V2),
    {W1, G1} = graph:add_vertex(graph:new()),
    {W2, G2} = graph:add_vertex(G1),
    {_W3, G3} = graph:add_vertex(G2),
    {_W4, G4} = graph:add_vertex(G3),
    G5 = graph:add_edge(G4, W1, W2, ['$e'|0]),
    ?assertError({bad_vertex, bv}, graph:add_edge(G5, bv, W1)),
    ?assertError({bad_vertex, bv}, graph:add_edge(G5, W1, bv)),
    ?assertNot(graph:get_short_cycle(G5, W1)),
    ?assert(sane(G5)),
    ok.

%% Graph utilities tests

simple_test() ->
    G = add_edges(add_vertices(graph:new(), [a]), [{b, c}, {b, d}, {e, f}, {f, g}, {g, e}, {h, h}, {i, i}, {i, ij1, j}, {i, ij2, j}]),
    ?assertEqual(10, length(graph:postorder(G))),
    ?assertEqual(10, length(graph:preorder(G))),
    ok = ?assertEqual(sort_ll(graph:components(G)), sort_ll([[a], [b, c, d], [e, f, g], [h], [i, j]])),
    ok = ?assertEqual(sort_ll(graph:strong_components(G)), sort_ll([[a], [b], [c], [d], [e, f, g], [h], [i], [j]])),
    ok = ?assertEqual(sort_ll(graph:cyclic_strong_components(G)), sort_ll([[e, f, g], [h], [i]])),
    ?assert(path(G, e, e)),
    ?assertNot(path(G, e, j)),
    ?assertNot(path(G, a, a)),
    ?assertEqual(lists:sort(graph:topsort(G)), lists:sort([a, b, c, d, e, f, g, h, i, j])),
    ?assertNot(graph:is_acyclic(G)),
    ?assert(graph:has_vertex(G, h)),
    ?assertNot(graph:has_vertex(G, z)),
    ?assert(graph:has_edge(G, {i, j, ij1})),
    ?assertNot(graph:has_edge(G, {j, i, ij1})),
    ?assert(graph:has_edge(G, {i, j, ij2})),
    ?assertNot(graph:has_edge(G, {j, i, ij2})),
    ?assert(graph:has_edge(G, i, j)),
    ?assertNot(graph:has_edge(G, j, i)),
    ?assertEqual([{i, j, ij1}, {i, j, ij2}], graph:edges(G, i, j)),
    ?assertEqual([], graph:edges(G, j, i)),
    ?assertEqual(lists:sort(graph:loop_vertices(G)), lists:sort([h, i])),
    ?assertEqual(lists:sort(graph:reaching(G, [e])), lists:sort([e, f, g])),
    ?assertEqual(lists:sort(graph:reaching_via_neighbours(G, [e])), lists:sort([e, f, g])),
    ?assertEqual(lists:sort(graph:reachable(G, [e])), lists:sort([e, f, g])),
    ?assertEqual(lists:sort(graph:reachable_via_neighbours(G, [e])), lists:sort([e, f, g])),
    ?assertEqual(graph:reaching(G, [b]), [b]),
    ?assertEqual(graph:reaching_via_neighbours(G, [b]), []),
    ?assertEqual(lists:sort(graph:reachable(G, [b])), lists:sort([b, c, d])),
    ?assertEqual(lists:sort(graph:reachable_via_neighbours(G, [b])), lists:sort([c, d])),
    ?assertEqual(graph:reaching(G, [h]), [h]),
    ?assertEqual(graph:reaching_via_neighbours(G, [h]), [h]),
    ?assertEqual(graph:reachable(G, [h]), [h]),
    ?assertEqual(graph:reachable_via_neighbours(G, [h]), [h]),
    ?assertEqual(lists:sort(graph:reachable(G, [e, f])), lists:sort([e, f, g])),
    ?assertEqual(lists:sort(graph:reachable_via_neighbours(G, [e, f])), lists:sort([e, f, g])),
    ?assertEqual(graph:reachable(G, [h, h, h]), [h]),
    ok.

roots_test() ->
    G = add_edges(add_vertices(graph:new(), [a]), [{a, b}, {b, c}, {c, a}, {c, d}, {j, j}, {j, k}, {j, l}]),
    ?assertEqual(7, length(graph:postorder(G))),
    ?assertEqual(7, length(graph:preorder(G))),
    ok.

loop_test() ->
    G = add_edges(add_vertices(graph:new(), [a, b]), [{a, a}, {b, b}]),
    ?assertEqual(sort_ll(graph:components(G)), sort_ll([[a], [b]])),
    ?assertEqual(sort_ll(graph:strong_components(G)), sort_ll([[a], [b]])),
    ?assertEqual(sort_ll(graph:cyclic_strong_components(G)), sort_ll([[a], [b]])),
    ?assertEqual(2, length(graph:topsort(G))),
    ?assertNot(graph:is_acyclic(G)),
    ?assertEqual(lists:sort(graph:loop_vertices(G)), lists:sort([a, b])),
    ?assertEqual(2, length(graph:preorder(G))),
    ?assertEqual(2, length(graph:postorder(G))),
    ?assertEqual(graph:reaching(G, [b]), [b]),
    ?assertEqual(graph:reaching_via_neighbours(G, [b]), [b]),
    ?assertEqual(graph:reachable(G, [b]), [b]),
    ?assertEqual(graph:reachable_via_neighbours(G, [b]), [b]),
    ?assert(path(G, a, a)),
    ok.

isolated_test() ->
    G = add_vertices(graph:new(), [a, b]),
    ?assertEqual(sort_ll(graph:components(G)), sort_ll([[a], [b]])),
    ?assertEqual(sort_ll(graph:strong_components(G)), sort_ll([[a], [b]])),
    ?assertEqual(graph:cyclic_strong_components(G), []),
    ?assertEqual(2, length(graph:topsort(G))),
    ?assert(graph:is_acyclic(G)),
    ?assertEqual(graph:loop_vertices(G), []),
    ?assertEqual(2, length(graph:preorder(G))),
    ?assertEqual(2, length(graph:postorder(G))),
    ?assertEqual(graph:reaching(G, [b]), [b]),
    ?assertEqual(graph:reaching_via_neighbours(G, [b]), []),
    ?assertEqual(graph:reachable(G, [b]), [b]),
    ?assertEqual(graph:reachable_via_neighbours(G, [b]), []),
    ?assertNot(path(G, a, a)),
    ok.

topsort_test() ->
    ?assertEqual(lists:sort(graph:topsort(add_edges(graph:new(), [{a, b}, {b, c}, {c, d}, {d, e}, {e, f}]))),
                 lists:sort([a, b, c, d, e, f])),
    ok.

subgraph_test() ->
    G = add_vertices(add_edges(graph:new([acyclic]), [{b, c}, {b, d}, {e, f}, {f, fg, g}, {f, fg2, g}, {h, i}, {i, j}]),
                     [{b, bl}, {f, fl}]),
    SG = graph:subgraph(G, [u1, b, c, u2, f, g, i, u3]),
    ?assertEqual([b, c, f, g, i], lists:sort(graph:vertices(SG))),
    ?assertEqual(bl, graph:vertex(SG, b)),
    ?assertEqual([], graph:vertex(SG, c)),
    ?assertEqual([{f, g, fg}, {f, g, fg2}], graph:edges(SG, f, g)),
    ?assertMatch({_, {_, acyclic}}, lists:keysearch(cyclicity, 1, graph:info(SG))),
    SG1 = graph:subgraph(G, [f, g, h], [{type, []}, {keep_labels, false}]),
    ?assertEqual([f, g, h], lists:sort(graph:vertices(SG1))),
    ?assertEqual([], graph:vertex(SG1, f)),
    ?assertEqual([{f, g, []}], graph:edges(SG1, f, g)),
    ?assertMatch({_, {_, cyclic}}, lists:keysearch(cyclicity, 1, graph:info(SG1))),
    SG2 = graph:subgraph(G, [f, g, h], [{type, [acyclic]}, {keep_labels, true}]),
    ?assertEqual([f, g, h], lists:sort(graph:vertices(SG2))),
    ?assertEqual(fl, graph:vertex(SG2, f)),
    ?assertEqual([{f, g, fg}, {f, g, fg2}], graph:edges(SG2, f, g)),
    ?assertMatch({_, {_, acyclic}}, lists:keysearch(cyclicity, 1, graph:info(SG2))),
    ?assertError(badarg, graph:subgraph(G, [f], [{invalid, opt}])),
    ?assertError(badarg, graph:subgraph(G, [f], [{keep_labels, not_Bool}])),
    ?assertError(badarg, graph:subgraph(G, [f], [{type, not_type}])),
    ?assertError(badarg, graph:subgraph(G, [f], [{type, [not_type]}])),
    ?assertError(badarg, graph:subgraph(G, [f], not_a_list)),
    ok.

condensation_test() ->
    CG = graph:condensation(add_vertices(add_edges(graph:new([]),
                                                   [{b, c}, {b, d}, {e, f}, {f, fgl, g},
                                                    {f, fgl2, g}, {g, e}, {h, h}, {j, i}, {i, j}]),
                                         [q])),
    ?assertEqual([[b], [c], [d], [e, f, g], [h], [i, j], [q]],
                 lists:sort(lists:map(fun lists:sort/1, graph:vertices(CG)))),
    ?assertEqual([{[b], [c]}, {[b], [d]}],
                 lists:sort(lists:map(fun({V1, V2, _L}) -> {lists:sort(V1), lists:sort(V2)} end, graph:edges(CG)))),
    ok.

tree_test() ->
    ?assertNot(is_tree([], [])),
    ?assert(is_tree([a], [])),
    ?assertNot(is_tree([a, b], [])),
    ?assert(is_tree([{a, b}])),
    ?assertNot(is_tree([{a, b}, {b, a}])),
    ?assert(is_tree([{a, b}, {a, c}, {b, d}, {b, e}])),
    ?assertNot(is_tree([{a, b}, {a, c}, {b, d}, {b, e}, {d, e}])),
    ?assertNot(is_tree([{a, b}, {a, c}, {b, d}, {b, l1, e}, {b, l2, e}])),
    ?assert(is_tree([{a, c}, {c, b}])),
    ?assert(is_tree([{b, a}, {c, a}])),
    %% Parallel edges. Acyclic and with one component
    ?assertNot(is_tree([{a, l1, b}, {a, l2, b}])),
    ?assertEqual(no, arborescence_root([], [])),
    ?assertEqual({yes, a}, arborescence_root([a], [])),
    ?assertEqual(no, arborescence_root([a, b], [])),
    ?assertEqual({yes, a}, arborescence_root([{a, b}])),
    ?assertEqual(no, arborescence_root([{a, b}, {b, a}])),
    ?assertEqual({yes, a}, arborescence_root([{a, b}, {a, c}, {b, d}, {b, e}])),
    ?assertEqual(no, arborescence_root([{a, b}, {a, c}, {b, d}, {b, e}, {d, e}])),
    ?assertEqual(no, arborescence_root([{a, b}, {a, c}, {b, d}, {b, l1, e}, {b, l2, e}])),
    ?assertEqual({yes, a}, arborescence_root([{a, c}, {c, b}])),
    ?assertEqual(no, arborescence_root([{b, a}, {c, a}])),
    ?assertNot(is_arborescence([], [])),
    ?assert(is_arborescence([a], [])),
    ?assertNot(is_arborescence([a, b], [])),
    ?assert(is_arborescence([{a, b}])),
    ?assertNot(is_arborescence([{a, b}, {b, a}])),
    ?assert(is_arborescence([{a, b}, {a, c}, {b, d}, {b, e}])),
    ?assertNot(is_arborescence([{a, b}, {a, c}, {b, d}, {b, e}, {d, e}])),
    ?assertNot(is_arborescence([{a, b}, {a, c}, {b, d}, {b, l1, e}, {b, l2, e}])),
    ?assert(is_arborescence([{a, c}, {c, b}])),
    ?assertNot(is_arborescence([{b, a}, {c, a}])),
    %% Parallel edges.
    ?assertNot(is_arborescence([{a, l1, b}, {a, l2, b}])),
    ok.

traversals_test() ->
    G = graph:new([]),
    ?assertEqual([], graph:preorder(G)),
    ?assertEqual([], graph:postorder(G)),
    G1 = add_edges(G, [{a, b}, {b, c}, {c, d}, {d, e}]),
    ?assertEqual([a, b, c, d, e], graph:preorder(G1)),
    ?assertEqual([e, d, c, b, a], graph:postorder(G1)),
    G2 = add_edges(G1, [{0, 1}, {1, 2}, {2, 0}]),
    ?assertEqual([a, b, c, d, e, 1, 2, 0], graph:preorder(G2)),
    ?assertEqual([e, d, c, b, a, 0, 2, 1], graph:postorder(G2)),
    list_to_integer(erlang:system_info(otp_release)) >= 28 andalso
    begin
    G3 = add_edges(G1, [{x, 0}, {y, 1}, {z, 2}]),
    ?assertEqual([x, 0, y, 1, a, b, c, d, e, z, 2], graph:preorder(G3)),
    ?assertEqual([0, x, 1, y, e, d, c, b, a, 2, z], graph:postorder(G3))
    end,
    ok.

info(G, What) ->
    case lists:keyfind(What, 1, graph:info(G)) of
        {What, Value} -> Value;
        false -> []
    end.

labels(List) -> lists:map(fun label/1, List).

label({_V1, _V2, L}) -> L;
label({_V, L}) -> L.

check(R0, E0) ->
    case {lists:sort(R0), lists:sort(E0)} of
        {R, R} -> [];
        {R, E} -> (R -- E) ++ (E -- R)
    end.

build_graph(Opts, Gs) -> build_g(graph:new(Opts), Gs).

build_g(G, [{V, Ns}|Gs]) -> build_g(build_ns(graph:add_vertex(G, V), V, Ns), Gs);
build_g(G, []) ->
    true = sane(G),
    G.

build_ns(G, V, [{L, W}|Ns]) -> build_ns(graph:add_edge(graph:add_vertex(G, W), V, W, L), V, Ns);
build_ns(G, V, [W|Ns]) -> build_ns(graph:add_edge(graph:add_vertex(G, W), V, W), V, Ns);
build_ns(G, _V, []) -> G.

sane(G) ->
    sane1(G),
    erase(sane) =:= undefined.

sane1(G) ->
    Es = graph:edges(G),
    Vs = graph:vertices(G),
    VEs = lists:flatmap(fun(V) -> graph:edges(G, V) end, Vs),
    case {lists:sort(Es ++ Es), lists:sort(VEs)} of
        {R1, R1} -> ok;
        _ ->
            io:format("Bad edges~n", []),
            put(sane, no)
    end,
    lists:foreach(fun({V1, V2, _L} = Edge) ->
                      case {graph:vertex(G, V1, none), graph:vertex(G, V2, none)} of
                          {L1, L2} when L1 =/= none, L2 =/= none -> ok;
                          _ ->
                              io:format("Missing vertex ~p~n", [Edge]),
                              put(sane, no)
                      end,
                      In = graph:in_edges(G, V2),
                      case lists:member(Edge, In) of
                          true -> ok;
                          false ->
                              io:format("Missing in-neighbour ~p~n", [Edge]),
                              put(sane, no)
                      end,
                      Out = graph:out_edges(G, V1),
                      case lists:member(Edge, Out) of
                          true -> ok;
                          false ->
                              io:format("Missing out-neighbour ~p~n", [Edge]),
                              put(sane, no)
                      end
                  end,
                  Es),
    lists:foreach(fun(V) ->
                      InEs = graph:in_edges(G, V),
                      %% *All* in-edges of V
                      lists:foreach(fun({_, V1, _}) when V1 =:= V -> ok;
                                       (E) ->
                                        io:format("Bad in-edge ~p: ~p~n", [V, E]),
                                        put(sane, no)
                                    end,
                                    InEs),
                      OutEs = graph:out_edges(G, V),
                      lists:foreach(fun({V1, _, _}) when V1 =:= V -> ok;
                                       (E) ->
                                        io:format("Bad out-edge ~p: ~p~n", [V, E]),
                                        put(sane, no)
                                    end,
                                    OutEs)
                  end,
                  Vs),
    InEs = lists:flatmap(fun(V) -> graph:in_edges(G, V) end, Vs),
    OutEs = lists:flatmap(fun(V) -> graph:out_edges(G, V) end, Vs),
    lists:foreach(fun({_, _, _}) -> ok;
                     (E) ->
                      io:format("Unknown edge (neighbour) ~p~n", [E]),
                      put(sane, no)
                  end,
                  InEs ++ OutEs),
    case {length(InEs), length(OutEs), graph:no_edges(G)} of
        {N, N, N} -> ok;
        {N, N, N_edges} ->
            io:format("Invalid number of edges (~p+~p =/= 2*~p)~n", [N, N, N_edges]),
            put(sane, no);
        {_, _, _} ->
            io:format("Number of in- and out-edges differs~n", []),
            put(sane, no)
    end,
    EVs = lists:usort([V || {V, _, _} <- Es] ++ [V || {_, V, _} <- Es]),
    lists:foreach(fun(V) ->
                      case graph:vertex(G, V, none) of
                          none ->
                              io:format("Unknown vertex in edge: ~p~n", [V]),
                              put(sane, no);
                          _ -> ok
                      end
                  end,
                  EVs),
    %% sink: a vertex with no outgoing edges
    SinkVs = [V || V <- Vs, graph:out_edges(G, V) =:= []],
    case lists:sort(SinkVs) =:= lists:sort(graph:sink_vertices(G)) of
        true -> ok;
        false -> io:format("Bad sinks~n"), put(sane, no)
    end,
    %% source: a vertex with no lists:sort(graph:source_vertices(G)incoming edges
    SourceVs = [V || V <- Vs, graph:in_edges(G, V) =:= [] ],
    case {lists:sort(SourceVs), lists:sort(graph:source_vertices(G))} of
        {R2, R2} -> ok;
        _ ->
            io:format("Bad sources~n"),
            put(sane, no)
    end,
    true.

path(G, V1, V2) -> graph:get_path(G, V1, V2) =/= false.

add_vertices(G, Vs) ->
    lists:foldl(fun({V, Label}, G0) -> graph:add_vertex(G0, V, Label);
                   (V, G0) -> graph:add_vertex(G0, V)
                end,
                G, Vs).

add_edges(G, L) ->
    lists:foldl(fun({From, To}, G0) -> graph:add_edge(graph:add_vertex(graph:add_vertex(G0, From), To), From, To);
                   ({From, Label, To}, G0) ->
                    graph:add_edge(graph:add_vertex(graph:add_vertex(G0, From), To), From, To, Label)
                end,
                G, L).

sort_ll(LL) -> lists:sort(lists:map(fun lists:sort/1, LL)).

is_tree(Es) -> graph:is_tree(add_edges(add_vertices(graph:new(), []), Es)).

is_tree(Vs, Es) -> graph:is_tree(add_edges(add_vertices(graph:new(), Vs), Es)).

is_arborescence(Es) -> graph:is_arborescence(add_edges(add_vertices(graph:new(), []), Es)).

is_arborescence(Vs, Es) -> graph:is_arborescence(add_edges(add_vertices(graph:new(), Vs), Es)).

arborescence_root(Es) -> graph:arborescence_root(add_edges(add_vertices(graph:new(), []), Es)).

arborescence_root(Vs, Es) -> graph:arborescence_root(add_edges(add_vertices(graph:new(), Vs), Es)).
