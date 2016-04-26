-module(otpbp_erl_syntax).

-ifdef(buggy__revert_implicit_fun_1a).
-ifndef(buggy__revert_implicit_fun).
-define(buggy__revert_implicit_fun, true).
-endif.
-endif.
-ifdef(buggy__revert_implicit_fun_1m).
-ifndef(buggy__revert_implicit_fun).
-define(buggy__revert_implicit_fun, true).
-endif.
-endif.

-ifdef(buggy__revert_implicit_fun).
-export([revert/1]).

%% =====================================================================
%% Declarations of globally used internal data structures
%% =====================================================================

%% `com' records are used to hold comment information attached to a
%% syntax tree node or a wrapper structure.
%%
%% #com{pre :: Pre, post :: Post}
%%
%%	Pre = Post = [Com]
%%	Com = syntaxTree()
%%
%%	type(Com) = comment

-record(com, {pre = []::[syntaxTree()], post = []::[syntaxTree()]}).

%% `attr' records store node attributes as an aggregate.
%%
%% #attr{pos :: Pos, ann :: Ann, com :: Comments}
%%
%%	Pos = term()
%%	Ann = [term()]
%%	Comments = none | #com{}
%%
%% where `Pos' `Ann' and `Comments' are the corresponding values of a
%% `tree' or `wrapper' record.

-record(attr, {pos = 0::term(), ann = []::[term()], com = none::'none'|#com{}}).

%% `tree' records represent new-form syntax tree nodes.
%%
%% Tree = #tree{type :: Type, attr :: Attr, data :: Data}
%%
%%	Type = atom()
%%	Attr = #attr{}
%%	Data = term()
%%
%%	is_tree(Tree) = true

-record(tree, {type :: atom(), attr = #attr{}::#attr{}, data :: term()}).

%% `wrapper' records are used for attaching new-form node information to
%% `erl_parse' trees.
%%
%% Wrapper = #wrapper{type :: Type, attr :: Attr, tree :: ParseTree}
%%
%%	Type = atom()
%%	Attr = #attr{}
%%	ParseTree = term()
%%
%%	is_tree(Wrapper) = false

-record(wrapper, {type :: atom(), attr = #attr{} :: #attr{}, tree :: erl_parse()}).

%% =====================================================================

-type syntaxTree() :: erl_syntax:syntaxTree().
-type erl_parse() :: erl_syntax:erl_parse().

%% =====================================================================
%% @doc Returns an `erl_parse'-compatible representation of a
%% syntax tree, if possible. If `Tree' represents a
%% well-formed Erlang program or expression, the conversion should work
%% without problems. Typically, {@link is_tree/1} yields
%% `true' if conversion failed (i.e., the result is still an
%% abstract syntax tree), and `false' otherwise.
%%
%% The {@link is_tree/1} test is not completely foolproof. For a
%% few special node types (e.g. `arity_qualifier'), if such a
%% node occurs in a context where it is not expected, it will be left
%% unchanged as a non-reverted subtree of the result. This can only
%% happen if `Tree' does not actually represent legal Erlang
%% code.
%%
%% @see revert_forms/1
%% @see //stdlib/erl_parse

-spec revert(syntaxTree()) -> syntaxTree().
revert(Node) ->
    case erl_syntax:is_tree(Node) of
        %% Just remove any wrapper. `erl_parse' nodes never contain abstract syntax tree nodes as subtrees.
        false -> unwrap(Node);
        true -> revert_root(case erl_syntax:is_leaf(Node) of
                                true -> Node;
                                %% First revert the subtrees, where possible.
                                %% (Sometimes, subtrees cannot be reverted out of context, and the real work will be done
                                %% when the parent node is reverted.)
                                %% Then reconstruct the node from the reverted parts, and revert the node itself.
                                false -> erl_syntax:update_tree(Node,
                                                                lists:map(fun(L) -> lists:map(fun revert/1, L) end,
                                                                                              erl_syntax:subtrees(Node)))
                            end)
    end.

%% Note: The concept of "compatible root node" is not strictly defined.
%% At a minimum, if `make_tree' is used to compose a node `T' from
%% subtrees that are all completely backwards compatible, then the
%% result of `revert_root(T)' should also be completely backwards
%% compatible.

revert_root(Node) ->
    case erl_syntax:type(Node) of
        implicit_fun -> revert_implicit_fun(Node);
        _ -> erl_syntax:revert(Node)
    end.

%% =====================================================================
%% @doc Removes any wrapper structure, if present. If `Node'
%% is a wrapper structure, this function returns the wrapped
%% `erl_parse' tree; otherwise it returns `Node'
%% itself.

revert_implicit_fun(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case erl_syntax:type(Name) of
        arity_qualifier ->
            F = erl_syntax:arity_qualifier_body(Name),
            case erl_syntax:type(F) of
                atom ->
                    A = erl_syntax:arity_qualifier_argument(Name),
                    case erl_syntax:type(A) of
                        integer -> {'fun', erl_syntax:get_pos(Node), {function, erl_syntax:concrete(F), erl_syntax:concrete(A)}};
                        _ -> Node
                    end;
                _ -> Node
            end;
        module_qualifier ->
            N = erl_syntax:module_qualifier_body(Name),
            case erl_syntax:type(N) of
                arity_qualifier -> {'fun', erl_syntax:get_pos(Node), {function,
                                                                      erl_syntax:module_qualifier_argument(Name),
                                                                      erl_syntax:arity_qualifier_body(N),
                                                                      erl_syntax:arity_qualifier_argument(N)}};
                _ -> Node
            end;
        _ -> Node
    end.

-spec unwrap(syntaxTree()) -> #tree{} | erl_parse().
unwrap(#wrapper{tree = Node}) -> Node;
unwrap(Node) -> Node. % This could also be a new-form node.

-compile({inline, [revert_implicit_fun/1, revert_root/1, unwrap/1]}).
-endif.
