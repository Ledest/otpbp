-module(otpbp_erl_syntax).

-ifndef(HAVE_erl_syntax__char_literal_2).
-export([char_literal/2]).
-endif.
-ifndef(HAVE_erl_syntax__string_literal_2).
-export([string_literal/2]).
-endif.
-ifndef(HAVE_erl_syntax__record_access_2).
-export([record_access/2]).
-endif.

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
-define(buggy__revert_implicit_fun, true).

-ifdef(buggy__revert_implicit_fun).
-export([concrete/1, revert/1, revert_forms/1]).
-endif.

-ifndef(HAVE_erl_syntax__char_literal_2).
char_literal(Node, latin1) -> erl_syntax:char_literal(Node).
-endif.

-ifndef(HAVE_erl_syntax__string_literal_2).
string_literal(Node, latin1) -> erl_syntax:string_literal(Node).
-endif.

-ifndef(HAVE_erl_syntax__record_access_2).
record_access(Argument, Field) -> erl_syntax:record_access(Argument, none, Field).
-endif.

-ifdef(buggy__revert_implicit_fun).
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
        binary_field -> revert_binary_field(Node);
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
                        integer -> {'fun', erl_syntax:get_pos(Node), {function, concrete(F), concrete(A)}};
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

%% =====================================================================
%% @doc Returns the Erlang term represented by a syntax tree. Evaluation
%% fails with reason `badarg' if `Node' does not
%% represent a literal term.
%%
%% Note: Currently, the set of syntax trees which have a concrete
%% representation is larger than the set of trees which can be built
%% using the function {@link abstract/1}. An abstract character
%% will be concretised as an integer, while {@link abstract/1} does
%% not at present yield an abstract character for any input. (Use the
%% {@link char/1} function to explicitly create an abstract
%% character.)
%%
%% @see abstract/1
%% @see is_literal/1
%% @see char/1

-spec concrete(syntaxTree()) -> term().
concrete(Node) ->
    case erl_syntax:type(Node) of
        list -> [concrete(erl_syntax:list_head(Node))|concrete(erl_syntax:list_tail(Node))];
        tuple -> list_to_tuple(concrete_list(erl_syntax:tuple_elements(Node)));
        binary ->
            Fs = lists:map(fun(F) ->
                               revert_binary_field(erl_syntax:binary_field(erl_syntax:binary_field_body(F),
                                                                           case erl_syntax:binary_field_size(F) of
                                                                               none -> none;
                                                                               S -> revert(S)
                                                                           end,
                                                                           erl_syntax:binary_field_types(F)))
                      end, erl_syntax:binary_fields(Node)),
            {value, B, _} = eval_bits:expr_grp(Fs, [], fun(F, _) -> {value, concrete(F), []} end, [], true),
            B;
        _ -> erl_syntax:concrete(Node)
    end.

concrete_list([E|Es]) -> [concrete(E)|concrete_list(Es)];
concrete_list([]) -> [].

%% =====================================================================
%% @doc Reverts a sequence of Erlang source code forms. The sequence can
%% be given either as a `form_list' syntax tree (possibly
%% nested), or as a list of "program form" syntax trees. If successful,
%% the corresponding flat list of `erl_parse'-compatible
%% syntax trees is returned (see {@link revert/1}). If some program
%% form could not be reverted, `{error, Form}' is thrown.
%% Standalone comments in the form sequence are discarded.
%%
%% @see revert/1
%% @see form_list/1
%% @see is_form/1

-type forms() :: syntaxTree() | [syntaxTree()].

-spec revert_forms(forms()) -> [erl_parse()].
revert_forms(Forms) when is_list(Forms) -> revert_forms(erl_syntax:form_list(Forms));
revert_forms(T) ->
    case type(T) of
        form_list -> case catch {ok, revert_forms_1(erl_syntax:form_list_elements(erl_syntax:flatten_form_list(T)))} of
                         {ok, Fs} -> Fs;
                         {error, _} = Error -> error(Error);
                         {'EXIT', R} -> exit(R);
                         R -> throw(R)
                     end;
        _ -> error({badarg, T})
    end.

revert_forms_1([T|Ts]) ->
    case erl_stntax:type(T) of
        comment -> revert_forms_1(Ts);
        _ ->
            T1 = revert(T),
            case erl_syntax:is_tree(T1) of
                true -> throw({error, T1});
                false -> [T1|revert_forms_1(Ts)]
            end
    end;
revert_forms_1([]) -> [].

revert_binary_field(Node) ->
    Pos = erl_syntax:get_pos(Node),
    Body = erl_syntax:binary_field_body(Node),
    {Expr, Size} = case erl_syntax:type(Body) of
		       size_qualifier ->
			   %% Note that size qualifiers are not
			   %% revertible out of context.
			   {erl_syntax:size_qualifier_body(Body),
			    erl_syntax:size_qualifier_argument(Body)};
		       _ ->
			   {Body, default}
		   end,
    Types = case erl_syntax:binary_field_types(Node) of
		[] ->
		    default;
		Ts ->
		    fold_binary_field_types(Ts)
	    end,
    {bin_element, Pos, Expr, Size, Types}.

-compile({inline, [revert_implicit_fun/1, revert_root/1, unwrap/1]}).
-endif.
