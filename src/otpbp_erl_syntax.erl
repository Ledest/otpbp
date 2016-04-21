-module(otpbp_erl_syntax).

-export([revert/1]).

-import(erl_syntax, [application_arguments/1, application_operator/1,
                     arity_qualifier_argument/1, arity_qualifier_body/1,
                     atom_value/1,
                     attribute_arguments/1, attribute_name/1,
                     binary_comp_body/1, binary_comp_template/1,
                     block_expr_body/1,
                     binary_fields/1, binary_field_body/1, binary_field_types/1,
                     binary_generator_body/1, binary_generator_pattern/1,
                     case_expr_argument/1, case_expr_clauses/1,
                     catch_expr_body/1,
                     char_value/1,
                     class_qualifier_argument/1, class_qualifier_body/1,
                     clause_body/1, clause_guard/1, clause_patterns/1,
                     concrete/1,
                     cond_expr_clauses/1,
                     conjunction_body/1,
                     disjunction_body/1,
                     error_marker_info/1,
                     float_value/1, fun_expr_clauses/1, function_arity/1, function_clauses/1, function_name/1,
                     generator_body/1, generator_pattern/1, get_pos/1,
                     if_expr_clauses/1,
                     implicit_fun_name/1,
                     infix_expr_left/1, infix_expr_operator/1, infix_expr_right/1,
                     integer_value/1,
                     is_leaf/1, is_list_skeleton/1, is_proper_list/1, is_tree/1,
                     list_comp_body/1, list_comp_template/1, list_elements/1, list_prefix/1, list_suffix/1,
                     match_expr_body/1, match_expr_pattern/1, module_qualifier_argument/1, module_qualifier_body/1,
                     nil/0,
                     operator_name/1,
                     parentheses_body/1,
                     prefix_expr_argument/1, prefix_expr_operator/1,
                     receive_expr_action/1, receive_expr_clauses/1, receive_expr_timeout/1,
                     record_access_argument/1, record_access_field/1, record_access_type/1,
                     record_expr_argument/1, record_expr_fields/1, record_expr_type/1,
                     record_field_name/1, record_field_value/1,
                     record_index_expr_field/1, record_index_expr_type/1,
                     rule_arity/1, rule_clauses/1, rule_name/1,
                     set_pos/2, size_qualifier_argument/1, size_qualifier_body/1, string_value/1, subtrees/1,
                     tree/2,
                     try_expr_after/1, try_expr_body/1, try_expr_clauses/1, try_expr_handlers/1,
                     tuple_elements/1, type/1,
                     update_tree/2,
                     variable_name/1,
                     warning_marker_info/1]).

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

-record(com, {pre  = [] :: [syntaxTree()],
	      post = [] :: [syntaxTree()]}).

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

-record(attr, {pos = 0    :: term(),
	       ann = []   :: [term()],
	       com = none :: 'none' | #com{}}).

%% `tree' records represent new-form syntax tree nodes.
%%
%% Tree = #tree{type :: Type, attr :: Attr, data :: Data}
%%
%%	Type = atom()
%%	Attr = #attr{}
%%	Data = term()
%%
%%	is_tree(Tree) = true

-record(tree, {type           :: atom(),
	       attr = #attr{} :: #attr{},
	       data           :: term()}).

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

-record(wrapper, {type           :: atom(),
		  attr = #attr{} :: #attr{},
		  tree           :: erl_parse()}).

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
    case is_tree(Node) of
	false ->
	    %% Just remove any wrapper. `erl_parse' nodes never contain
	    %% abstract syntax tree nodes as subtrees.
	    unwrap(Node);
	true ->
	    case is_leaf(Node) of
		true ->
		    revert_root(Node);
		false ->
		    %% First revert the subtrees, where possible.
		    %% (Sometimes, subtrees cannot be reverted out of
		    %% context, and the real work will be done when the
		    %% parent node is reverted.)
		    Gs = [[revert(X) || X <- L] || L <- subtrees(Node)],

		    %% Then reconstruct the node from the reverted
		    %% parts, and revert the node itself.
		    Node1 = update_tree(Node, Gs),
		    revert_root(Node1)
	    end
    end.

%% Note: The concept of "compatible root node" is not strictly defined.
%% At a minimum, if `make_tree' is used to compose a node `T' from
%% subtrees that are all completely backwards compatible, then the
%% result of `revert_root(T)' should also be completely backwards
%% compatible.

revert_root(Node) ->
    case type(Node) of
	application ->
	    revert_application(Node);
	atom ->
	    revert_atom(Node);
	attribute ->
	    revert_attribute(Node);
	binary ->
	    revert_binary(Node);
        binary_comp ->
	    revert_binary_comp(Node);
	binary_field ->
	    revert_binary_field(Node);
        binary_generator ->
	    revert_binary_generator(Node);
	block_expr ->
	    revert_block_expr(Node);
	case_expr ->
	    revert_case_expr(Node);
	catch_expr ->
	    revert_catch_expr(Node);
	char ->
	    revert_char(Node);
	clause ->
	    revert_clause(Node);
	cond_expr ->
	    revert_cond_expr(Node);
	eof_marker ->
	    revert_eof_marker(Node);
	error_marker ->
	    revert_error_marker(Node);
	float ->
	    revert_float(Node);
	fun_expr ->
	    revert_fun_expr(Node);
	function ->
	    revert_function(Node);
	generator ->
	    revert_generator(Node);
	if_expr ->
	    revert_if_expr(Node);
	implicit_fun ->
	    revert_implicit_fun(Node);
	infix_expr ->
	    revert_infix_expr(Node);
	integer ->
	    revert_integer(Node);
	list ->
	    revert_list(Node);
	list_comp ->
	    revert_list_comp(Node);
	match_expr ->
	    revert_match_expr(Node);
	module_qualifier ->
	    revert_module_qualifier(Node);
	nil ->
	    revert_nil(Node);
	parentheses ->
	    revert_parentheses(Node);
	prefix_expr ->
	    revert_prefix_expr(Node);
	receive_expr ->
	    revert_receive_expr(Node);
	record_access ->
	    revert_record_access(Node);
	record_expr ->
	    revert_record_expr(Node);
	record_index_expr ->
	    revert_record_index_expr(Node);
	rule ->
	    revert_rule(Node);
	string ->
	    revert_string(Node);
	try_expr ->
	    revert_try_expr(Node);
	tuple ->
	    revert_tuple(Node);
	underscore ->
	    revert_underscore(Node);
	variable ->
	    revert_variable(Node);
	warning_marker ->
	    revert_warning_marker(Node);
	_ ->
	    %% Non-revertible new-form node
	    Node
    end.

%% =====================================================================
%% @doc Removes any wrapper structure, if present. If `Node'
%% is a wrapper structure, this function returns the wrapped
%% `erl_parse' tree; otherwise it returns `Node'
%% itself.

-spec unwrap(syntaxTree()) -> #tree{} | erl_parse().

unwrap(#wrapper{tree = Node}) -> Node;
unwrap(Node) -> Node.	 % This could also be a new-form node.

revert_application(Node) ->
    Pos = get_pos(Node),
    Operator = application_operator(Node),
    Arguments = application_arguments(Node),
    {call, Pos, Operator, Arguments}.

revert_atom(Node) ->
    Pos = get_pos(Node),
    {atom, Pos, atom_value(Node)}.

revert_attribute(Node) ->
    Name = attribute_name(Node),
    Args = attribute_arguments(Node),
    Pos = get_pos(Node),
    case type(Name) of
	atom ->
	    revert_attribute_1(atom_value(Name), Args, Pos, Node);
	_ ->
	    Node
    end.

%% All the checking makes this part a bit messy:

revert_attribute_1(module, [M], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} -> 
	    {attribute, Pos, module, A};
	error -> Node
    end;
revert_attribute_1(module, [M, List], Pos, Node) ->
    Vs = case is_list_skeleton(List) of
	     true ->
		 case is_proper_list(List) of
		     true ->
			 fold_variable_names(list_elements(List));
		     false ->
			 Node
		 end;
	     false ->
		 Node
	 end,
    case revert_module_name(M) of
	{ok, A} -> 
	    {attribute, Pos, module, {A, Vs}};
	error -> Node
    end;
revert_attribute_1(export, [List], Pos, Node) ->
    case is_list_skeleton(List) of
	true ->
	    case is_proper_list(List) of
		true ->
		    Fs = fold_function_names(list_elements(List)),
		    {attribute, Pos, export, Fs};
		false ->
		    Node
	    end;
	false ->
	    Node
    end;
revert_attribute_1(import, [M], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} -> {attribute, Pos, import, A};
	error -> Node
    end;
revert_attribute_1(import, [M, List], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} ->
	    case is_list_skeleton(List) of
		true ->
		    case is_proper_list(List) of
			true ->
			    Fs = fold_function_names(
				   list_elements(List)),
			    {attribute, Pos, import, {A, Fs}};
			false ->
			    Node
		    end;
		false ->
		    Node
	    end;
	error ->
	    Node
    end;
revert_attribute_1(file, [A, Line], Pos, Node) ->
    case type(A) of
	string ->
	    case type(Line) of
		integer ->
		    {attribute, Pos, file,
		     {concrete(A), concrete(Line)}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end;
revert_attribute_1(record, [A, Tuple], Pos, Node) ->
    case type(A) of
	atom ->
	    case type(Tuple) of
		tuple ->
		    Fs = fold_record_fields(
			   tuple_elements(Tuple)),
		    {attribute, Pos, record, {concrete(A), Fs}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end;
revert_attribute_1(N, [T], Pos, _) ->
    {attribute, Pos, N, concrete(T)};
revert_attribute_1(_, _, _, Node) ->
    Node.

revert_binary(Node) ->
    Pos = get_pos(Node),
    {bin, Pos, binary_fields(Node)}.

revert_binary_comp(Node) ->
    Pos = get_pos(Node),
    Template = binary_comp_template(Node),
    Body = binary_comp_body(Node),
    {bc, Pos, Template, Body}.

revert_binary_field(Node) ->
    Pos = get_pos(Node),
    Body = binary_field_body(Node),
    {Expr, Size} = case type(Body) of
		       size_qualifier ->
			   %% Note that size qualifiers are not
			   %% revertible out of context.
			   {size_qualifier_body(Body),
			    size_qualifier_argument(Body)};
		       _ ->
			   {Body, default}
		   end,
    Types = case binary_field_types(Node) of
		[] ->
		    default;
		Ts ->
		    fold_binary_field_types(Ts)
	    end,
    {bin_element, Pos, Expr, Size, Types}.

revert_binary_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = binary_generator_pattern(Node),
    Body = binary_generator_body(Node),
    {b_generate, Pos, Pattern, Body}.

revert_block_expr(Node) ->
    Pos = get_pos(Node),
    Body = block_expr_body(Node),
    {block, Pos, Body}.

revert_case_expr(Node) ->
    Pos = get_pos(Node),
    Argument = case_expr_argument(Node),
    Clauses = [revert_clause(C) || C <- case_expr_clauses(Node)],
    {'case', Pos, Argument, Clauses}.

revert_catch_expr(Node) ->
    Pos = get_pos(Node),
    Expr = catch_expr_body(Node),
    {'catch', Pos, Expr}.

revert_char(Node) ->
    Pos = get_pos(Node),
    {char, Pos, char_value(Node)}.

revert_cond_expr(Node) -> {'cond', get_pos(Node), list:map(fun revert_clause/1, cond_expr_clauses(Node))}.

revert_clause(Node) ->
    Pos = get_pos(Node),
    Guard = case clause_guard(Node) of
		none ->
		    [];
		E ->
		    case type(E) of
			disjunction ->
			    revert_clause_disjunction(E);
			conjunction ->
			    %% Only the top level expression is
			    %% unfolded here; no recursion.
			    [conjunction_body(E)];
			_ ->
			    [[E]]	% a single expression
		    end
	    end,
    {clause, Pos, clause_patterns(Node), Guard,
     clause_body(Node)}.

revert_clause_disjunction(D) ->
    %% We handle conjunctions within a disjunction, but only at the top level; no recursion.
    lists:map(fun(E) ->
                  case type(E) of
                      conjunction -> conjunction_body(E);
                      _ -> [E]
                  end
              end, disjunction_body(D)).

revert_eof_marker(Node) -> {eof, get_pos(Node)}.

%% Note that the position information of the node itself is not preserved.
revert_error_marker(Node) -> {error, error_marker_info(Node)}.

revert_float(Node) -> {float, get_pos(Node), float_value(Node)}.

revert_fun_expr(Node) -> {'fun', get_pos(Node), {clauses, lists:map(fun revert_clause/1, fun_expr_clauses(Node))}}.

revert_function(Node) ->
    Name = function_name(Node),
    Clauses = [revert_clause(C) || C <- function_clauses(Node)],
    Pos = get_pos(Node),
    case type(Name) of
	atom ->
	    A = function_arity(Node),
	    {function, Pos, concrete(Name), A, Clauses};
	_ ->
	    Node
    end.

revert_generator(Node) -> {generate, get_pos(Node), generator_pattern(Node), generator_body(Node)}.

revert_if_expr(Node) -> {'if', get_pos(Node), lists:map(fun revert_clause/1, if_expr_clauses(Node))}.

revert_implicit_fun(Node) ->
    Pos = get_pos(Node),
    Name = implicit_fun_name(Node),
    case type(Name) of
	arity_qualifier ->
	    F = arity_qualifier_body(Name),
	    A = arity_qualifier_argument(Name),
	    case {type(F), type(A)} of
		{atom, integer} ->
		    {'fun', Pos,
		     {function, concrete(F), concrete(A)}};
		_ ->
		    Node
	    end;
	module_qualifier ->
	    M = module_qualifier_argument(Name),
	    Name1 = module_qualifier_body(Name),
	    case type(Name1) of
		arity_qualifier ->
		    F = arity_qualifier_body(Name1),
		    A = arity_qualifier_argument(Name1),
		    {'fun', Pos, {function, M, F, A}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end.

revert_infix_expr(Node) ->
    Operator = infix_expr_operator(Node),
    case type(Operator) of
        %% Note that the operator itself is not revertible out of context.
        operator -> {op, get_pos(Node), operator_name(Operator), infix_expr_left(Node), infix_expr_right(Node)};
        _ -> Node
    end.

revert_integer(Node) -> {integer, get_pos(Node), integer_value(Node)}.

revert_list(Node) ->
    Pos = get_pos(Node),
    lists:foldr(fun(X, A) -> {cons, Pos, X, A} end,
                case list_suffix(Node) of
                    none -> revert_nil(set_pos(nil(), Pos));
                    S -> S
                end,
                list_prefix(Node)).

revert_list_comp(Node) -> {lc, get_pos(Node), list_comp_template(Node), list_comp_body(Node)}.

revert_match_expr(Node) -> {match, get_pos(Node), match_expr_pattern(Node), match_expr_body(Node)}.

revert_module_name(A) ->
    case type(A) of
        atom -> {ok, concrete(A)};
        _ -> error
    end.

revert_module_qualifier(Node) -> {remote, get_pos(Node), module_qualifier_argument(Node), module_qualifier_body(Node)}.

revert_nil(Node) -> {nil, get_pos(Node)}.

revert_parentheses(Node) -> parentheses_body(Node).

revert_prefix_expr(Node) ->
    Operator = prefix_expr_operator(Node),
    case type(Operator) of
        %% Note that the operator itself is not revertible out of context.
        operator -> {op, get_pos(Node), operator_name(Operator), prefix_expr_argument(Node)};
        _ -> Node
    end.

revert_receive_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = lists:map(fun revert_clause/1, receive_expr_clauses(Node)),
    case receive_expr_timeout(Node) of
        none -> {'receive', Pos, Clauses};
        Timeout -> {'receive', Pos, Clauses, Timeout, receive_expr_action(Node)}
    end.

revert_record_access(Node) ->
    Pos = get_pos(Node),
    Argument = record_access_argument(Node),
    Field = record_access_field(Node),
    case record_access_type(Node) of
        none -> {record_field, Pos, Argument, Field};
        Type -> case type(Type) of
                    atom -> {record_field, Pos, Argument, concrete(Type), Field};
                    _ -> Node
                end
    end.

revert_record_expr(Node) ->
    Type = record_expr_type(Node),
    case type(Type) of
        atom ->
            Pos = get_pos(Node),
            T = concrete(Type),
            Fs = fold_record_fields(record_expr_fields(Node)),
            case record_expr_argument(Node) of
                none -> {record, Pos, T, Fs};
                Argument -> {record, Pos, Argument, T, Fs}
            end;
        _ -> Node
    end.

revert_record_index_expr(Node) ->
    Type = record_index_expr_type(Node),
    case type(Type) of
        atom -> {record_index, get_pos(Node), concrete(Type), record_index_expr_field(Node)};
        _ -> Node
    end.

revert_rule(Node) ->
    Name = rule_name(Node),
    case type(Name) of
        atom ->
            {rule, get_pos(Node), concrete(Name), rule_arity(Node), lists:map(fun revert_clause/1, rule_clauses(Node))};
        _ -> Node
    end.

revert_string(Node) -> {string, get_pos(Node), string_value(Node)}.

revert_try_clause(Node) ->
    {clause, Pos, [P], _, _} = C = revert_clause(Node),
    {A, B} = case type(P) of
                 class_qualifier -> {class_qualifier_argument(P), class_qualifier_body(P)};
                 _ -> {{atom, Pos, throw}, P}
             end,
     setelement(3, C, [{tuple, Pos, [A, B, {var, Pos, '_'}]}]).

revert_try_expr(Node) ->
    {'try',
     get_pos(Node),
     try_expr_body(Node),
     lists:map(fun revert_clause/1, try_expr_clauses(Node)),
     lists:map(fun revert_try_clause/1, try_expr_handlers(Node)),
     try_expr_after(Node)}.

revert_tuple(Node) -> {tuple, get_pos(Node), tuple_elements(Node)}.

revert_underscore(Node) -> {var, get_pos(Node), '_'}.

revert_variable(Node) -> {var, get_pos(Node), variable_name(Node)}.

revert_warning_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {warning, warning_marker_info(Node)}.

fold_function_names(Ns) ->
    lists:map(fun(N) ->
                  Name = arity_qualifier_body(N),
                  Arity = arity_qualifier_argument(N),
                  true = (type(Name) =:= atom) and (type(Arity) =:= integer),
                  {concrete(Name), concrete(Arity)}
              end, Ns).

fold_variable_names(Vs) -> lists:map(fun erl_syntax:variable_name/1, Vs).

fold_record_fields(Fs) ->
    lists:map(fun(F) ->
                  Pos = get_pos(F),
                  Name = record_field_name(F),
                  case record_field_value(F) of
                      none -> {record_field, Pos, Name};
                      Value -> {record_field, Pos, Name, Value}
                  end
              end, Fs).

fold_binary_field_types(Ts) ->
    lists:map(fun(Node) ->
                  case type(Node) of
                      size_qualifier -> {concrete(size_qualifier_body(Node)), concrete(size_qualifier_argument(Node))};
                      _ -> concrete(Node)
                  end
              end, Ts).
