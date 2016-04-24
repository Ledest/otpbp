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

-import(lists, [map/2]).
-import(erl_syntax, [concrete/1, get_pos/1, type/1]).

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
                                false -> erl_syntax:update_tree(Node, map(fun(L) -> map(fun revert/1, L) end,
                                                                                        erl_syntax:subtrees(Node)))
                            end)
    end.

%% Note: The concept of "compatible root node" is not strictly defined.
%% At a minimum, if `make_tree' is used to compose a node `T' from
%% subtrees that are all completely backwards compatible, then the
%% result of `revert_root(T)' should also be completely backwards
%% compatible.

revert_root(Node) ->
    case type(Node) of
        application -> revert_application(Node);
        atom -> revert_atom(Node);
        attribute -> revert_attribute(Node);
        binary -> revert_binary(Node);
        binary_comp -> revert_binary_comp(Node);
        binary_field -> revert_binary_field(Node);
        binary_generator -> revert_binary_generator(Node);
        block_expr -> revert_block_expr(Node);
        case_expr -> revert_case_expr(Node);
        catch_expr -> revert_catch_expr(Node);
        char -> revert_char(Node);
        clause -> revert_clause(Node);
        cond_expr -> revert_cond_expr(Node);
        eof_marker -> revert_eof_marker(Node);
        error_marker -> revert_error_marker(Node);
        float -> revert_float(Node);
        fun_expr -> revert_fun_expr(Node);
        function -> revert_function(Node);
        generator -> revert_generator(Node);
        if_expr -> revert_if_expr(Node);
        implicit_fun -> revert_implicit_fun(Node);
        infix_expr -> revert_infix_expr(Node);
        integer -> revert_integer(Node);
        list -> revert_list(Node);
        list_comp -> revert_list_comp(Node);
        match_expr -> revert_match_expr(Node);
        module_qualifier -> revert_module_qualifier(Node);
        nil -> revert_nil(Node);
        parentheses -> revert_parentheses(Node);
        prefix_expr -> revert_prefix_expr(Node);
        receive_expr -> revert_receive_expr(Node);
        record_access -> revert_record_access(Node);
        record_expr -> revert_record_expr(Node);
        record_index_expr -> revert_record_index_expr(Node);
        rule -> revert_rule(Node);
        string -> revert_string(Node);
        try_expr -> revert_try_expr(Node);
        tuple -> revert_tuple(Node);
        underscore -> revert_underscore(Node);
        variable -> revert_variable(Node);
        warning_marker -> revert_warning_marker(Node);
        %% Non-revertible new-form node
        _ -> Node
    end.

%% =====================================================================
%% @doc Removes any wrapper structure, if present. If `Node'
%% is a wrapper structure, this function returns the wrapped
%% `erl_parse' tree; otherwise it returns `Node'
%% itself.

-spec unwrap(syntaxTree()) -> #tree{} | erl_parse().
unwrap(#wrapper{tree = Node}) -> Node;
unwrap(Node) -> Node. % This could also be a new-form node.

revert_application(Node) ->
    {call, get_pos(Node), erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node)}.

revert_atom(Node) -> {atom, get_pos(Node), erl_syntax:atom_value(Node)}.

revert_attribute(Node) ->
    Name = erl_syntax:attribute_name(Node),
    case type(Name) of
        atom -> revert_attribute_1(erl_syntax:atom_value(Name), erl_syntax:attribute_arguments(Node), get_pos(Node),
                                   Node);
        _ -> Node
    end.

%% All the checking makes this part a bit messy:

revert_attribute_1(module, [M], Pos, Node) ->
    case revert_module_name(M) of
        {ok, A} -> {attribute, Pos, module, A};
        error -> Node
    end;
revert_attribute_1(module, [M, List], Pos, Node) ->
    case revert_module_name(M) of
        {ok, A} -> {attribute, Pos, module,
                    {A, case erl_syntax:is_list_skeleton(List) andalso erl_syntax:is_proper_list(List) of
                            true -> fold_variable_names(erl_syntax:list_elements(List));
                            false -> Node
                        end}};
        error -> Node
    end;
revert_attribute_1(export, [List], Pos, Node) ->
    case erl_syntax:is_list_skeleton(List) andalso erl_syntax:is_proper_list(List) of
        true -> {attribute, Pos, export, fold_function_names(erl_syntax:list_elements(List))};
        false -> Node
    end;
revert_attribute_1(import, [M], Pos, Node) ->
    case revert_module_name(M) of
        {ok, A} -> {attribute, Pos, import, A};
        error -> Node
    end;
revert_attribute_1(import, [M, List], Pos, Node) ->
    case revert_module_name(M) of
        {ok, A} -> case erl_syntax:is_list_skeleton(List) andalso erl_syntax:is_proper_list(List) of
                       true -> {attribute, Pos, import, {A, fold_function_names(erl_syntax:list_elements(List))}};
                       false -> Node
                   end;
        error -> Node
    end;
revert_attribute_1(file, [A, Line], Pos, Node) ->
    case type(A) =:= string andalso type(Line) of
        integer -> {attribute, Pos, file, {concrete(A), concrete(Line)}};
        _ -> Node
    end;
revert_attribute_1(record, [A, Tuple], Pos, Node) ->
    case type(A) =:= atom andalso type(Tuple) of
        tuple -> {attribute, Pos, record, {concrete(A), fold_record_fields(erl_syntax:tuple_elements(Tuple))}};
        _ -> Node
    end;
revert_attribute_1(N, [T], Pos, _) -> {attribute, Pos, N, concrete(T)};
revert_attribute_1(_, _, _, Node) -> Node.

revert_binary(Node) -> {bin, get_pos(Node), erl_syntax:binary_fields(Node)}.

revert_binary_comp(Node) ->
    {bc, get_pos(Node), erl_syntax:binary_comp_template(Node), erl_syntax:binary_comp_body(Node)}.

revert_binary_field(Node) ->
    Body = erl_syntax:binary_field_body(Node),
    {Expr, Size} = case type(Body) of
                       %% Note that size qualifiers are not revertible out of context.
                       size_qualifier -> {erl_syntax:size_qualifier_body(Body),
                                          erl_syntax:size_qualifier_argument(Body)};
                       _ -> {Body, default}
                   end,
    {bin_element, get_pos(Node), Expr, Size,
     case erl_syntax:binary_field_types(Node) of
         [] -> default;
         Ts -> fold_binary_field_types(Ts)
     end}.

revert_binary_generator(Node) ->
    {b_generate, get_pos(Node), erl_syntax:binary_generator_pattern(Node), erl_syntax:binary_generator_body(Node)}.

revert_block_expr(Node) -> {block, get_pos(Node), erl_syntax:block_expr_body(Node)}.

revert_case_expr(Node) ->
    {'case', get_pos(Node),
     erl_syntax:case_expr_argument(Node), map(fun revert_clause/1, erl_syntax:case_expr_clauses(Node))}.

revert_catch_expr(Node) -> {'catch', get_pos(Node), erl_syntax:catch_expr_body(Node)}.

revert_char(Node) -> {char, get_pos(Node), erl_syntax:char_value(Node)}.

revert_cond_expr(Node) -> {'cond', get_pos(Node), list:map(fun revert_clause/1, erl_syntax:cond_expr_clauses(Node))}.

revert_clause(Node) ->
    {clause, get_pos(Node), erl_syntax:clause_patterns(Node),
     case erl_syntax:clause_guard(Node) of
         none -> [];
         E -> case type(E) of
                  disjunction -> revert_clause_disjunction(E);
                  %% Only the top level expression is unfolded here; no recursion.
                  conjunction -> [erl_syntax:conjunction_body(E)];
                  _ -> [[E]] % a single expression
              end
     end,
     erl_syntax:clause_body(Node)}.

revert_clause_disjunction(D) ->
    %% We handle conjunctions within a disjunction, but only at the top level; no recursion.
    map(fun(E) ->
            case type(E) of
                conjunction -> erl_syntax:conjunction_body(E);
                _ -> [E]
            end
        end, erl_syntax:disjunction_body(D)).

revert_eof_marker(Node) -> {eof, get_pos(Node)}.

%% Note that the position information of the node itself is not preserved.
revert_error_marker(Node) -> {error, erl_syntax:error_marker_info(Node)}.

revert_float(Node) -> {float, get_pos(Node), erl_syntax:float_value(Node)}.

revert_fun_expr(Node) -> {'fun', get_pos(Node), {clauses, map(fun revert_clause/1, erl_syntax:fun_expr_clauses(Node))}}.

revert_function(Node) ->
    Name = erl_syntax:function_name(Node),
    case type(Name) of
        atom -> {function, get_pos(Node), concrete(Name), erl_syntax:function_arity(Node),
                 map(fun revert_clause/1, erl_syntax:function_clauses(Node))};
        _ -> Node
    end.

revert_generator(Node) -> {generate, get_pos(Node), erl_syntax:generator_pattern(Node), erl_syntax:generator_body(Node)}.

revert_if_expr(Node) -> {'if', get_pos(Node), map(fun revert_clause/1, erl_syntax:if_expr_clauses(Node))}.

revert_implicit_fun(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case type(Name) of
        arity_qualifier ->
            F = erl_syntax:arity_qualifier_body(Name),
            A = erl_syntax:arity_qualifier_argument(Name),
            case type(F) =:= atom andalso type(A) of
                integer -> {'fun', get_pos(Node), {function, concrete(F), concrete(A)}};
                _ -> Node
            end;
        module_qualifier ->
            Name1 = erl_syntax:module_qualifier_body(Name),
            case type(Name1) of
                arity_qualifier -> {'fun', get_pos(Node), {function, erl_syntax:module_qualifier_argument(Name),
                                    erl_syntax:arity_qualifier_body(Name1), erl_syntax:arity_qualifier_argument(Name1)}};
                _ -> Node
            end;
        _ -> Node
    end.

revert_infix_expr(Node) ->
    Operator = erl_syntax:infix_expr_operator(Node),
    case type(Operator) of
        %% Note that the operator itself is not revertible out of context.
        operator -> {op, get_pos(Node), erl_syntax:operator_name(Operator),
                     erl_syntax:infix_expr_left(Node), erl_syntax:infix_expr_right(Node)};
        _ -> Node
    end.

revert_integer(Node) -> {integer, get_pos(Node), erl_syntax:integer_value(Node)}.

revert_list(Node) ->
    Pos = get_pos(Node),
    lists:foldr(fun(X, A) -> {cons, Pos, X, A} end,
                case erl_syntax:list_suffix(Node) of
                    none -> revert_nil(erl_syntax:set_pos(erl_syntax:nil(), Pos));
                    S -> S
                end,
                erl_syntax:list_prefix(Node)).

revert_list_comp(Node) -> {lc, get_pos(Node), erl_syntax:list_comp_template(Node), erl_syntax:list_comp_body(Node)}.

revert_match_expr(Node) -> {match, get_pos(Node), erl_syntax:match_expr_pattern(Node), erl_syntax:match_expr_body(Node)}.

revert_module_name(A) ->
    case type(A) of
        atom -> {ok, concrete(A)};
        _ -> error
    end.

revert_module_qualifier(Node) ->
    {remote, get_pos(Node), erl_syntax:module_qualifier_argument(Node), erl_syntax:module_qualifier_body(Node)}.

revert_nil(Node) -> {nil, get_pos(Node)}.

revert_parentheses(Node) -> erl_syntax:parentheses_body(Node).

revert_prefix_expr(Node) ->
    Operator = erl_syntax:prefix_expr_operator(Node),
    case type(Operator) of
        %% Note that the operator itself is not revertible out of context.
        operator -> {op, get_pos(Node), erl_syntax:operator_name(Operator), erl_syntax:prefix_expr_argument(Node)};
        _ -> Node
    end.

revert_receive_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = map(fun revert_clause/1, erl_syntax:receive_expr_clauses(Node)),
    case erl_syntax:receive_expr_timeout(Node) of
        none -> {'receive', Pos, Clauses};
        Timeout -> {'receive', Pos, Clauses, Timeout, erl_syntax:receive_expr_action(Node)}
    end.

revert_record_access(Node) ->
    Pos = get_pos(Node),
    Argument = erl_syntax:record_access_argument(Node),
    Field = erl_syntax:record_access_field(Node),
    case erl_syntax:record_access_type(Node) of
        none -> {record_field, Pos, Argument, Field};
        Type -> case type(Type) of
                    atom -> {record_field, Pos, Argument, concrete(Type), Field};
                    _ -> Node
                end
    end.

revert_record_expr(Node) ->
    Type = erl_syntax:record_expr_type(Node),
    case type(Type) of
        atom ->
            Pos = get_pos(Node),
            T = concrete(Type),
            Fs = fold_record_fields(erl_syntax:record_expr_fields(Node)),
            case erl_syntax:record_expr_argument(Node) of
                none -> {record, Pos, T, Fs};
                Argument -> {record, Pos, Argument, T, Fs}
            end;
        _ -> Node
    end.

revert_record_index_expr(Node) ->
    Type = erl_syntax:record_index_expr_type(Node),
    case type(Type) of
        atom -> {record_index, get_pos(Node), concrete(Type), erl_syntax:record_index_expr_field(Node)};
        _ -> Node
    end.

revert_rule(Node) ->
    Name = erl_syntax:rule_name(Node),
    case type(Name) of
        atom ->
            {rule, get_pos(Node),
             concrete(Name), erl_syntax:rule_arity(Node), map(fun revert_clause/1, erl_syntax:rule_clauses(Node))};
        _ -> Node
    end.

revert_string(Node) -> {string, get_pos(Node), erl_syntax:string_value(Node)}.

revert_try_clause(Node) ->
    {clause, Pos, [P], _, _} = C = revert_clause(Node),
    {A, B} = case type(P) of
                 class_qualifier -> {erl_syntax:class_qualifier_argument(P), erl_syntax:class_qualifier_body(P)};
                 _ -> {{atom, Pos, throw}, P}
             end,
     setelement(3, C, [{tuple, Pos, [A, B, {var, Pos, '_'}]}]).

revert_try_expr(Node) ->
    {'try',
     get_pos(Node),
     erl_syntax:try_expr_body(Node),
     map(fun revert_clause/1, erl_syntax:try_expr_clauses(Node)),
     map(fun revert_try_clause/1, erl_syntax:try_expr_handlers(Node)),
     erl_syntax:try_expr_after(Node)}.

revert_tuple(Node) -> {tuple, get_pos(Node), erl_syntax:tuple_elements(Node)}.

revert_underscore(Node) -> {var, get_pos(Node), '_'}.

revert_variable(Node) -> {var, get_pos(Node), erl_syntax:variable_name(Node)}.

revert_warning_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {warning, erl_syntax:warning_marker_info(Node)}.

fold_function_names(Ns) ->
    map(fun(N) ->
            Name = erl_syntax:arity_qualifier_body(N),
            Arity = erl_syntax:arity_qualifier_argument(N),
            true = (type(Name) =:= atom) and (type(Arity) =:= integer),
            {concrete(Name), concrete(Arity)}
        end, Ns).

fold_variable_names(Vs) -> map(fun erl_syntax:variable_name/1, Vs).

fold_record_fields(Fs) ->
    map(fun(F) ->
            Pos = get_pos(F),
            Name = erl_syntax:record_field_name(F),
            case erl_syntax:record_field_value(F) of
                none -> {record_field, Pos, Name};
                Value -> {record_field, Pos, Name, Value}
            end
        end, Fs).

fold_binary_field_types(Ts) ->
    map(fun(Node) ->
            case type(Node) of
                size_qualifier -> {concrete(erl_syntax:size_qualifier_body(Node)),
                                   concrete(erl_syntax:size_qualifier_argument(Node))};
                _ -> concrete(Node)
            end
        end, Ts).

-compile([{inline, [fold_binary_field_types/1, fold_variable_names/1,
                    revert_application/1, revert_atom/1, revert_attribute/1,
                    revert_binary/1, revert_binary_comp/1, revert_binary_field/1, revert_binary_generator/1,
                    revert_block_expr/1,
                    revert_case_expr/1, revert_catch_expr/1, revert_char/1, revert_clause_disjunction/1,
                    revert_cond_expr/1,
                    revert_eof_marker/1, revert_error_marker/1,
                    revert_float/1, revert_fun_expr/1, revert_function/1,
                    revert_generator/1,
                    revert_if_expr/1, revert_implicit_fun/1, revert_infix_expr/1, revert_integer/1,
                    revert_list/1, revert_list_comp/1,
                    revert_match_expr/1, revert_module_qualifier/1,
                    revert_parentheses/1, revert_prefix_expr/1,
                    revert_receive_expr/1, revert_record_access/1, revert_record_expr/1, revert_record_index_expr/1,
                    revert_rule/1,
                    revert_string/1,
                    revert_try_expr/1, revert_tuple/1,
                    revert_underscore/1,
                    revert_variable/1,
                    revert_warning_marker/1,
                    unwrap/1]}]).
-endif.
