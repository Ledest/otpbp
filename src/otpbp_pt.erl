-module(otpbp_pt).
-export([parse_transform/2]).

-import(erl_syntax, [type/1, copy_pos/2, atom_value/1,
                     implicit_fun_name/1,
                     arity_qualifier_argument/1, arity_qualifier_body/1,
                     module_qualifier_argument/1, module_qualifier_body/1]).

parse_transform(Forms, _Options) ->
    TL = transform_list(),
    case dict:size(TL) of
        0 -> Forms;
        _ ->
            L = lists:foldl(fun({M, Fs}, IA) ->
                                lists:foldl(fun(FA, IAM) ->
                                                case dict:find({M, FA}, TL) of
                                                    {ok, V} -> dict:store(FA, V, IAM);
                                                    _ -> IAM
                                                end
                                            end, IA, Fs)
                            end, TL, proplists:get_value(imports, erl_syntax_lib:analyze_forms(Forms), [])),
            [erl_syntax:revert(erl_syntax_lib:map(fun(E) -> do_transform(L, E) end, Tree)) || Tree <- Forms]
    end.

-define(TRANSFORM_FUNCTIONS, [{{[binary_to_integer, integer_to_binary, float_to_binary], [1, 2]}, otpbp_erlang},
                              {{binary_to_float, 1}, otpbp_erlang},
                              {{get_keys, 0}, otpbp_erlang},
                              {{[float_to_list, delete_element], 2}, otpbp_erlang},
                              {{insert_element, 3}, otpbp_erlang},
                              {{erlang, timestamp, 0}, os},
                              {{application, [ensure_started, ensure_all_started], [1, 2]}, otpbp_application},
                              {{application, get_env, 3}, otpbp_application},
                              {{error_handler, raise_undef_exception, 3}, otpbp_error_handler},
                              {{file, [list_dir_all, read_link_all], 1}, otpbp_file},
                              {{inet, ntoa, 1}, inet_parse},
                              {{inet, parse_address, 1}, {inet_parse, address}},
                              {{inet, parse_ipv4_address, 1}, {inet_parse, ipv4_address}},
                              {{inet, parse_ipv4strict_address, 1}, {inet_parse, ipv4strict_address}},
                              {{inet, parse_ipv6_address, 1}, {inet_parse, ipv6_address}},
                              {{inet, parse_ipv6strict_address, 1}, {inet_parse, ipv6strict_address}},
                              {{inet, parse_strict_address, 1}, {otpbp_inet_parse, strict_address}},
                              {{inet_parse, strict_address, 1}, otpbp_inet_parse},
                              {{edlin, current_chars, 1}, otpbp_edlin},
                              {{edlin, start, 2}, otpbp_edlin},
                              {{erl_compile, compile_cmdline, 0}, otpbp_erl_compile},
                              {{erl_scan, [category, column, line, location, symbol, text, continuation_location], 1},
                               otpbp_erl_scan},
                              {{epp, parse_file, 2}, otpbp_epp},
                              {{dict, is_empty, 1}, otpbp_dict},
                              {{gen_event, system_get_state, 1}, otpbp_gen_event},
                              {{gen_event, system_replace_state, 2}, otpbp_gen_event},
                              {{gen_fsm, system_get_state, 1}, otpbp_gen_fsm},
                              {{gen_fsm, system_replace_state, 2}, otpbp_gen_fsm},
                              {{gen_server, system_get_state, 1}, otpbp_gen_server},
                              {{gen_server, system_replace_state, 2}, otpbp_gen_server},
                              {{io_lib, deep_latin1_char_list, 1}, {io_lib, deep_char_list}},
                              {{io_lib, latin1_char_list, 1}, {io_lib, char_list}},
                              {{io_lib, printable_latin1_list, 1}, {io_lib, printable_list}},
                              {{io_lib, [write_char_as_latin1, write_latin1_char], 1}, {io_lib, write_char}},
                              {{io_lib, write_latin1_string, 1}, {io_lib, write_string}},
                              {{io_lib, write_string_as_latin1, [1, 2]}, {io_lib, write_string}},
                              {{lists, droplast, 1}, otpbp_lists},
                              {{lists, filtermap, 2}, {lists, zf}},
                              {{orddict, is_empty, 1}, otpbp_orddict},
                              {{os, system_time, 1}, otpbp_os},
                              {{os, getenv, 2}, otpbp_os}]).

add_func(F, MF, D, I) -> lists:foldl(fun(A, Acc) -> add_func(setelement(I, F, A), MF, Acc) end, D, element(I, F)).

add_func(F, MF, D) when is_list(element(tuple_size(F), F)) -> add_func(F, MF, D, tuple_size(F));
add_func(F, MF, D) when is_list(element(tuple_size(F) - 1, F)) -> add_func(F, MF, D, tuple_size(F) - 1);
add_func(F, MF, D) ->
    case check_func(F) orelse F of
        true -> D;
        {M, F, A} -> store_func({M, {F, A}}, MF, D);
        {_, _} -> store_func({erlang, F}, MF, store_func(F, MF, D))
    end.

check_func({M, F, A}) -> erlang:is_builtin(M, F, A) orelse (catch lists:member({F, A}, M:module_info(exports))) =:= true;
check_func({F, A}) -> check_func({erlang, F, A}).

store_func(F, {_, _} = MF, D) -> dict:store(F, MF, D);
store_func({_, {F, _}} = MFA, M, D) -> store_func(MFA, {M, F}, D);
store_func({F, _} = FA, M, D) -> store_func(FA, {M, F}, D).

transform_list() -> lists:foldl(fun({F, D}, Acc) -> add_func(F, D, Acc) end, dict:new(), ?TRANSFORM_FUNCTIONS).
-compile([{inline, [transform_list/0]}]).

do_transform(L, Node) ->
    case type(Node) of
        application -> application_transform(L, Node);
        implicit_fun -> revert_implicit_fun(implicit_fun_transform(L, Node));
        _ -> Node
    end.
-compile([{inline, [do_transform/2]}]).

application_transform(L, Node) ->
    A = erl_syntax_lib:analyze_application(Node),
    case dict:find(A, L) of
        {ok, {M, N}} ->
            {ML, NL} = module_name_lines(erl_syntax:application_operator(Node), A),
            copy_pos(Node, erl_syntax:application(atom(ML, M), atom(NL, N), erl_syntax:application_arguments(Node)));
        error -> Node
    end.
-compile([{inline, [application_transform/2]}]).

module_name_lines(O, {_, {_, _}}) -> {module_qualifier_argument(O), module_qualifier_body(O)};
module_name_lines(O, {_, _}) -> {O, O}.
-compile([{inline, [module_name_lines/2]}]).

-ifdef(buggy__revert_implicit_fun_1a).
revert_implicit_fun(Node) ->
    case erl_syntax:revert(Node) of
        {'fun', Pos, {function, {atom, _, F}, {integer, _, A}}} -> {'fun', Pos, {function, F, A}};
        _ -> Node
    end.
-else.
-ifdef(buggy__revert_implicit_fun_1m).
revert_implicit_fun(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case erl_syntax:type(Name) of
        module_qualifier ->
            N = erl_syntax:module_qualifier_body(Name),
            case type(N) of
                arity_qualifier -> {'fun', erl_syntax:get_pos(Node),
                                    {function,
                                     erl_syntax:revert(erl_syntax:module_qualifier_argument(Name)),
                                     erl_syntax:revert(erl_syntax:arity_qualifier_body(N)),
                                     erl_syntax:revert(erl_syntax:arity_qualifier_argument(N))}};
                _ -> Node
            end;
        _ -> Node
    end.
-else.
revert_implicit_fun(Node) -> Node.
-endif.
-endif.

-compile([{inline, [revert_implicit_fun/1]}]).

implicit_fun_transform(L, Node) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case dict:find(F, L) of
                 {ok, {M, N}} ->
                     Q = implicit_fun_name(Node),
                     case type(Q) of
                         arity_qualifier ->
                             AQ = Q,
                             MP = arity_qualifier_body(Q);
                         module_qualifier ->
                             AQ = module_qualifier_body(Q),
                             MP = module_qualifier_argument(Q)
                     end,
                     implicit_fun(Node, AQ, MP, M, arity_qualifier_body(AQ), N);
                 error -> Node
             end
    catch
        _:_ -> Node
    end.

implicit_fun(Node, Q, MP, M, NP, N) ->
    copy_pos(Node, erl_syntax:implicit_fun(atom(MP, M), atom(NP, N), arity_qualifier_argument(Q))).

atom(P, A) when is_tuple(P), is_atom(A) -> copy_pos(P, erl_syntax:atom(A)).
