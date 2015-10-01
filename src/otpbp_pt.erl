-module(otpbp_pt).
-export([parse_transform/2]).

-import(erl_syntax, [type/1, get_pos/1, set_pos/2]).

parse_transform(Forms, _Options) ->
    case transform_list() of
        [] -> Forms;
        L -> lists:map(fun(Tree) -> erl_syntax:revert(erl_syntax_lib:map(fun(E) -> do_transform(L, E) end, Tree)) end,
                       Forms)
    end.

-define(TRANSFORM_FUNCTIONS, [{{binary_to_integer, 1}, otpbp_erlang},
                              {{binary_to_integer, 2}, otpbp_erlang},
                              {{binary_to_float, 1}, otpbp_erlang},
                              {{integer_to_binary, 1}, otpbp_erlang},
                              {{integer_to_binary, 2}, otpbp_erlang},
                              {{float_to_binary, 1}, otpbp_erlang},
                              {{float_to_binary, 2}, otpbp_erlang},
                              {{float_to_list, 2}, otpbp_erlang},
                              {{get_keys, 0}, otpbp_erlang},
                              {{delete_element, 2}, otpbp_erlang},
                              {{insert_element, 3}, otpbp_erlang},
                              {{erlang, timestamp, 0}, os},
                              {{application, ensure_started, 1}, otpbp_application},
                              {{application, ensure_started, 2}, otpbp_application},
                              {{application, ensure_all_started, 1}, otpbp_application},
                              {{application, ensure_all_started, 2}, otpbp_application},
                              {{application, get_env, 3}, otpbp_application},
                              {{error_handler, raise_undef_exception, 3}, otpbp_error_handler},
                              {{file, list_dir_all, 1}, otpbp_file},
                              {{file, read_link_all, 1}, otpbp_file},
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
                              {{erl_scan, category, 1}, otpbp_erl_scan},
                              {{erl_scan, column, 1}, otpbp_erl_scan},
                              {{erl_scan, line, 1}, otpbp_erl_scan},
                              {{erl_scan, location, 1}, otpbp_erl_scan},
                              {{erl_scan, symbol, 1}, otpbp_erl_scan},
                              {{erl_scan, text, 1}, otpbp_erl_scan},
                              {{erl_scan, continuation_location, 1}, otpbp_erl_scan},
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
                              {{io_lib, write_char_as_latin1, 1}, {io_lib, write_char}},
                              {{io_lib, write_latin1_char, 1}, {io_lib, write_char}},
                              {{io_lib, write_latin1_sring, 1}, {io_lib, write_string}},
                              {{io_lib, write_string_as_latin1, 1}, {io_lib, write_string}},
                              {{io_lib, write_string_as_latin1, 2}, {io_lib, write_string}},
                              {{lists, droplast, 1}, otpbp_lists},
                              {{lists, filtermap, 2}, {lists, zf}},
                              {{orddict, is_empty, 1}, otpbp_orddict},
                              {{os, system_time, 1}, otpbp_os},
                              {{os, getenv, 2}, otpbp_os}]).

dest({_, _} = MF, _) -> MF;
dest(M, F) -> {M, F}.

check_func(M, F, A) -> erlang:is_builtin(M, F, A) orelse (catch lists:member({F, A}, M:module_info(exports))) =:= true.

transform_list() ->
    lists:foldl(fun({{F, A} = FA, D}, Acc) ->
                    case check_func(erlang, F, A) of
                        true -> Acc;
                        _ ->
                            Dest = dest(D, F),
                            dict:store(FA, Dest, dict:store({erlang, FA}, Dest, Acc))
                    end;
                   ({{M, F, A}, D}, Acc) ->
                    case check_func(M, F, A) of
                        true -> Acc;
                        _ -> dict:store({M, {F, A}}, dest(D, F), Acc)
                    end
                end, dict:new(), ?TRANSFORM_FUNCTIONS).

do_transform(L, Node) ->
    case type(Node) of
        application -> application_transform(L, Node);
        implicit_fun -> implicit_fun_transform(L, Node);
        _ -> Node
    end.

application_transform(L, Node) ->
    A = erl_syntax_lib:analyze_application(Node),
    case dict:find(A, L) of
        {ok, {M, N}} ->
            O = erl_syntax:application_operator(Node),
            case A of
                {_, {_, _}} ->
                    ML = get_pos(erl_syntax:module_qualifier_argument(O)),
                    NL = get_pos(erl_syntax:module_qualifier_body(O));
                {_, _} -> ML = NL = get_pos(O)
            end,
            erl_syntax:copy_pos(Node,
                                erl_syntax:application(set_pos(erl_syntax:atom(M), ML),
                                                       set_pos(erl_syntax:atom(N), NL),
                                                       erl_syntax:application_arguments(Node)));
        error -> Node
    end.

-ifdef(buggy__revert_implicit_fun_1a).
revert_implicit_fun_a(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case erl_syntax:type(Name) of
        arity_qualifier ->
            F = erl_syntax:arity_qualifier_body(Name),
            A = erl_syntax:arity_qualifier_argument(Name),
            case type(F) =:= atom andalso type(A) of
                integer -> {'fun', get_pos(Node), {function, erl_syntax:concrete(F), erl_syntax:concrete(A)}};
                _ -> Node
            end;
        _ -> Node
    end.
-else.
revert_implicit_fun_a(N) -> N.
-endif.

-ifdef(buggy__revert_implicit_fun_1m).
revert_implicit_fun_m(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case erl_syntax:type(Name) of
        module_qualifier ->
            N = erl_syntax:module_qualifier_body(Name),
            case type(N) of
                arity_qualifier -> {'fun', get_pos(Node), {function,
                                                           erl_syntax:module_qualifier_argument(Name),
                                                           erl_syntax:arity_qualifier_body(N),
                                                           erl_syntax:arity_qualifier_argument(N)}};
                _ -> Node
            end;
        _ -> Node
    end.
-else.
revert_implicit_fun_m(N) -> N.
-endif.

-compile([{inline, [revert_implicit_fun_a/1, revert_implicit_fun_m/1]}]).

implicit_fun_transform(L, Node) ->
    N = erl_syntax:implicit_fun_name(Node),
    case type(N) of
        arity_qualifier ->
            Name = erl_syntax:arity_qualifier_body(N),
            revert_implicit_fun_a(case type(Name) of
                                      atom ->
                                          NL = get_pos(Name),
                                          implicit_fun_transform(L, Node, {erlang, NL},
                                                                 {erl_syntax:atom_value(Name), NL}, N);
                                      _ -> Node
                                  end);
        module_qualifier ->
            Module = erl_syntax:module_qualifier_argument(N),
            case type(Module) of
                atom ->
                    Arity = erl_syntax:module_qualifier_body(N),
                    case type(Arity) of
                        arity_qualifier ->
                            Name = erl_syntax:arity_qualifier_body(Arity),
                            case type(Name) =:= atom andalso type(erl_syntax:arity_qualifier_argument(Arity)) of
                                integer -> implicit_fun_transform(L, Node, atom_pos(Module), atom_pos(Name), Arity);
                                _ -> Node
                            end;
                        _ -> Node
                    end;
                _ -> revert_implicit_fun_m(Node)
            end;
        _ -> Node
    end.

implicit_fun_transform(L, Node, {Module, ML}, {Name, NL}, Arity) ->
    A = erl_syntax:arity_qualifier_argument(Arity),
    case dict:find({Module, {Name, erl_syntax:integer_value(A)}}, L) of
        {ok, MN} ->
            {M, N} = if
                         is_atom(MN) -> {MN, Name};
                         true -> MN
                     end,
            set_pos(erl_syntax:implicit_fun(atom_pos(M, ML), atom_pos(N, NL), A), ML);
        error -> Node
    end.

atom_pos(Atom, Pos) when is_tuple(Atom), is_tuple(Pos) -> {erl_syntax:atom_value(Atom), get_pos(Pos)};
atom_pos(Atom, Pos) when is_atom(Atom), is_tuple(Pos) -> {Atom, get_pos(Pos)};
atom_pos(Atom, Pos) when is_atom(Atom), is_integer(Pos) -> set_pos(erl_syntax:atom(Atom), Pos).

atom_pos(Atom) when is_tuple(Atom) -> atom_pos(Atom, Atom).
