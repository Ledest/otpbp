%%% Copyright 2015 Oleksandr Chumachenko <ledest@gmail.com>
%%%
%%% This file is part of OTPBP.
%%%
%%% OTPBP is free software: you can redistribute it and/or modify it
%%% under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% OTPBP is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with OTPBP. If not, see <http://www.gnu.org/licenses/>.

-module(otpbp_pt).
-export([parse_transform/2]).

-define(TRANSFORM_FUNCTIONS, [{{[binary_to_integer, integer_to_binary, float_to_binary], [1, 2]}, otpbp_erlang},
                              {{binary_to_float, 1}, otpbp_erlang},
                              {{get_keys, 0}, otpbp_erlang},
                              {{[float_to_list, delete_element], 2}, otpbp_erlang},
                              {{insert_element, 3}, otpbp_erlang},
                              {{is_map, 1}, otpbp_erlang},
                              {{map_size, 1}, {dict, size}},
                              {{erlang, convert_time_unit, 3}, otpbp_erlang},
                              {{erlang, monotonic_time, [0, 1]}, {otpbp_erlang, system_time}},
                              {{erlang, [system_time, time_offset], [0, 1]}, otpbp_erlang},
                              {{erlang, timestamp, 0}, {erlang, now}},
                              {{erlang, unique_integer, 0}, {otpbp_erlang, system_time}},
                              {{erlang, unique_integer, 1}, otpbp_erlang},
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
                              {{gb_sets, iterator_from, 2}, otpbp_gb_sets},
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
                              {{maps, new, 0}, dict},
                              {{maps, keys, 1}, {dict, fetch_keys}},
                              {{maps, [from_list, is_key, size, to_list], 1}, dict},
                              {{maps, values, 1}, otpbp_maps},
                              {{maps, [filter, find, map], 2}, dict},
                              {{maps, get, 2}, {dict, fetch}},
                              {{maps, remove, 2}, {dict, erase}},
                              {{maps, [merge, with, without], 2}, otpbp_maps},
                              {{maps, get, 3}, otpbp_maps},
                              {{maps, [fold, update], 3}, dict},
                              {{maps, put, 3}, {dict, store}},
                              {{orddict, is_empty, 1}, otpbp_orddict},
                              {{os, system_time, 1}, otpbp_os},
                              {{os, getenv, 2}, otpbp_os}]).

-import(erl_syntax, [type/1,
                     get_pos/1, copy_pos/2,
                     atom_value/1,
                     revert/1,
                     application/2, application_arguments/1, application_operator/1,
                     infix_expr/3,
                     match_expr/2,
                     implicit_fun/3, implicit_fun_name/1,
                     arity_qualifier_argument/1, arity_qualifier_body/1,
                     module_qualifier/2, module_qualifier_argument/1, module_qualifier_body/1]).
-import(erl_syntax_lib, [analyze_forms/1]).
-import(dict, [store/3, find/2]).
-import(lists, [foldl/3]).

-record(param, {options = [] :: list(),
                funs,
                file = "" :: string()}).

parse_transform(Forms, Options) ->
    TL = transform_list(),
    case is_empty(TL) of
        true -> Forms;
        _ ->
            AF = analyze_forms(Forms),
            element(1, lists:mapfoldl(fun(Tree, P) ->
                                          case type(Tree) of
                                              function -> {transform_function(Tree, P), P};
                                              attribute -> {Tree, transform_attribute(Tree, P)};
                                              _ -> {Tree, P}
                                          end
                                      end,
                                      #param{options = Options,
                                             funs = foldl(fun({M, Fs}, IA) ->
                                                              foldl(fun(FA, IAM) ->
                                                                        case find({M, FA}, TL) of
                                                                            {ok, V} -> store(FA, V, IAM);
                                                                            _ -> IAM
                                                                        end
                                                                    end, IA, Fs)
                                                          end,
                                                          foldl(fun dict:erase/2, TL, get_no_auto_import(AF)),
                                                          get_imports(AF))},
                                      Forms))
    end.

gl(K, L) -> proplists:get_value(K, L, []).

get_no_auto_import(AF) ->
    lists:flatten(proplists:get_all_values(no_auto_import, proplists:get_all_values(compile, gl(attributes, AF)))).

get_imports(AF) -> gl(imports, AF).

-compile([{inline, [get_imports/1, get_no_auto_import/1]}]).

transform_function(Tree, P) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case do_transform(case F andalso type(E) of
                                                          conjunction -> conjunction;
                                                          _ -> P
                                                      end, E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> revert(T);
        _ -> Tree
    end.

transform_attribute(Tree, P) ->
    case erl_syntax_lib:analyze_attribute(Tree) of
        {file, {F, _}} -> P#param{file = F};
        _ -> P
    end.

-compile([{inline, [transform_function/2, transform_attribute/2]}]).

add_func(F, MF, D, I) -> foldl(fun(A, Acc) -> add_func(setelement(I, F, A), MF, Acc) end, D, element(I, F)).

add_func(F, MF, D) when is_list(element(tuple_size(F), F)) -> add_func(F, MF, D, tuple_size(F));
add_func(F, MF, D) when is_list(element(tuple_size(F) - 1, F)) -> add_func(F, MF, D, tuple_size(F) - 1);
add_func(FA, MF, D) ->
    case check_func(FA) orelse FA of
        true -> D;
        {M, F, A} -> store_func({M, {F, A}}, MF, D);
        {_, _} -> store_func({erlang, FA}, MF, store_func(FA, MF, D))
    end.

check_func({M, F, A}) -> erlang:is_builtin(M, F, A) orelse (catch lists:member({F, A}, M:module_info(exports))) =:= true;
check_func({F, A}) -> check_func({erlang, F, A}).

store_func(F, {_, _} = MF, D) -> store(F, MF, D);
store_func({_, {F, _}} = MFA, M, D) -> store_func(MFA, {M, F}, D);
store_func({F, _} = FA, M, D) -> store_func(FA, {M, F}, D).

transform_list() -> foldl(fun({F, D}, Acc) -> add_func(F, D, Acc) end, dict:new(), ?TRANSFORM_FUNCTIONS).
-compile([{inline, [transform_list/0]}]).

-ifdef(HAVE_dict__is_empty_1).
is_empty(D) -> dict:is_empty(D).
-else.
is_empty(D) -> dict:size(D) =:= 0.
-endif.
-compile([{inline, [is_empty/1]}]).

do_transform(conjunction, Tree) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case type(E) =:= application andalso application_transform_guard(E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> T;
        _ -> Tree
    end;
do_transform(P, Node) when is_record(P, param) ->
    case type(Node) of
        application -> application_transform(P, Node);
        implicit_fun -> revert_implicit_fun(implicit_fun_transform(P, Node));
        _ -> false
    end.

application_transform_guard(Node) ->
    case erl_syntax_lib:analyze_application(Node) of
        {M, {N, _}} -> application_guard(Node, M, N);
        _ -> false
    end.

-compile([{inline, [application_transform_guard/1]}]).

application_guard(Node, dict, size) ->
    [A] = application_arguments(Node),
    O = application_operator(Node),
    ML = module_qualifier_argument(O),
    NL = module_qualifier_body(O),
    copy_pos(Node,
             erl_syntax:parentheses(copy_pos(ML,
                                             infix_expr(copy_pos(ML,
                                                                 match_expr(atom(ML, true),
                                                                            copy_pos(ML,
                                                                                     application(otpbp_erlang, is_map,
                                                                                                 ML, ML,
                                                                                                 [copy_pos(ML, A)])))),
                                                        copy_pos(ML, erl_syntax:operator('andalso')),
                                                        copy_pos(NL, application(erlang, element, NL, NL,
                                                                                 [integer(A, 2), A]))))));
application_guard(Node, otpbp_erlang, is_map) ->
    [A] = application_arguments(Node),
    O = application_operator(Node),
    ML = module_qualifier_argument(O),
    copy_pos(Node,
             application(copy_pos(ML, module_qualifier(atom(ML, erlang), atom(module_qualifier_body(O), is_record))),
                         [A, atom(A, dict), integer(A, tuple_size(dict:new()))]));
application_guard(_, _, _) -> false.

application_transform(#param{funs = L} = P, Node) ->
    AA = erl_syntax_lib:analyze_application(Node),
    case find(AA, L) of
        error -> false;
        {ok, {M, N}} ->
            replace_message(AA, M, N, Node, P),
            application(Node, AA, M, N)
    end.

application(Node, AA, M, N) ->
    O = application_operator(Node),
    case AA of
        {_, {_, _}} ->
            ML = module_qualifier_argument(O),
            NL = module_qualifier_body(O);
        {_, _} -> ML = NL = O
    end,
    copy_pos(Node, application(M, N, ML, NL, application_arguments(Node))).

application(M, N, ML, NL, A) -> application(copy_pos(ML, module_qualifier(atom(ML, M), atom(NL, N))), A).

-compile([{inline, [application_transform/2, application/4, application/5]}]).

-ifdef(buggy__revert_implicit_fun_1a).
-define(ORIG_IMPLICIT_FUN, Node).
revert_implicit_fun(Node) ->
    case revert(Node) of
        {'fun', Pos, {function, {atom, _, F}, {integer, _, A}}} -> {'fun', Pos, {function, F, A}};
        _ -> Node
    end.
-else.
-ifdef(buggy__revert_implicit_fun_1m).
-define(ORIG_IMPLICIT_FUN, Node).
revert_implicit_fun(Node) ->
    Name = erl_syntax:implicit_fun_name(Node),
    case type(Name) of
        module_qualifier ->
            N = module_qualifier_body(Name),
            case type(N) of
                arity_qualifier -> {'fun', get_pos(Node), {function,
                                                           revert(module_qualifier_argument(Name)),
                                                           revert(arity_qualifier_body(N)),
                                                           revert(arity_qualifier_argument(N))}};
                _ -> Node
            end;
        _ -> Node
    end.
-else.
-define(ORIG_IMPLICIT_FUN, false).
revert_implicit_fun(Node) -> Node.
-endif.
-endif.

-compile([{inline, [revert_implicit_fun/1]}]).

implicit_fun_transform(#param{funs = L} = P, Node) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case find(F, L) of
                 error -> ?ORIG_IMPLICIT_FUN;
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
                     replace_message(F, M, N, Node, P),
                     copy_pos(Node,
                              implicit_fun(atom(MP, M), atom(arity_qualifier_body(AQ), N), arity_qualifier_argument(AQ)))
             end
    catch
        throw:syntax_error -> ?ORIG_IMPLICIT_FUN
    end.

-compile([{inline, [implicit_fun_transform/2]}]).

atom(P, A) when is_tuple(P), is_atom(A) -> copy_pos(P, erl_syntax:atom(A)).

integer(P, I) when is_tuple(P), is_integer(I) -> copy_pos(P, erl_syntax:integer(I)).

replace_message(F, NM, NN, Node, #param{options = O} = P) ->
    proplists:get_value(verbose, O) =:= true andalso do_replace_message(F, NM, NN, P#param.file, Node).

do_replace_message({M, {N, A}}, NM, NN, F, Node) -> do_replace_message({lists:concat([M, ":", N]), A}, NM, NN, F, Node);
do_replace_message({N, A}, NM, NN, F, Node) ->
    io:fwrite("~ts:~b: replace ~s/~b to ~s:~s/~b~n", [F, get_pos(Node), N, A, NM, NN, A]).
