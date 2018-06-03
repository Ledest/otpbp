%%% Copyright 2015-2018 Oleksandr Chumachenko <ledest@gmail.com>
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

-dialyzer({no_opaque, application_guard/3}).

-define(TRANSFORM_FUNCTIONS, [{{[binary_to_integer, integer_to_binary, float_to_binary], [1, 2]}, otpbp_erlang},
                              {{binary_to_float, 1}, otpbp_erlang},
                              {{get_keys, 0}, otpbp_erlang},
                              {{float_to_list, 2}, otpbp_erlang},
                              {{is_map, 1}, otpbp_erlang},
                              {{is_map_key, 2}, {maps, is_key}},
                              {{map_get, 2}, {maps, get}},
                              {{map_size, 1}, {dict, size}},
                              {{[ceil, floor], 1}, otpbp_erlang},
                              {{erlang, delete_element, 2}, otpbp_erlang},
                              {{erlang, insert_element, 3}, otpbp_erlang},
                              {{erlang, convert_time_unit, 3}, otpbp_erlang},
                              {{erlang, [monotonic_time, system_time, time_offset, unique_integer], [0, 1]}, otpbp_erlang},
                              {{erlang, timestamp, 0}, os},
                              {{application, [ensure_started, ensure_all_started], [1, 2]}, otpbp_application},
                              {{application, get_env, 3}, otpbp_application},
                              {{calendar, [system_time_to_local_time, system_time_to_universal_time], 2}, otpbp_calendar},
                              {{code, modified_modules, 0}, otpbp_code},
                              {{code, module_status, 1}, otpbp_code},
                              {{compile, env_compiler_options, 0}, otpbp_compile},
                              {{crypto, hmac, [3, 4]}, otpbp_crypto},
                              {{crypto, rand_bytes, 1}, {crypto, strong_rand_bytes}},
                              {{crypto, [sha224, sha256, sha384, sha512], 1}, otpbp_crypto},
                              {{crypto, [sha224_final, sha256_final, sha384_final, sha512_final], 1},
                               {crypto, hash_final}},
                              {{crypto, [sha224_init, sha256_init, sha384_init, sha512_init], 0}, otpbp_crypto},
                              {{crypto, [sha224_mac, sha256_mac, sha384_mac, sha512_mac], 2}, otpbp_crypto},
                              {{crypto, [sha224_update, sha256_update, sha384_update, sha512_update], 2},
                               {crypto, hash_update}},
                              {{ct, get_progname, 0}, otpbp_ct},
                              {{error_handler, raise_undef_exception, 3}, otpbp_error_handler},
                              {{error_logger, get_format_depth, 0}, otpbp_error_logger},
                              {{error_logger, limit_term, 1}, otpbp_error_logger},
                              {{file, [list_dir_all, read_link_all], 1}, otpbp_file},
                              {{filename, safe_relative_path, 1}, otpbp_filename},
                              {{inet, ipv4_mapped_ipv6_address, 1}, otpbp_inet},
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
                              {{erl_error, format_exception, [6, 7]}, lib},
                              {{erl_error, format_fun, [1, 2]}, lib},
                              {{erl_error, [format_call, format_stacktrace], [4, 5]}, lib},
                              {{erl_eval, [extended_parse_exprs, extended_parse_term], 1}, lib},
                              {{erl_eval, subst_values_for_vars, 2}, lib},
                              {{erl_scan, [category, column, line, location, symbol, text, continuation_location], 1},
                               otpbp_erl_scan},
                              {{epp, parse_file, 2}, otpbp_epp},
                              {{ets, take, 2}, otpbp_ets},
                              {{ets, update_counter, 4}, otpbp_ets},
                              {{dict, is_empty, 1}, otpbp_dict},
                              {{gb_sets, iterator_from, 2}, otpbp_gb_sets},
                              {{gb_trees, iterator_from, 2}, otpbp_gb_trees},
                              {{gen, get_parent, 0}, otpbp_gen},
                              {{gen, [debug_options, get_proc_name, name, unregister_name], 1}, otpbp_gen},
                              {{gen, debug_options, 2}, otpbp_gen},
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
                              {{io_lib, limit_term, 2}, otpbp_io_lib},
                              {{lib, [flush_receive, progname], 0}, otpbp_lib},
                              {{lib, [eval_str, nonl], 1}, otpbp_lib},
                              {{lib, [error_message, send, sendw], 2}, otpbp_lib},
                              {{lib, format_exception, [6, 7]}, erl_error},
                              {{lib, format_fun, [1, 2]}, erl_error},
                              {{lib, [format_call, format_stacktrace], [4, 5]}, erl_error},
                              {{lib, [extended_parse_exprs, extended_parse_term], 1}, erl_eval},
                              {{lib, subst_values_for_vars, 2}, erl_eval},
                              {{lists, droplast, 1}, otpbp_lists},
                              {{lists, filtermap, 2}, {lists, zf}},
                              {{lists, [join, search], 2}, otpbp_lists},
                              {{maps, new, 0}, dict},
                              {{maps, keys, 1}, {dict, fetch_keys}},
                              {{maps, [from_list, is_key, size, to_list], 1}, dict},
                              {{maps, values, 1}, otpbp_maps},
                              {{maps, [find, map], 2}, dict},
                              {{maps, get, 2}, {dict, fetch}},
                              {{maps, remove, 2}, {dict, erase}},
                              {{maps, [filter, merge, take, with, without], 2}, otpbp_maps},
                              {{maps, fold, 3}, dict},
                              {{maps, [get, update], 3}, otpbp_maps},
                              {{maps, put, 3}, {dict, store}},
                              {{maps, update_with, [3, 4]}, otpbp_maps},
                              {{math, [ceil, floor, log2], 1}, otpbp_math},
                              {{orddict, is_empty, 1}, otpbp_orddict},
                              {{ordsets, is_empty, 1}, otpbp_ordsets},
                              {{os, [cmd, getenv], 2}, otpbp_os},
                              {{os, system_time, 1}, otpbp_os},
                              {{rand, [export_seed, normal], 0}, otpbp_rand},
                              {{rand, [export_seed_s, normal_s], 1}, otpbp_rand},
                              {{rand, [seed, seed_s, uniform_s], [1, 2]}, otpbp_rand},
                              {{rand, uniform, [0, 1]}, otpbp_rand},
                              {{sets, is_empty, 1}, otpbp_sets},
                              {{ssl, connection_information, [1, 2]}, otpbp_ssl},
                              {{string, [casefold, chomp, is_empty, length, lowercase, next_codepoint,
                                         next_grapheme, reverse, titlecase, to_graphemes, uppercase], 1}, otpbp_string},
                              {{string, [lexemes, prefix], 2}, otpbp_string},
                              {{string, nth_lexeme, 3}, otpbp_string},
                              {{string, [equal, pad, take], [2, 3, 4]}, otpbp_string},
                              {{string, [find, slice, split], [2, 3]}, otpbp_string},
                              {{string, replace, [3, 4]}, otpbp_string},
                              {{string, trim, [1, 2, 3]}, otpbp_string},
                              {{supervisor, get_childspec, 2}, otpbp_supervisor},
                              {{uri_string, [is_host, is_path, parse], 1}, otpbp_uri_string},
                              {{zlib, [compress, gzip, zip], 2}, otpbp_zlib}]).

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
-import(otpbp_erl_syntax, [revert/1]).
-else.
-import(erl_syntax, [revert/1]).
-endif.
-import(erl_syntax, [type/1,
                     get_pos/1, copy_pos/2,
                     atom_value/1,
                     application/2, application_arguments/1, application_operator/1,
                     infix_expr/3,
                     match_expr/2,
                     implicit_fun/3, implicit_fun_name/1,
                     arity_qualifier_argument/1, arity_qualifier_body/1,
                     module_qualifier/2, module_qualifier_argument/1, module_qualifier_body/1]).
-import(lists, [foldl/3]).
-ifdef(HAVE_maps__find_2).
-import(maps, [find/2]).
-else.
-import(dict, [find/2]).
-endif.
-ifdef(HAVE_maps__new_0).
-import(maps, [new/0]).
-else.
-import(dict, [new/0]).
-endif.
-ifdef(HAVE_maps__put_3).
-import(maps, [put/3]).
-endif.

-ifndef(HAVE_maps__size_1).
-ifdef(HAVE_dict__is_empty_1).
-import(dict, [is_empty/1]).
-endif.
-endif.

-ifdef(HAVE_maps__without_2).
-import(maps, [without/2]).
-endif.

-ifdef(HAVE_maps__size_1).
-spec is_empty(M::map()) -> boolean().
is_empty(M) -> maps:size(M) =:= 0.
-compile({inline, [is_empty/1]}).
-else.
-ifndef(HAVE_dict__is_empty_1).
-spec is_empty(M::dict()) -> boolean().
is_empty(M) -> dict:size(M) =:= 0.
-compile({inline, [is_empty/1]}).
-endif.
-endif.

-ifndef(HAVE_maps__put_3).
put(K, V, M) -> dict:store(K, V, M).
-compile({inline, [put/3]}).
-endif.

-ifndef(HAVE_maps__without_2).
without(Ks, M) -> lists:foldl(fun dict:erase/2, M, Ks).
-endif.

-record(param, {options = [] :: list(),
                verbose = false :: boolean(),
                otp_release = otp_release() :: non_neg_integer(),
                erts_version = erts_version() :: [non_neg_integer(),...],
                funs,
                file = "" :: string()}).

parse_transform(Forms, Options) ->
    TL = transform_list(),
    case is_empty(TL) of
        true -> Forms;
        _ -> try erl_syntax_lib:analyze_forms(Forms) of
                 AF ->
                     {NF, _} = lists:mapfoldl(fun(Tree, P) -> transform(Tree, P, type(Tree)) end,
                                              #param{options = Options,
                                                     verbose = proplists:get_bool(verbose, Options),
                                                     funs = foldl(fun({M, Fs}, IA) ->
                                                                      foldl(fun(FA, IAM) ->
                                                                                case find({M, FA}, TL) of
                                                                                    {ok, V} -> put(FA, V, IAM);
                                                                                    _ -> IAM
                                                                                end
                                                                             end, IA, Fs)
                                                                  end,
                                                                  without(get_no_auto_import(AF), TL),
                                                                  proplists:get_value(imports, AF, []))},
                                              Forms),
                     NF
            catch
                C:E ->
                    io:fwrite(standard_error,
                              ?MODULE_STRING ": error erl_syntax_lib:analyze_forms/1 {~p:~p}, see below.~n",
                              [C, E]),
                    Forms
            end
    end.

get_no_auto_import(AF) ->
    proplists:append_values(no_auto_import, proplists:append_values(compile, proplists:get_value(attributes, AF, []))).

-compile({inline, [get_no_auto_import/1, transform/3]}).

transform(Tree, P, function) -> {transform_function(Tree, P), P};
transform(Tree, P, attribute) -> {Tree, transform_attribute(Tree, P)};
transform(Tree, P, _) -> {Tree, P}.

transform_function(Tree, P) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case do_transform(case type(E) of
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

-compile({inline, [transform_function/2, transform_attribute/2]}).

add_func(F, MF, D, I) -> foldl(fun(A, Acc) -> add_func(setelement(I, F, A), MF, Acc) end, D, element(I, F)).

add_func(F, MF, D) when is_list(element(tuple_size(F), F)) -> add_func(F, MF, D, tuple_size(F));
add_func(F, MF, D) when is_list(element(tuple_size(F) - 1, F)) -> add_func(F, MF, D, tuple_size(F) - 1);
add_func(FA, MF, D) ->
    case check_func(FA) orelse FA of
        true -> D;
        {M, F, A} -> store_func({M, {F, A}}, MF, D);
        {_, _} -> store_func({erlang, FA}, MF, store_func(FA, MF, D))
    end.

check_func({M, F, A}) ->
    erlang:is_builtin(M, F, A) orelse try M:module_info(exports) of
                                          Exports -> lists:member({F, A}, Exports)
                                      catch
                                          _:_ -> false
                                      end;
check_func({F, A}) -> check_func({erlang, F, A}).

store_func(F, {_, _} = MF, D) -> put(F, MF, D);
store_func({_, {F, _}} = MFA, M, D) -> store_func(MFA, {M, F}, D);
store_func({F, _} = FA, M, D) -> store_func(FA, {M, F}, D).

transform_list() -> foldl(fun({F, D}, Acc) -> add_func(F, D, Acc) end, new(), ?TRANSFORM_FUNCTIONS).
-compile({inline, [transform_list/0]}).

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
do_transform(#param{} = P, Node) ->
    case type(Node) of
        application -> application_transform(P, Node);
        implicit_fun -> implicit_fun_transform(P, Node);
        try_expr -> try_expr_transform(P, Node);
        _ -> false
    end.

application_transform_guard(Node) ->
    case erl_syntax_lib:analyze_application(Node) of
        {M, {N, _}} -> application_guard(Node, M, N);
        _ -> false
    end.

-compile({inline, [application_transform_guard/1]}).

application_guard(Node, dict, size) ->
    AL = [A] = application_arguments(Node),
    O = application_operator(Node),
    ML = module_qualifier_argument(O),
    NL = module_qualifier_body(O),
    copy_pos(Node,
             infix_expr(copy_pos(ML, infix_expr(copy_pos(ML, check_dict(ML, O, A)),
                                                copy_pos(ML, erl_syntax:operator('andalso')),
                                                copy_pos(NL, application(erlang, element, NL, NL, [integer(A, 2)|AL])))),
                        copy_pos(A, erl_syntax:operator('+')),
                        copy_pos(A, integer(A, 0))));
application_guard(Node, otpbp_erlang, is_map) ->
    [A] = application_arguments(Node),
    O = application_operator(Node),
    copy_pos(Node, check_dict(module_qualifier_argument(O), O, A));
application_guard(Node, otpbp_erlang, ceil) -> application_guard_ceil_floor(Node, '+');
application_guard(Node, otpbp_erlang, floor) -> application_guard_ceil_floor(Node, '-');
application_guard(_, _, _) -> false.

application_guard_ceil_floor(Node, Op) ->
    [A] = application_arguments(Node),
    O = application_operator(Node),
    ML = module_qualifier_argument(O),
    copy_pos(Node, application(copy_pos(ML, module_qualifier(atom(ML, erlang), atom(module_qualifier_body(O), round))),
                               [copy_pos(ML, infix_expr(copy_pos(ML, A), copy_pos(ML, erl_syntax:operator(Op)),
                                                        copy_pos(ML, erl_syntax:float(0.5))))])).

check_dict(L, O, A) ->
    application(copy_pos(L, module_qualifier(atom(L, erlang), atom(module_qualifier_body(O), is_record))),
                [A, atom(A, dict), integer(A, tuple_size(dict:new()))]).

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

-compile({inline, [application_transform/2, application/4, application/5]}).

implicit_fun_transform(#param{funs = L} = P, Node) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case find(F, L) of
                 error -> false;
                 {ok, {M, N}} ->
                     Q = implicit_fun_name(Node),
                     {AQ, MP} = case type(Q) of
                                    arity_qualifier -> {Q, arity_qualifier_body(Q)};
                                    module_qualifier -> {module_qualifier_body(Q), module_qualifier_argument(Q)}
                                end,
                     replace_message(F, M, N, Node, P),
                     copy_pos(Node,
                              implicit_fun(atom(MP, M), atom(arity_qualifier_body(AQ), N), arity_qualifier_argument(AQ)))
             end
    catch
        throw:syntax_error -> false
    end.

-compile({inline, [implicit_fun_transform/2]}).

try_expr_transform(#param{erts_version = V}, Node) ->
    V < [10] andalso case lists:mapfoldr(fun(H, A) ->
                                             case try_expr_handler_transform(H) of
                                                 false -> {H, A};
                                                 NH -> {NH, true}
                                             end
                                         end, false, erl_syntax:try_expr_handlers(Node)) of
                         {Hs, true} -> copy_pos(Node, erl_syntax:try_expr(erl_syntax:try_expr_body(Node),
                                                                          erl_syntax:try_expr_clauses(Node),
                                                                          Hs, erl_syntax:try_expr_after(Node)));
                         _ -> false
                     end.

try_expr_handler_transform(Node) ->
    case type(Node) =:= clause andalso try_expr_clause_patterns_transform(erl_syntax:clause_patterns(Node)) of
        {P, [_|_] = B} ->
            copy_pos(Node, erl_syntax:clause(P, erl_syntax:clause_guard(Node), B ++ erl_syntax:clause_body(Node)));
        _ -> false
    end.

try_expr_clause_patterns_transform(Ps) ->
    lists:mapfoldr(fun(P, A) ->
                       case type(P) of
                           class_qualifier ->
                               B = erl_syntax:class_qualifier_body(P),
                               case type(B) of
                                   module_qualifier ->
                                       M = module_qualifier_body(B),
                                       case type(M) of
                                           variable ->
                                               S = copy_pos(M, erl_syntax:application(atom(M, erlang),
                                                                                      atom(M, get_stacktrace), [])),
                                               C = erl_syntax:class_qualifier(erl_syntax:class_qualifier_argument(P),
                                                                              erl_syntax:module_qualifier_argument(B)),
                                               {copy_pos(P, C), [copy_pos(M, match_expr(M, S))|A]};
                                           _ -> {P, A}
                                       end;
                                   _ -> {P, A}
                               end;
                           _ -> {P, A}
                       end
                   end, [], Ps).

-compile({inline, [try_expr_transform/2, try_expr_handler_transform/1, try_expr_clause_patterns_transform/1]}).

atom(P, A) when is_tuple(P), is_atom(A) -> copy_pos(P, erl_syntax:atom(A)).

integer(P, I) when is_tuple(P), is_integer(I) -> copy_pos(P, erl_syntax:integer(I)).

otp_release() ->
    {R, _} = string:to_integer(case erlang:system_info(otp_release) of
                                   [$R|S] -> S;
                                   S -> S
                               end),
    R.

erts_version() -> lists:map(fun list_to_integer/1, string:tokens(erlang:system_info(version), ".")).

-compile({inline, [otp_release/0, erts_version/0]}).

replace_message(_F, _NM, _NN, _Node, #param{verbose = false}) -> ok;
replace_message(F, NM, NN, Node, #param{file = File}) -> do_replace_message(F, NM, NN, File, Node).

do_replace_message({M, {N, A}}, NM, NN, F, Node) -> do_replace_message({lists:concat([M, ":", N]), A}, NM, NN, F, Node);
do_replace_message({N, A}, NM, NN, F, Node) ->
    io:fwrite("~ts:~b: replace ~s/~b to ~s:~s/~b~n", [F, get_pos(Node), N, A, NM, NN, A]).
