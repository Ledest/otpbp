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

-define(TRANSFORM_FUNCTIONS, [{{get_keys, 0}, otpbp_erlang},
                              {{is_map, 1}, otpbp_erlang},
                              {{is_map_key, 2}, {maps, is_key}},
                              {{map_get, 2}, {maps, get}},
                              {{map_size, 1}, {dict, size}},
                              {{[ceil, floor], 1}, otpbp_erlang},
                              {{[atom_to_binary, binary_to_atom, binary_to_existing_atom], 1}, otpbp_erlang},
                              {{erlang, convert_time_unit, 3}, otpbp_erlang},
                              {{erlang, [monotonic_time, system_time, time_offset, unique_integer], [0, 1]}, otpbp_erlang},
                              {{erlang, timestamp, 0}, os},
                              {{application, set_env, [1, 2]}, otpbp_application},
                              {{calendar, [system_time_to_local_time, system_time_to_universal_time], 2}, otpbp_calendar},
                              {{code, modified_modules, 0}, otpbp_code},
                              {{code, module_status, 1}, otpbp_code},
                              {{compile, env_compiler_options, 0}, otpbp_compile},
                              {{crypto, rand_bytes, 1}, {crypto, strong_rand_bytes}},
                              {{crypto, [rand_seed, supports], 1}, otpbp_crypto},
                              {{crypto, [sha224, sha256, sha384, sha512], 1}, otpbp_crypto},
                              {{crypto, [sha224_final, sha256_final, sha384_final, sha512_final], 1},
                               {crypto, hash_final}},
                              {{crypto, [sha224_init, sha256_init, sha384_init, sha512_init], 0}, otpbp_crypto},
                              {{crypto, [sha224_mac, sha256_mac, sha384_mac, sha512_mac], 2}, otpbp_crypto},
                              {{crypto, [sha224_update, sha256_update, sha384_update, sha512_update], 2},
                               {crypto, hash_update}},
                              {{ct, [get_event_mgr_ref, get_progname], 0}, otpbp_ct},
                              {{dict, take, 2}, otpbp_dict},
                              {{epp, open, 1}, otpbp_epp},
                              {{erl_anno, new, 1}, otpbp_erl_anno},
                              {{erl_compile, compile_cmdline, 0}, otpbp_erl_compile},
                              {{erl_error, format_fun, [1, 2]}, lib},
                              {{erl_error, [format_call, format_stacktrace], [4, 5]}, lib},
                              {{erl_error, format_exception, [6, 7]}, lib},
                              {{erl_eval, [eval_str, extended_parse_exprs, extended_parse_term], 1}, lib},
                              {{erl_eval, subst_values_for_vars, 2}, lib},
                              {{erl_scan, [category, column, line, location, symbol, text], 1}, otpbp_erl_scan},
                              {{erl_syntax, record_access, 2}, otpbp_erl_syntax},
                              {{error_logger, get_format_depth, 0}, otpbp_error_logger},
                              {{error_logger, limit_term, 1}, otpbp_error_logger},
                              {{ets, take, 2}, otpbp_ets},
                              {{ets, update_counter, 4}, otpbp_ets},
                              {{filename, safe_relative_path, 1}, otpbp_filename},
                              {{gb_sets, iterator_from, 2}, otpbp_gb_sets},
                              {{gb_trees, [iterator_from, take, take_any], 2}, otpbp_gb_trees},
                              {{gen, get_parent, 0}, otpbp_gen},
                              {{gen, [debug_options, get_proc_name, name, unregister_name], 1}, otpbp_gen},
                              {{gen, debug_options, 2}, otpbp_gen},
                              {{gen_event, system_get_state, 1}, otpbp_gen_event},
                              {{gen_event, system_replace_state, 2}, otpbp_gen_event},
                              {{inet, ipv4_mapped_ipv6_address, 1}, otpbp_inet},
                              {{io_lib, limit_term, 2}, otpbp_io_lib},
                              {{lib, [flush_receive, progname], 0}, otpbp_lib},
                              {{lib, nonl, 1}, otpbp_lib},
                              {{lib, [error_message, send, sendw], 2}, otpbp_lib},
                              {{lib, [eval_str, extended_parse_exprs, extended_parse_term], 1}, erl_eval},
                              {{lib, [format_call, format_stacktrace], [4, 5]}, erl_error},
                              {{lib, format_exception, [6, 7]}, erl_error},
                              {{lib, format_fun, [1, 2]}, erl_error},
                              {{lib, subst_values_for_vars, 2}, erl_eval},
                              {{lists, [join, search], 2}, otpbp_lists},
                              {{maps, new, 0}, dict},
                              {{maps, [from_list, keys, size, to_list, values], 1}, otpbp_maps},
                              {{maps, [filter, find, get, is_key, map, merge, remove, take, with, without], 2},
                               otpbp_maps},
                              {{maps, [fold, get, put, update, update_with], 3}, otpbp_maps},
                              {{maps, update_with, 4}, otpbp_maps},
                              {{math, [ceil, floor, log2], 1}, otpbp_math},
                              {{orddict, take, 2}, otpbp_orddict},
                              {{ordsets, is_empty, 1}, otpbp_ordsets},
                              {{os, [cmd, getenv], 2}, otpbp_os},
                              {{os, system_time, 1}, otpbp_os},
                              {{rand, [export_seed, normal, uniform], 0}, otpbp_rand},
                              {{rand, [export_seed_s, normal_s, seed, seed_s, uniform, uniform_s], 1}, otpbp_rand},
                              {{rand, [seed, seed_s, uniform_s], 2}, otpbp_rand},
                              {{sets, is_empty, 1}, otpbp_sets},
                              {{ssl, connection_information, [1, 2]}, otpbp_ssl},
                              {{string, [casefold, chomp, is_empty, length, lowercase, next_codepoint,
                                         next_grapheme, reverse, titlecase, to_graphemes, trim, uppercase], 1},
                               otpbp_string},
                              {{string, [equal, find, lexemes, pad, prefix, slice, split, take, trim], 2}, otpbp_string},
                              {{string, [equal, find, nth_lexeme, pad, replace, slice, split, take, trim], 3},
                               otpbp_string},
                              {{string, [equal, pad, replace, take], 4}, otpbp_string},
                              {{supervisor, get_callback_module, 1}, otpbp_supervisor},
                              {{supervisor, [format_status, get_childspec, try_again_restart], 2}, otpbp_supervisor},
                              {{unicode_util, [spec_version, whitespace], 0}, otpbp_unicode_util},
                              {{unicode_util, [casefold, cp, gc, get_case, is_whitespace, lookup, lowercase, nfc, nfd,
                                               nfkc, nfkd, titlecase,uppercase], 1},
                               otpbp_unicode_util},
                              {{uri_string, [compose_query, normalize], [1, 2]}, otpbp_uri_string},
                              {{uri_string, [dissect_query, is_host, is_path, parse, recompose], 1}, otpbp_uri_string},
                              {{uri_string, transcode, 2}, otpbp_uri_string},
                              {{zlib, [compress, gzip, zip], 2}, otpbp_zlib}]).

-import(erl_syntax, [copy_pos/2]).
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
                     {NF, _} = lists:mapfoldl(fun(Tree, P) -> transform(Tree, P, erl_syntax:type(Tree)) end,
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
                                    case do_transform(case erl_syntax:type(E) of
                                                          conjunction -> conjunction;
                                                          _ -> P
                                                      end, E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> erl_syntax:revert(T);
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
                                    case erl_syntax:type(E) =:= application andalso application_transform_guard(E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> T;
        _ -> Tree
    end;
do_transform(#param{} = P, Node) ->
    case erl_syntax:type(Node) of
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
    AL = [A] = erl_syntax:application_arguments(Node),
    O = erl_syntax:application_operator(Node),
    ML = erl_syntax:module_qualifier_argument(O),
    NL = erl_syntax:module_qualifier_body(O),
    copy_pos(Node,
             erl_syntax:infix_expr(copy_pos(ML, erl_syntax:infix_expr(copy_pos(ML, check_dict(ML, O, A)),
                                                                      copy_pos(ML, erl_syntax:operator('andalso')),
                                                                      copy_pos(NL, application(erlang, element, NL, NL,
                                                                                               [integer(A, 2)|AL])))),
                                   copy_pos(A, erl_syntax:operator('+')),
                                   copy_pos(A, integer(A, 0))));
application_guard(Node, otpbp_erlang, is_map) ->
    [A] = erl_syntax:application_arguments(Node),
    O = erl_syntax:application_operator(Node),
    copy_pos(Node, check_dict(erl_syntax:module_qualifier_argument(O), O, A));
application_guard(Node, otpbp_erlang, ceil) -> application_guard_ceil_floor(Node, '+');
application_guard(Node, otpbp_erlang, floor) -> application_guard_ceil_floor(Node, '-');
application_guard(_, _, _) -> false.

application_guard_ceil_floor(Node, Op) ->
    [A] = erl_syntax:application_arguments(Node),
    O = erl_syntax:application_operator(Node),
    copy_pos(Node,
             application(erlang, round, erl_syntax:module_qualifier_argument(O), erl_syntax:module_qualifier_body(O),
                         [copy_pos(A, erl_syntax:infix_expr(A, copy_pos(A, erl_syntax:operator(Op)),
                                                            copy_pos(A, erl_syntax:float(0.5))))])).

check_dict(L, O, A) ->
    application(erlang, is_record, L, erl_syntax:module_qualifier_body(O),
                [A, atom(A, dict), integer(A, tuple_size(dict:new()))]).

application_transform(#param{funs = L} = P, Node) ->
    AA = erl_syntax_lib:analyze_application(Node),
    case find(AA, L) of
        error -> false;
        {ok, {M, N}} ->
            replace_message(AA, M, N, Node, P),
            application(M, N, Node, erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node), AA)
    end.

application(M, N, Node, O, A, {_, {_, _}}) ->
    copy_pos(Node, application(M, N, erl_syntax:module_qualifier_argument(O), erl_syntax:module_qualifier_body(O), A));
application(M, N, Node, O, A, {_, _}) -> copy_pos(Node, application(M, N, O, O, A)).

-compile({inline, [application_transform/2, application/6]}).

application(M, N, ML, NL, A) ->
    erl_syntax:application(copy_pos(ML, erl_syntax:module_qualifier(atom(ML, M), atom(NL, N))), A).

implicit_fun_transform(#param{funs = L} = P, Node) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case find(F, L) of
                 error -> false;
                 {ok, {M, N}} ->
                     Q = erl_syntax:implicit_fun_name(Node),
                     {AQ, MP} = case erl_syntax:type(Q) of
                                    arity_qualifier -> {Q, erl_syntax:arity_qualifier_body(Q)};
                                    module_qualifier ->
                                        {erl_syntax:module_qualifier_body(Q), erl_syntax:module_qualifier_argument(Q)}
                                end,
                     replace_message(F, M, N, Node, P),
                     copy_pos(Node, erl_syntax:implicit_fun(atom(MP, M), atom(erl_syntax:arity_qualifier_body(AQ), N),
                                                            erl_syntax:arity_qualifier_argument(AQ)))
             end
    catch
        throw:syntax_error -> false
    end.

-compile({inline, [implicit_fun_transform/2]}).

try_expr_transform(_P, Node) ->
    case lists:mapfoldr(fun(H, A) ->
                            case try_expr_handler_transform(H) of
                                false -> {H, A};
                                NH -> {NH, true}
                            end
                        end, false, erl_syntax:try_expr_handlers(Node)) of
        {_, false} -> false;
        {Hs, _} -> copy_pos(Node, erl_syntax:try_expr(erl_syntax:try_expr_body(Node), erl_syntax:try_expr_clauses(Node),
                                                      Hs, erl_syntax:try_expr_after(Node)))
    end.

try_expr_handler_transform(Node) ->
    case erl_syntax:type(Node) =:= clause andalso try_expr_clause_patterns_transform(erl_syntax:clause_patterns(Node)) of
        {P, {true, B}} ->
            copy_pos(Node, erl_syntax:clause(P, erl_syntax:clause_guard(Node), B ++ erl_syntax:clause_body(Node)));
        _ -> false
    end.

try_expr_clause_patterns_transform(Ps) ->
    lists:mapfoldr(fun(P, {_, L} = A) ->
                       case erl_syntax:type(P) of
                           class_qualifier ->
                               B = erl_syntax:class_qualifier_body(P),
                               case erl_syntax:type(B) of
                                   module_qualifier ->
                                       M = erl_syntax:module_qualifier_body(B),
                                       case erl_syntax:type(M) of
                                           variable ->
                                               S = copy_pos(M, application(erlang, get_stacktrace, M, M, [])),
                                               C = erl_syntax:class_qualifier(erl_syntax:class_qualifier_argument(P),
                                                                              erl_syntax:module_qualifier_argument(B)),
                                               {copy_pos(P, C), {true, [copy_pos(M, erl_syntax:match_expr(M, S))|L]}};
                                           underscore ->
                                               C = erl_syntax:class_qualifier(erl_syntax:class_qualifier_argument(P),
                                                                              erl_syntax:module_qualifier_argument(B)),
                                               {copy_pos(P, C), {true, L}};
                                           _ -> {P, A}
                                       end;
                                   _ -> {P, A}
                               end;
                           _ -> {P, A}
                       end
                   end, {false, []}, Ps).

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
    io:fwrite("~ts:~b: replace ~s/~b to ~s:~s/~b~n", [F, erl_syntax:get_pos(Node), N, A, NM, NN, A]).
