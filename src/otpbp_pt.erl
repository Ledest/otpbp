%%% Copyright 2015-2019 Oleksandr Chumachenko <ledest@gmail.com>
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

-define(TRANSFORM_FUNCTIONS, [{{is_map_key, 2}, {maps, is_key}},
                              {{map_get, 2}, {maps, get}},
                              {{[ceil, floor], 1}, otpbp_erlang},
                              {{[atom_to_binary, binary_to_atom, binary_to_existing_atom], 1}, otpbp_erlang},
                              {{application, set_env, [1, 2]}, otpbp_application},
                              {{binary, [decode_hex, encode_hex], 1}, otpbp_binary},
                              {{calendar, [system_time_to_local_time, system_time_to_universal_time], 2},
                               otpbp_calendar},
                              {{code, [all_available, modified_modules], 0}, otpbp_code},
                              {{code, rehash, 0}, otpbp_code},
                              {{code, module_status, 1}, otpbp_code},
                              {{code, is_module_native, 1}, otpbp_code},
                              {{compile, env_compiler_options, 0}, otpbp_compile},
                              {{compile, iofile, 1}, otpbp_compile},
                              {{crypto, [block_encrypt, block_decrypt], [3, 4]}, otpbp_crypto},
                              {{crypto, hash_equals, 2}, otpbp_crypto},
                              {{crypto, [cmac, hmac], [3, 4]}, otpbp_crypto},
                              {{crypto, [hmac_init, hmac_update], 2}, otpbp_crypto},
                              {{crypto, hmac_final, 1}, otpbp_crypto},
                              {{crypto, hmac_final_n, 2}, otpbp_crypto},
                              {{crypto, poly1305, 2}, otpbp_crypto},
                              {{crypto, mac, [3, 4]}, otpbp_crypto},
                              {{crypto, [dss_sign, rsa_sign], [2, 3]}, otpbp_crypto},
                              {{crypto, [dss_verify, rsa_verify], [3, 4]}, otpbp_crypto},
                              {{crypto, [md4_init, md5_init, sha_init], 0}, otpbp_crypto},
                              {{crypto, [md4, md5, sha], 1}, otpbp_crypto},
                              {{crypto, [md4_final, md5_final, sha_final], 2}, {crypto, hash_final}},
                              {{crypto, [md4_update, md5_update, sha_update], 2}, {crypto, hash_update}},
                              {{crypto, rand_bytes, 1}, {crypto, strong_rand_bytes}},
                              {{crypto, supports, 1}, otpbp_crypto},
                              {{crypto, [sha224, sha256, sha384, sha512], 1}, otpbp_crypto},
                              {{crypto, [sha224_final, sha256_final, sha384_final, sha512_final], 1},
                               {crypto, hash_final}},
                              {{crypto, [sha224_init, sha256_init, sha384_init, sha512_init], 0}, otpbp_crypto},
                              {{crypto, [sha224_mac, sha256_mac, sha384_mac, sha512_mac], 2}, otpbp_crypto},
                              {{crypto, [sha224_update, sha256_update, sha384_update, sha512_update], 2},
                               {crypto, hash_update}},
                              {{ct, get_progname, 0}, otpbp_ct},
                              {{dict, take, 2}, otpbp_dict},
                              {{disk_log, accessible_logs, 0}, otpbp_disk_log},
                              {{disk_log, lclose, 1}, {disk_log, close}},
                              {{disk_log, lclose, 2}, otpbp_disk_log},
                              {{erl_error, format_fun, [1, 2]}, lib},
                              {{erl_error, [format_call, format_stacktrace], [4, 5]}, lib},
                              {{erl_error, format_exception, [6, 7]}, lib},
                              {{erl_error, format_exception, [3, 4]}, otpbp_erl_error},
                              {{erl_eval, [eval_str, extended_parse_exprs, extended_parse_term], 1}, lib},
                              {{erl_eval, subst_values_for_vars, 2}, lib},
                              {{erlang, get_stacktrace, 0}, otpbp_erlang},
                              {{erlang, [get_cookie, set_cookie], 1}, otpbp_erlang},
                              {{error_logger, get_format_depth, 0}, otpbp_error_logger},
                              {{error_logger, limit_term, 1}, otpbp_error_logger},
                              {{erts_internal, map_next, 3}, otpbp_erts_internal},
                              {{file, del_dir_r, 1}, otpbp_file},
                              {{file, delete, 2}, otpbp_file},
                              {{filelib, ensure_path, 1}, otpbp_filelib},
                              {{filelib, safe_relative_path, 2}, otpbp_filelib},
                              {{filename, safe_relative_path, 1}, otpbp_filename},
                              {{ftp, start_service, 1}, otpbp_ftp},
                              {{ftp, stop_service, 1}, {ftp, close}},
                              {{http_uri, scheme_defaults, 0}, otpbp_http_uri},
                              {{http_uri, [decode, encode], 1}, otpbp_http_uri},
                              {{http_uri, parse, [1, 2]}, otpbp_http_uri},
                              {{httpd_util, hexlist_to_integer, 1}, http_util},
                              {{httpd_util, [flatlength, integer_to_hexlist, strip, suffix], 1}, otpbp_httpd_util},
                              {{gb_trees, [take, take_any], 2}, otpbp_gb_trees},
                              {{gen, get_parent, 0}, otpbp_gen},
                              {{gen, [debug_options, get_proc_name, name, unregister_name], 1}, otpbp_gen},
                              {{gen, debug_options, 2}, otpbp_gen},
                              {{gen_statem, call, [2, 3]}, otpbp_gen_statem},
                              {{gen_statem, cast, 2}, otpbp_gen_statem},
                              {{gen_statem, enter_loop, [4, 5, 6]}, otpbp_gen_statem},
                              {{gen_statem, reply, [1, 2]}, otpbp_gen_statem},
                              {{gen_statem, [start, start_link], [3, 4]}, otpbp_gen_statem},
                              {{gen_statem, stop, [1, 3]}, otpbp_gen_statem},
                              {{gen_statem, init_it, 6}, otpbp_gen_statem},
                              {{gen_statem, system_continue, 3}, otpbp_gen_statem},
                              {{gen_statem, system_get_state, 1}, otpbp_gen_statem},
                              {{gen_statem, [system_code_change, system_terminate], 4}, otpbp_gen_statem},
                              {{gen_statem, [format_status, system_replace_state], 2}, otpbp_gen_statem},
                              {{gen_statem, wakeup_from_hibernate, 3}, otpbp_gen_statem},
                              {{inet, [info, ipv4_mapped_ipv6_address], 1}, otpbp_inet},
                              {{io_lib, limit_term, 2}, otpbp_io_lib},
                              {{lib, [flush_receive, progname], 0}, otpbp_lib},
                              {{lib, nonl, 1}, otpbp_lib},
                              {{lib, [error_message, send, sendw], 2}, otpbp_lib},
                              {{lib, [eval_str, extended_parse_exprs, extended_parse_term], 1}, erl_eval},
                              {{lib, [format_call, format_stacktrace], [4, 5]}, erl_error},
                              {{lib, format_exception, [6, 7]}, erl_error},
                              {{lib, format_fun, [1, 2]}, erl_error},
                              {{lib, subst_values_for_vars, 2}, erl_eval},
                              {{lists, enumerate, [1, 2]}, otpbp_lists},
                              {{lists, [join, search], 2}, otpbp_lists},
                              {{lists, uniq, [1, 2]}, otpbp_lists},
                              {{maps, [iterator, next], 1}, otpbp_maps},
                              {{maps, [filtermap, foreach, from_keys, intersect, take], 2}, otpbp_maps},
                              {{maps, groups_from_list, [2, 3]}, otpbp_maps},
                              {{maps, [intersect_with, merge_with], 3}, otpbp_maps},
                              {{maps, update_with, [3, 4]}, otpbp_maps},
                              {{math, [ceil, floor], 1}, otpbp_math},
                              {{orddict, take, 2}, otpbp_orddict},
                              {{ordsets, is_empty, 1}, otpbp_ordsets},
                              {{os, cmd, 2}, otpbp_os},
                              {{os, [env, list_env_vars], 0}, otpbp_os},
                              {{pg2, [start, start_link, which_groups], 0}, otpbp_pg2},
                              {{pg2, [create, delete, get_closest_pid, get_local_members, get_members], 1}, otpbp_pg2},
                              {{pg2, [join, leave], 2}, otpbp_pg2},
                              {{proplists, from_map, 1}, {maps, to_list}}, % OTP 24.0
                              {{proplists, to_map, [1, 2]}, otpbp_proplists},
                              {{public_key, ssh_decode, 2}, {ssh_file, decode}},
                              {{public_key, ssh_encode, 2}, {ssh_file, encode}},
                              {{public_key, ssh_hostkey_fingerprint, [1, 2]}, {ssh, hostkey_fingerprint}},
                              {{queue, [all, any, delete, delete_r, delete_with, delete_with_r, filtermap, foreach], 2},
                               otpbp_queue},
                              {{queue, fold, 3}, otpbp_queue},
                              {{rand, bytes, 1}, otpbp_rand},
                              {{rand, bytes_s, 2}, otpbp_rand},
                              {{sets, is_empty, 1}, otpbp_sets},
                              {{snmpa, old_info_format, 1}, otpbp_snmpa},
                              {{snmpm, async_get, [3, 4, 5, 6]}, {snmpm, async_get2}},
                              {{snmpm, async_get_bulk, [5, 6, 7, 8]}, {snmpm, async_get_bulk2}},
                              {{snmpm, async_get_next, [3, 4, 5, 6]}, {snmpm, async_get_next2}},
                              {{snmpm, async_set, [3, 4, 5, 6]}, {snmpm, async_set2}},
                              {{snmpm, sync_get, [3, 4, 5, 6]}, {snmpm, sync_get2}},
                              {{snmpm, sync_get_bulk, [5, 6, 7, 8]}, {snmpm, sync_get_bulk2}},
                              {{snmpm, sync_get_next, [3, 4, 5, 6]}, {snmpm, sync_get_next2}},
                              {{snmpm, sync_set, [3, 4, 5, 6]}, {snmpm, sync_set2}},
                              {{ssl, cipher_suites, [0, 1]}, otpbp_ssl},
                              {{ssl, ssl_accept, [1, 2, 3]}, otpbp_ssl},
                              {{string, [casefold, chomp, is_empty, length, lowercase, next_codepoint,
                                         next_grapheme, reverse, titlecase, to_graphemes, trim, uppercase], 1},
                               otpbp_string},
                              {{string, [equal, find, lexemes, pad, prefix, slice, split, take, trim], 2}, otpbp_string},
                              {{string, [equal, find, nth_lexeme, pad, replace, slice, split, take, trim], 3},
                               otpbp_string},
                              {{string, [equal, pad, replace, take], 4}, otpbp_string},
                              {{supervisor, get_callback_module, 1}, otpbp_supervisor},
                              {{supervisor, format_status, 2}, otpbp_supervisor},
                              {{unicode_util, [spec_version, whitespace], 0}, otpbp_unicode_util},
                              {{unicode_util, [casefold, cp, gc, get_case, is_whitespace, lookup, lowercase, nfc, nfd,
                                               nfkc, nfkd, titlecase,uppercase], 1},
                               otpbp_unicode_util},
                              {{uri_string, [compose_query, normalize], [1, 2]}, otpbp_uri_string},
                              {{uri_string, [dissect_query, is_host, is_path, parse, recompose], 1}, otpbp_uri_string},
                              {{uri_string, transcode, 2}, otpbp_uri_string},
                              {{zlib, [adler32, crc32], [2, 3]}, otpbp_zlib},
                              {{zlib, [adler32_combine, crc32_combine], 4}, otpbp_zlib},
                              {{zlib, inflateChunk, [1, 2]}, otpbp_zlib},
                              {{zlib, getBufSize, 1}, otpbp_zlib},
                              {{zlib, setBufSize, 2}, otpbp_zlib},
                              {{zlib, [compress, gzip, zip], 2}, otpbp_zlib}]).
-define(TRANSFORM_BEHAVIOURS, [{gen_statem, otpbp_gen_statem}]).

-import(erl_syntax, [copy_pos/2]).
-import(lists, [foldl/3]).

-record(param, {options = [] :: list(),
                verbose = false :: boolean(),
                otp_release = otp_release() :: 18..24,
                erts_version = erts_version() :: [non_neg_integer(),...],
                funs = #{} :: #{{module(), {atom(), arity()}} => {module(), atom()}},
                behaviours = maps:from_list(?TRANSFORM_BEHAVIOURS) :: #{module() => module()},
                file = "" :: string()}).

parse_transform(Forms, Options) ->
    case transform_list() of
        TL when map_size(TL) =/= 0 ->
            try erl_syntax_lib:analyze_forms(Forms) of
                 AF ->
                     {NF, _} = lists:mapfoldl(fun(Tree, P) -> transform(Tree, P, erl_syntax:type(Tree)) end,
                                              #param{options = Options,
                                                     verbose = proplists:get_bool(verbose, Options),
                                                     funs = foldl(fun({M, Fs}, IA) ->
                                                                      foldl(fun(FA, IAM) ->
                                                                                case maps:find({M, FA}, TL) of
                                                                                    {ok, V} -> IAM#{FA => V};
                                                                                    _ -> IAM
                                                                                end
                                                                             end, IA, Fs)
                                                                  end,
                                                                  maps:without(get_no_auto_import(AF), TL),
                                                                  proplists:get_value(imports, AF, []))},
                                              Forms),
                     NF
            catch
                C:E ->
                    io:fwrite(standard_error,
                              ?MODULE_STRING ": error erl_syntax_lib:analyze_forms/1 {~p:~p}, see below.~n",
                              [C, E]),
                    Forms
            end;
        _ -> Forms
    end.

get_no_auto_import(AF) ->
    proplists:append_values(no_auto_import, proplists:append_values(compile, proplists:get_value(attributes, AF, []))).

-compile({inline, [get_no_auto_import/1, transform/3]}).

transform(Tree, P, function) -> {transform_function(Tree, P), P};
transform(Tree, P, attribute) -> transform_attribute(Tree, P);
transform(Tree, P, _) -> {Tree, P}.

transform_function(Tree, P) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case transform(case erl_syntax:type(E) of
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
        {file, {F, _}} -> {Tree, P#param{file = F}};
        {behaviour, _} -> {transform_behaviour(Tree, P), P};
        {behavior, _} -> {transform_behaviour(setelement(3, Tree, behaviour), P), P};
        _ -> {Tree, P}
    end.

transform_behaviour({_, _, _, B} = Tree, #param{behaviours = Bs}) ->
    case Bs of
        #{B := M} ->
            case check_behaviour(B) of
                true -> Tree;
                _false -> setelement(4, Tree, M)
            end;
        _ -> Tree
    end;
transform_behaviour(Tree, _) -> Tree.

check_behaviour(B) ->
    try B:module_info(exports) of
        Exports -> lists:member({behaviour_info, 1}, Exports)
    catch
        _:_ -> false
    end.

-compile({inline, [transform_function/2, transform_attribute/2, check_behaviour/1, transform_behaviour/2]}).

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

store_func(F, {_, _} = MF, D) -> D#{F => MF};
store_func({_, {F, _}} = MFA, M, D) -> store_func(MFA, {M, F}, D);
store_func({F, _} = FA, M, D) -> store_func(FA, {M, F}, D).

transform_list() -> foldl(fun({F, D}, Acc) -> add_func(F, D, Acc) end, #{}, ?TRANSFORM_FUNCTIONS).
-compile({inline, [transform_list/0]}).

transform(conjunction, Tree) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case erl_syntax:type(E) =:= application andalso application_transform_guard(E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> T;
        _ -> Tree
    end;
transform(#param{} = P, Node) ->
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

application_transform(#param{funs = L} = P, Node) ->
    A = erl_syntax_lib:analyze_application(Node),
    case L of
        #{A := {M, N}} ->
            replace_message(A, M, N, Node, P),
            application(M, N, Node, A);
        #{} -> false
    end.

application(M, N, Node, A) ->
    application(M, N, Node, erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node), A).

application(M, N, Node, O, A, {_, {_, _}}) ->
    copy_pos(Node, application(M, N, erl_syntax:module_qualifier_argument(O), erl_syntax:module_qualifier_body(O), A));
application(M, N, Node, O, A, {_, _}) -> copy_pos(Node, application(M, N, O, O, A)).

-compile({inline, [application_transform/2, application/4, application/6]}).

application(M, N, ML, NL, A) ->
    erl_syntax:application(copy_pos(ML, erl_syntax:module_qualifier(atom(ML, M), atom(NL, N))), A).

implicit_fun_transform(#param{funs = L} = P, Node) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case L of
                 #{F := {M, N}} ->
                     Q = erl_syntax:implicit_fun_name(Node),
                     {AQ, MP} = case erl_syntax:type(Q) of
                                    arity_qualifier -> {Q, erl_syntax:arity_qualifier_body(Q)};
                                    module_qualifier ->
                                        {erl_syntax:module_qualifier_body(Q), erl_syntax:module_qualifier_argument(Q)}
                                end,
                     replace_message(F, M, N, Node, P),
                     copy_pos(Node, erl_syntax:implicit_fun(atom(MP, M), atom(erl_syntax:arity_qualifier_body(AQ), N),
                                                            erl_syntax:arity_qualifier_argument(AQ)));
                 #{} -> false
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

otp_release() -> list_to_integer(erlang:system_info(otp_release)).

erts_version() -> lists:map(fun list_to_integer/1, string:tokens(erlang:system_info(version), ".")).

-compile({inline, [otp_release/0, erts_version/0]}).

replace_message(_F, _NM, _NN, _Node, #param{verbose = false}) -> ok;
replace_message(F, NM, NN, Node, #param{file = File}) -> do_replace_message(F, NM, NN, Node, File).

do_replace_message({M, {N, A}}, NM, NN, Node, F) -> do_replace_message({lists:concat([M, ":", N]), A}, NM, NN, Node, F);
do_replace_message({N, A}, NM, NN, Node, F) ->
    io:fwrite("~ts:~b: replace ~s/~b to ~s:~s/~b~n", [F, get_pos(Node), N, A, NM, NN, A]).

get_pos(Node) ->
    case erl_syntax:get_pos(Node) of
        {L, _} -> L;
        L -> L
    end.
