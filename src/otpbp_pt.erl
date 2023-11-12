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
                              {{error, 3}, otpbp_erlang},
                              {{term_to_iovec, [1, 2]}, otpbp_erlang},
                              {{application, get_supervisor, 1}, otpbp_application},
                              {{application, set_env, [1, 2]}, otpbp_application},
                              {{argparse, [format_error, help], [1, 2]}, otpbp_argparse},
                              {{argparse, parse, [2, 3]}, otpbp_argparse},
                              {{argparse, run, 3}, otpbp_argparse},
                              {{argparse, validate, [1, 2]}, otpbp_argparse},
                              {{beam_lib, significant_chunks, 0}, otpbp_beam_lib},
                              {{beam_lib, [strip, strip_files, strip_release], 2}, otpbp_beam_lib},
                              {{binary, decode_hex, 1}, otpbp_binary},
                              {{binary, encode_hex, [1, 2]}, otpbp_binary},
                              {{c, c, 3}, otpbp_c},
                              {{c, erlangrc, 1}, otpbp_c},
                              {{c, [h, hcb, ht], [1, 2, 3]}, otpbp_c},
                              {{c, lm, 0}, otpbp_c},
                              {{c, mm, 0}, {otpbp_code, modified_modules}},
                              {{calendar, [rfc3339_to_system_time, system_time_to_rfc3339], [1, 2]}, otpbp_calendar},
                              {{calendar, [system_time_to_local_time, system_time_to_universal_time], 2},
                               otpbp_calendar},
                              {{code, [all_available, clear_cache, modified_modules, rehash], 0}, otpbp_code},
                              {{code, module_status, [0, 1]}, otpbp_code},
                              {{code, [del_paths, get_doc], 1}, otpbp_code},
                              {{code, [set_path, add_path, add_patha, add_pathz, add_paths, add_pathsa, add_pathsz], 2},
                               otpbp_code},
                              {{code, replace_path, 3}, otpbp_code},
                              {{code, is_module_native, 1}, otpbp_code},
                              {{compile, iofile, 1}, otpbp_compile},
                              {{crypto, [block_encrypt, block_decrypt], [3, 4]}, otpbp_crypto},
                              {{crypto, hash_equals, 2}, otpbp_crypto},
                              {{crypto, [cmac, hmac], [3, 4]}, otpbp_crypto},
                              {{crypto, [hmac_init, hmac_update], 2}, otpbp_crypto},
                              {{crypto, hmac_final, 1}, otpbp_crypto},
                              {{crypto, hmac_final_n, 2}, otpbp_crypto},
                              {{crypto, poly1305, 2}, otpbp_crypto},
                              {{crypto, mac, [3, 4]}, otpbp_crypto},
                              {{crypto, macN, [4, 5]}, otpbp_crypto},
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
                              {{crypto, stream_init, [2, 3]}, otpbp_crypto},
                              {{crypto, [stream_decrypt, stream_encrypt], 2}, otpbp_crypto},
                              {{ct, get_progname, 0}, otpbp_ct},
                              {{dbg, stop_clear, 0}, {dbg, stop}}, % OTP < 27
                              {{dict, foreach, 2}, otpbp_dict},
                              {{dict, take, 2}, otpbp_dict},
                              {{disk_log, accessible_logs, 0}, otpbp_disk_log},
                              {{disk_log, all, 0}, otpbp_disk_log_server},
                              {{disk_log, lclose, 1}, {disk_log, close}},
                              {{disk_log, lclose, 2}, otpbp_disk_log},
                              {{disk_log_server, all, 0}, otpbp_disk_log_server},
                              {{disksup, get_disk_info, [0, 1]}, otpbp_disksup},
                              {{disksup, parse_df, 2}, otpbp_disksup},
                              {{edlin, keymap, 0}, otpbp_edlin},
                              {{epp, scan_file, [1, 2]}, otpbp_epp},
                              {{erl_epmd, listen_port_please, 2}, otpbp_erl_epmd},
                              {{erl_epmd, port_please, [2, 3]}, otpbp_erl_epmd},
                              {{erl_error, format_fun, [1, 2]}, lib},
                              {{erl_error, [format_call, format_stacktrace], [4, 5]}, lib},
                              {{erl_error, format_exception, [6, 7]}, lib},
                              {{erl_error, format_exception, [3, 4]}, otpbp_erl_error},
                              {{erl_eval, [eval_str, extended_parse_exprs, extended_parse_term], 1}, lib},
                              {{erl_eval, subst_values_for_vars, 2}, lib},
                              {{erl_features, [all, configurable, enabled, info, long, short, used], 0},
                               otpbp_erl_features},
                              {{erl_internal, add_predefined_functions, 1}, otpbp_erl_internal},
                              {{erl_pp, legalize_vars, 1}, otpbp_erl_pp},
                              {{erlang, get_stacktrace, 0}, otpbp_erlang},
                              {{erlang, [get_cookie, set_cookie], 1}, otpbp_erlang},
                              {{erlang, iolist_to_iovec, 1}, otpbp_erlang},
                              {{erpc, reqids_new, 0}, {maps, new}},
                              {{erpc, [reqids_size, reqids_to_list], 1}, otpbp_erpc},
                              {{erpc, [check_response, receive_response, reqids_add, wait_response], 3}, otpbp_erpc},
                              {{erpc, send_request, 6}, otpbp_erpc},
                              {{error_logger, get_format_depth, 0}, otpbp_error_logger},
                              {{error_logger, limit_term, 1}, otpbp_error_logger},
                              {{erts_internal, [binary_to_integer, list_to_integer], 2}, otpbp_erts_internal},
                              {{erts_internal, map_next, 3}, otpbp_erts_internal},
                              {{ets, lookup_element, 4}, otpbp_ets},
                              {{ets, whereis, 1}, otpbp_ets},
                              {{file, del_dir_r, 1}, otpbp_file},
                              {{file, delete, 2}, otpbp_file},
                              {{filelib, ensure_path, 1}, otpbp_filelib},
                              {{filelib, find_file, [2, 3]}, otpbp_filelib},
                              {{filelib, safe_relative_path, 2}, otpbp_filelib},
                              {{filelib, find_source, [1, 2, 3]}, otpbp_filelib},
                              {{ftp, start_service, 1}, ftp_internal},
                              {{ftp, stop_service, 1}, {ftp, close}},
                              {{http_uri, scheme_defaults, 0}, otpbp_http_uri},
                              {{http_uri, decode, 1}, {uri_string, unquote}},
                              {{http_uri, encode, 1}, {uri_string, quote}},
                              {{http_uri, parse, [1, 2]}, otpbp_http_uri},
                              {{httpd, [serve, start], 1}, otpbp_httpd},
                              {{httpd_util, hexlist_to_integer, 1}, http_util},
                              {{httpd_util, [flatlength, integer_to_hexlist, strip, suffix], 1}, otpbp_httpd_util},
                              {{gb_sets, [filtermap, is_equal, map], 2}, otp_gb_sets},
                              {{gb_sets, foreach, 2}, otp_gb_sets},
                              {{gb_trees, [take, take_any], 2}, otpbp_gb_trees},
                              {{gb_trees, foreach, 2}, otpbp_gb_trees},
                              {{gen, debug_options, 2}, otpbp_gen},
                              {{gen_event, [start, start_link], 2}, otpbp_gen_event},
                              {{gen_event, start_monitor, [0, 1, 2]}, otpbp_gen_event},
                              {{geb_sctp, [connect, connect_init], 3}, otpbp_gen_sctp},
                              {{gen_server, start_monitor, [3, 4]}, otpbp_gen_server},
                              {{gen_tcp, connect, 2}, otpbp_gen_tcp},
                              {{gen_udp, connect, 2}, otpbp_gen_udp},
                              {{gen_udp, send, [3, 5]}, otpbp_gen_udp},
                              {{inet, [ensure_sockaddr, gen_tcp_module, gen_udp_module, info], 1}, otpbp_inet},
                              {{inet, [ipv4_mapped_ipv6_address, is_ip_address, is_ipv4_address, is_ipv6_address], 1},
                               otpbp_inet},
                              {{io_lib, format, 3}, otpbp_io_lib},
                              {{io_lib, fwrite, 3}, {otpbp_io_lib, format}},
                              {{io_lib, limit_term, 2}, otpbp_io_lib},
                              {{io_lib, write_atom_as_latin1, 1}, otpbp_io_lib},
                              {{lib, [flush_receive, progname], 0}, otpbp_lib},
                              {{lib, nonl, 1}, otpbp_lib},
                              {{lib, [error_message, send, sendw], 2}, otpbp_lib},
                              {{lib, [eval_str, extended_parse_exprs, extended_parse_term], 1}, erl_eval},
                              {{lib, [format_call, format_stacktrace], [4, 5]}, erl_error},
                              {{lib, format_exception, [6, 7]}, erl_error},
                              {{lib, format_fun, [1, 2]}, erl_error},
                              {{lib, subst_values_for_vars, 2}, erl_eval},
                              {{lists, enumerate, [1, 2, 3]}, otpbp_lists},
                              {{lists, search, 2}, otpbp_lists},
                              {{lists, uniq, [1, 2]}, otpbp_lists},
                              {{lists, zip, 3}, otpbp_lists},
                              {{lists, [zip3, zipwith], 4}, otpbp_lists},
                              {{lists, zipwith3, 5}, otpbp_lists},
                              {{logger, [reconfigure, timestamp], 0}, otpbp_logger},
                              {{logger, i, [0, 1]}, otpbp_logger},
                              {{logger, unset_application_level, 1}, otpbp_logger},
                              {{logger, set_application_level, 2}, otpbp_logger},
                              {{maps, [iterator, next], 1}, otpbp_maps},
                              {{maps, [filtermap, foreach, from_keys, intersect], 2}, otpbp_maps},
                              {{maps, groups_from_list, [2, 3]}, otpbp_maps},
                              {{maps, [intersect_with, merge_with], 3}, otpbp_maps},
                              {{math, [ceil, floor], 1}, otpbp_math},
                              {{math, fmod, 2}, otpbp_math},
                              {{math, tau, 0}, otpbp_math},
                              {{net_kernel, start, 2}, otpbp_net_kernel},
                              {{orddict, foreach, 2}, otpbp_orddict},
                              {{orddict, take, 2}, otpbp_orddict},
                              {{ordsets, [filtermap, is_equal, map], 2}, otpbp_ordsets},
                              {{ordsets, foreach, 2}, otpbp_orddict},
                              {{ordsets, is_empty, 1}, otpbp_ordsets},
                              {{os, cmd, 2}, otpbp_os},
                              {{os, [env, list_env_vars], 0}, otpbp_os},
                              {{peer, call,  [4, 5]}, otpbp_peer},
                              {{peer, cast,  4}, otpbp_peer},
                              {{peer, [get_state, start, stop], 1}, otpbp_peer},
                              {{peer, [random_name, start_link], [0, 1]}, otpbp_peer},
                              {{peer, send,  3}, otpbp_peer},
                              {{pg2, [start, start_link, which_groups], 0}, otpbp_pg2},
                              {{pg2, [create, delete, get_closest_pid, get_local_members, get_members], 1}, otpbp_pg2},
                              {{pg2, [join, leave], 2}, otpbp_pg2},
                              {{prim_socket, enc_sockaddr, 1}, otpbp_prim_socket},
                              {{proc_lib, init_fail, [2, 3]}, otpbp_proc_lib},
                              {{proc_lib, start_monitor, [3, 4, 5]}, otpbp_proc_lib},
                              {{proplists, from_map, 1}, {maps, to_list}}, % OTP 24.0
                              {{proplists, to_map, [1, 2]}, otpbp_proplists},
                              {{pubkey_os_cacerts, [clear, get], 0}, otpbp_pubkey_os_cacerts},
                              {{pubkey_os_cacerts, load, [0, 1]}, otpbp_pubkey_os_cacerts},
                              {{pubkey_os_cacerts, format_error, 2}, otpbp_pubkey_os_cacerts},
                              {{public_key, ssh_decode, 2}, {ssh_file, decode}},
                              {{public_key, ssh_encode, 2}, {ssh_file, encode}},
                              {{public_key, ssh_hostkey_fingerprint, [1, 2]}, {ssh, hostkey_fingerprint}},
                              {{queue, [all, any, delete, delete_r, delete_with, delete_with_r, filtermap, foreach], 2},
                               otpbp_queue},
                              {{queue, fold, 3}, otpbp_queue},
                              {{rand, bytes, 1}, otpbp_rand},
                              {{rand, bytes_s, 2}, otpbp_rand},
                              {{rand, jump, [1, 2]}, otpbp_rand},
                              {{rand, [exsp_jump, exsp_next], 1}, otpbp_rand},
                              {{rand, [mwc59, mwc59_float, mwc59_value, mwc59_value32], 1}, otpbp_rand},
                              {{rand, mwc59_seed, [0, 1]}, otpbp_rand},
                              {{rand, normal, 2}, otpbp_rand},
                              {{rand, normal_s, 3}, otpbp_rand},
                              {{rand, splitmix64_next, 1}, otpbp_rand},
                              {{rand, uniform_real, 0}, otpbp_rand},
                              {{rand, uniform_real_s, 1}, otpbp_rand},
                              {{re, version, 0}, otpbp_re},
                              {{scheduler, [get_sample, get_sample_all, sample, sample_all], 0}, otpbp_scheduler},
                              {{scheduler, utilization, [1, 2]}, otpbp_scheduler},
                              {{sets, [filtermap, from_list, is_equal, map], 2}, otpbp_sets},
                              {{sets, [is_empty, new], 1}, otpbp_sets},
                              {{sets, foreach, 2}, otpbp_sets},
                              {{shell_docs, [render, render_callback], 5}, otpbp_shell_docs},
                              {{shell_docs, supported_tags, 0}, otpbp_shell_docs},
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
                              {{ssl, [handshake, ssl_accept], [1, 2, 3]}, otpbp_ssl},
                              {{string, [casefold, chomp, is_empty, length, lowercase, next_codepoint,
                                         next_grapheme, reverse, titlecase, to_graphemes, trim, uppercase], 1},
                               otpbp_string},
                              {{string, [equal, find, lexemes, pad, prefix, slice, split, take, trim], 2}, otpbp_string},
                              {{string, [equal, find, nth_lexeme, pad, replace, slice, split, take, trim], 3},
                               otpbp_string},
                              {{string, [equal, pad, replace, take], 4}, otpbp_string},
                              {{supervisor, check_childspecs, 2}, otpbp_supervisor},
                              {{sys, get_log, 1}, otpbp_sys},
                              {{timer, [apply_after, apply_interval], [2, 3]}, otpbp_timer},
                              {{timer, tc, 4}, otpbp_timer},
                              {{unicode,
                                [characters_to_nfc_binary, characters_to_nfc_list,
                                 characters_to_nfd_binary, characters_to_nfd_list,
                                 characters_to_nfkc_binary, characters_to_nfkc_list,
                                 characters_to_nfkd_binary, characters_to_nfkd_list],
                                1},
                               otpbp_unicode},
                              {{unicode_util, [spec_version, whitespace], 0}, otpbp_unicode_util},
                              {{unicode_util, [casefold, lowercase, titlecase, uppercase], 1}, otpbp_unicode_util},
                              {{unicode_util, [cp, gc, get_case, is_whitespace, lookup], 1}, otpbp_unicode_util},
                              {{unicode_util, [nfc, nfd, nfkc, nfkd], 1}, otpbp_unicode_util},
                              {{uri_string, allowed_characters, 0}, otpbp_uri_string},
                              {{uri_string, [is_host, is_path], 1}, otpbp_uri_string},
                              {{uri_string, [dissect_query, parse, percent_decode, recompose, unquote], 1},
                               otpbp_uri_string},
                              {{uri_string, [compose_query, normalize, quote], [1, 2]}, otpbp_uri_string},
                              {{uri_string, transcode, 2}, otpbp_uri_string},
                              {{uri_string, resolve, [2, 3]}, otpbp_uri_string},
                              {{user, interfaces, 1}, otpbp_user},
                              {{zlib, [adler32, crc32], [2, 3]}, otpbp_zlib},
                              {{zlib, [adler32_combine, crc32_combine], 4}, otpbp_zlib},
                              {{zlib, inflate, 3}, otpbp_zlib},
                              {{zlib, inflateGetDictionary, 1}, otpbp_zlib},
                              {{zlib, inflateChunk, [1, 2]}, otpbp_zlib},
                              {{zlib, getBufSize, 1}, otpbp_zlib},
                              {{zlib, [safeInflate, setBufSize], 2}, otpbp_zlib},
                              {{zlib, [compress, gzip, zip], 2}, otpbp_zlib}]).
-define(TRANSFORM_BEHAVIOURS, []).

-record(param, {options = [] :: list(),
                verbose = false :: boolean(),
                otp_release = otp_release() :: 19..26,
                erts_version = erts_version() :: [non_neg_integer(),...],
                funs = #{} :: #{{module(), {atom(), arity()}} => {module(), atom()}},
                behaviours = maps:from_list(?TRANSFORM_BEHAVIOURS) :: #{module() => module()},
                apply = lists:foldr(fun(FA, A) -> [FA, {erlang, FA}|A] end,
                                    [{erlang, {hibernate, 3}},
                                     {timer, {tc, 3}}, {timer, {tc, 4}},
                                     {fprof, {apply, 3}}, {fprof, {apply, 4}}],
                                    [{apply, 3}, {spawn, 3}, {spawn_link, 3}, {spawn_monitor, 3}, {spawn_request, 3},
                                     {spawn_opt, 4}, {spawn_request, 4}]) ::
                            [{atom(), arity()}|{atom(), {atom(), arity()}}],
                file = "" :: string()}).

parse_transform(Forms, Options) ->
    case transform_list() of
        TL when map_size(TL) =/= 0 ->
            try erl_syntax_lib:analyze_forms(Forms) of
                 AF ->
                     {NF, _} = lists:mapfoldl(fun transform/2,
                                              #param{options = Options,
                                                     verbose = proplists:get_bool(verbose, Options),
                                                     funs = get_funs(AF, Options, TL)},
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

-compile({inline, [get_funs/3]}).
get_funs(AF, Options, TL) ->
    lists:foldl(fun({M, Fs}, IA) ->
                    lists:foldl(fun(FA, IAM) ->
                                    case maps:find({M, FA}, TL) of
                                        {ok, V} -> IAM#{FA => V};
                                        _ -> IAM
                                    end
                                 end, IA, Fs)
                end,
                maps:without(get_no_auto_import(AF, Options), TL),
                proplists:get_value(imports, AF, [])).

-compile({inline, [get_no_auto_import/2]}).
get_no_auto_import(AF, Options) ->
    lists:usort(proplists:append_values(no_auto_import,
                                        proplists:append_values(compile,
                                                                proplists:get_value(attributes, AF, [])) ++ Options)).

transform(Tree, P) ->
    case type(Tree) of
        function -> {transform_function(Tree, P), P};
        attribute -> transform_attribute(Tree, P);
        _ -> {Tree, P}
    end.

-compile({inline, [transform_function/2]}).
transform_function(Tree, P) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case function_transform(E, P) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Tree) of
        {T, true} -> erl_syntax:revert(T);
        _ -> Tree
    end.

-compile({inline, [transform_attribute/2]}).
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

-compile({inline, [check_behaviour/1]}).
check_behaviour(B) ->
    try B:module_info(exports) of
        Exports -> lists:member({behaviour_info, 1}, Exports)
    catch
        _:_ -> false
    end.

add_func(F, MF, D, I) -> lists:foldl(fun(A, Acc) -> add_func(setelement(I, F, A), MF, Acc) end, D, element(I, F)).

add_func(F, MF, D) when is_list(element(tuple_size(F), F)) -> add_func(F, MF, D, tuple_size(F));
add_func(F, MF, D) when is_list(element(tuple_size(F) - 1, F)) -> add_func(F, MF, D, tuple_size(F) - 1);
add_func(FA, MF, D) ->
    case check_func(FA) orelse FA of
        true -> D;
        {M, F, A} -> store_func({M, {F, A}}, MF, D);
        {_, _} -> store_func({erlang, FA}, MF, store_func(FA, MF, D))
    end.

check_func({M, F, A}) -> check_func(M, F, A);
check_func({F, A}) -> check_func(erlang, F, A).

check_func(M, F, A) ->
    erlang:is_builtin(M, F, A) orelse try M:module_info(exports) of
                                          Exports -> lists:member({F, A}, Exports)
                                      catch
                                          _:_ -> false
                                      end.

store_func(F, {_, _} = MF, D) -> D#{F => MF};
store_func({_, {F, _}} = MFA, M, D) -> store_func(MFA, {M, F}, D);
store_func({F, _} = FA, M, D) -> store_func(FA, {M, F}, D).

-compile({inline, [transform_list/0]}).
transform_list() -> lists:foldl(fun({F, D}, Acc) -> add_func(F, D, Acc) end, #{}, ?TRANSFORM_FUNCTIONS).

function_transform(Node, #param{} = P) ->
    case type(Node) of
        application -> application_transform(Node, P);
        implicit_fun -> implicit_fun_transform(Node, P);
        conjunction -> conjunction_transform(Node);
        try_expr -> try_expr_transform(Node);
        _ -> false
    end.

-compile({inline, [conjunction_transform/1]}).
conjunction_transform(Node) ->
    case erl_syntax_lib:mapfold(fun(E, F) ->
                                    case type(E) =:= application andalso application_transform_guard(E) of
                                        false -> {E, F};
                                        N -> {N, true}
                                    end
                                end, false, Node) of
        {T, true} -> T;
        _ -> Node
    end.

-compile({inline, [application_transform_guard/1]}).
application_transform_guard(Node) ->
    case erl_syntax_lib:analyze_application(Node) of
        {M, {N, _}} -> application_guard(Node, M, N);
        _ -> false
    end.

-compile({inline, [application_guard/3]}).
application_guard(Node, otpbp_erlang, ceil) -> application_guard_ceil_floor(Node, '+');
application_guard(Node, otpbp_erlang, floor) -> application_guard_ceil_floor(Node, '-');
application_guard(_, _, _) -> false.

application_guard_ceil_floor(Node, Op) ->
    [A] = erl_syntax:application_arguments(Node),
    cp(Node,
       application(remote, erl_syntax:application_operator(Node),
                   [cp(A, erl_syntax:infix_expr(A, cp(A, erl_syntax:operator(Op)), cp(A, erl_syntax:float(0.5))))],
                   erlang, round)).

application_transform(Node, #param{funs = FL} = P) ->
    A = erl_syntax_lib:analyze_application(Node),
    case apply_transform(Node, P, A) of
        false -> application_transform(Node, P, A, FL);
        NodeApply ->
            case application_transform(NodeApply, P, A, FL) of
                false -> NodeApply;
                NodeApplication -> NodeApplication
            end
    end.

-compile({inline, [application_transform/2]}).
application_transform(Node, P, A, L) ->
    case L of
        #{A := {M, N}} ->
            replace_message(Node, P, A, M, N),
            cp(Node,
               application(case A of
                               {_, {_, _}} -> remote;
                               {_, _} -> local
                           end,
                           erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node), M, N));
        #{} -> false
    end.

-compile({inline, [apply_transform/3]}).
apply_transform(Node, #param{apply = AL} = P, A) ->
    lists:member(A, AL) andalso
        case erl_syntax:application_arguments(Node) of
            [MT, FT|[AT|_] = T] ->
                case type(MT) =:= atom andalso type(FT) =:= atom andalso erl_syntax:is_list_skeleton(AT) of
                    true ->
                        AA = {erl_syntax:atom_value(MT), {erl_syntax:atom_value(FT), erl_syntax:list_length(AT)}},
                        case P#param.funs of
                            #{AA := {M, F}} ->
                                replace_message(Node, P, AA, M, F),
                                erl_syntax:application(erl_syntax:application_operator(Node),
                                                       [atom(MT, M), atom(FT, F)|T]);
                            #{} -> false
                        end;
                    _false -> false
                end;
            _ -> false
        end.

application(local, O, As, M, N) -> application_(O, O, As, M, N);
application(remote, O, As, M, N) -> application_(mqa(O), mqb(O), As, M, N).

application_(O1, O2, As, M, N) -> application_(atom(O1, M), atom(O2, N), As).

-compile({inline, [application_/3]}).
application_(M, N, As) -> erl_syntax:application(cp(M, erl_syntax:module_qualifier(M, N)), As).

-compile({inline, [implicit_fun_transform/2]}).
implicit_fun_transform(Node, #param{funs = L} = P) ->
    try erl_syntax_lib:analyze_implicit_fun(Node) of
        F -> case L of
                 #{F := {M, N}} ->
                     Q = erl_syntax:implicit_fun_name(Node),
                     {AQ, MP} = case type(Q) of
                                    arity_qualifier -> {Q, erl_syntax:arity_qualifier_body(Q)};
                                    module_qualifier -> {mqb(Q), mqa(Q)}
                                end,
                     replace_message(Node, P, F, M, N),
                     cp(Node,
                        erl_syntax:implicit_fun(atom(MP, M), atom(erl_syntax:arity_qualifier_body(AQ), N),
                                                erl_syntax:arity_qualifier_argument(AQ)));
                 #{} -> false
             end
    catch
        throw:syntax_error -> false
    end.

-compile({inline, [try_expr_transform/1]}).
try_expr_transform(Node) ->
    case lists:mapfoldr(fun(H, A) ->
                            case try_expr_handler_transform(H) of
                                false -> {H, A};
                                NH -> {NH, true}
                            end
                        end, false, erl_syntax:try_expr_handlers(Node)) of
        {_, false} -> false;
        {Hs, _} ->
            cp(Node,
               erl_syntax:try_expr(erl_syntax:try_expr_body(Node), erl_syntax:try_expr_clauses(Node),
                                   Hs, erl_syntax:try_expr_after(Node)))
    end.

-compile({inline, [try_expr_handler_transform/1]}).
try_expr_handler_transform(Node) ->
    case type(Node) =:= clause andalso try_expr_clause_patterns_transform(erl_syntax:clause_patterns(Node)) of
        {P, {true, B}} ->
            cp(Node, erl_syntax:clause(P, erl_syntax:clause_guard(Node), B ++ erl_syntax:clause_body(Node)));
        _ -> false
    end.

-compile({inline, [try_expr_clause_patterns_transform/1]}).
try_expr_clause_patterns_transform(Ps) ->
    lists:mapfoldr(fun(P, {_, L} = A) ->
                       case type(P) of
                           class_qualifier ->
                               B = erl_syntax:class_qualifier_body(P),
                               case type(B) of
                                   module_qualifier ->
                                       M = mqb(B),
                                       case type(M) of
                                           variable -> {class_qualifier(P, B), {true, match_expr_list(M, L)}};
                                           underscore -> {class_qualifier(P, B), {true, L}};
                                           _ -> {P, A}
                                       end;
                                   match_expr ->
                                       E = erl_syntax:match_expr_body(B),
                                       case type(E) of
                                           module_qualifier ->
                                               M = mqb(E),
                                               case type(M) of
                                                   variable ->
                                                       {class_qualifier_match(P, B, E), {true, match_expr_list(M, L)}};
                                                   underscore -> {class_qualifier_match(P, B, E), {true, L}};
                                                   _ -> {P, A}
                                               end;
                                           _ -> {P, A}
                                       end;
                                   _ -> {P, A}
                               end;
                           module_qualifier ->
                               M = mqb(P),
                               case type(M) of
                                   variable -> {class_qualifier(P), {true, match_expr_list(M, L)}};
                                   underscore -> {class_qualifier(P), {true, L}};
                                   _ -> {P, A}
                               end;
                           match_expr ->
                               E = erl_syntax:match_expr_body(P),
                               case type(E) of
                                   module_qualifier ->
                                       M = mqb(E),
                                       case type(M) of
                                           variable -> {class_qualifier_match(P, E), {true, match_expr_list(M, L)}};
                                           underscore -> {class_qualifier_match(P, E), {true, L}};
                                           _ -> {P, A}
                                       end;
                                   _ -> {P, A}
                               end;
                           _ -> {P, A}
                       end
                   end, {false, []}, Ps).

class_qualifier(P) -> class_qualifier(P, P, erl_syntax:atom('throw')).

class_qualifier(P, B) -> class_qualifier(P, B, erl_syntax:class_qualifier_argument(P)).

class_qualifier(P, B, C) -> cp(P, erl_syntax:class_qualifier(C, mqa(B))).

class_qualifier_match(B, E) -> class_qualifier_match(B, B, E, cp(B, erl_syntax:atom('throw'))).

class_qualifier_match(P, B, E) -> class_qualifier_match(P, B, E, erl_syntax:class_qualifier_argument(P)).

class_qualifier_match(P, B, E, C) ->
   cp(P, erl_syntax:class_qualifier(C, cp(B, erl_syntax:match_expr(erl_syntax:match_expr_pattern(B), mqa(E))))).

match_expr_list(M, L) -> [cp(M, erl_syntax:match_expr(M, cp(M, application(local, M, [], erlang, get_stacktrace))))|L].

atom(P, A) when is_tuple(P), is_atom(A) -> cp(P, erl_syntax:atom(A)).

-compile({inline, [otp_release/0]}).
otp_release() -> list_to_integer(erlang:system_info(otp_release)).

-compile({inline, [erts_version/0]}).
erts_version() -> lists:map(fun list_to_integer/1, string:tokens(erlang:system_info(version), ".")).

replace_message(_Node, #param{verbose = false}, _F, _NM, _NN) -> ok;
replace_message(Node, #param{file = File}, F, NM, NN) -> replace_message_(erl_syntax:get_pos(Node), File, F, NM, NN).

-compile({inline, [replace_message_/5]}).
replace_message_(P, F, {M, {N, A}}, NM, NN) -> replace_message_(P, F, lists:concat([M, ":", N]), NM, NN, A);
replace_message_(P, F, {N, A}, NM, NN) -> replace_message_(P, F, N, NM, NN, A).

replace_message_(P, F, N, NM, NN, A) -> io:fwrite("~ts:~p: replace ~ts/~B to ~s:~ts/~B~n", [F, P, N, A, NM, NN, A]).

-compile({inline, [cp/2]}).
cp(S, T) -> erl_syntax:copy_pos(S, T).

-compile({inline, [type/1]}).
type(N) -> erl_syntax:type(N).

-compile({inline, [mqa/1]}).
mqa(N) -> erl_syntax:module_qualifier_argument(N).

-compile({inline, [mqb/1]}).
mqb(N) -> erl_syntax:module_qualifier_body(N).
