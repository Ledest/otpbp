-module(otpbp_pt).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    transform_list(),
    case get() of
        [] -> Forms;
        _ -> parse_trans:plain_transform(fun do_transform/1, Forms)
    end.

-define(TRANSFORM_BIF, [{{binary_to_integer, 1}, otpbp_erlang},
                        {{binary_to_integer, 2}, otpbp_erlang},
                        {{binary_to_float, 1}, otpbp_erlang},
                        {{integer_to_binary, 1}, otpbp_erlang},
                        {{float_to_binary, 1}, otpbp_erlang},
                        {{delete_element, 2}, otpbp_erlang},
                        {{insert_element, 3}, otpbp_erlang}]).
-define(TRANSFORM_FUN, [{{erlang, integer_to_binary, 2}, otpbp_erlang},
                        {{erlang, get_keys, 0}, otpbp_erlang},
                        {{application, ensure_started, 1}, otpbp_application},
                        {{application, ensure_started, 2}, otpbp_application},
                        {{application, ensure_all_started, 1}, otpbp_application},
                        {{application, ensure_all_started, 2}, otpbp_application},
                        {{application, get_env, 3}, otpbp_application},
                        {{error_handler, raise_undef_exception, 3}, otpbp_error_handler},
                        {{file, list_dir_all, 1}, otpbp_file},
                        {{file, read_link_all, 1}, otpbp_file},
                        {{inet, ntoa, 1}, otpbp_inet_parse},
                        {{inet, parse_address, 1}, {inet_parse, address}},
                        {{inet, parse_ipv4_address, 1}, {inet_parse, ipv4_address}},
                        {{inet, parse_ipv4strict_address, 1}, {inet_parse, ipv4strict_address}},
                        {{inet, parse_ipv6_address, 1}, {inet_parse, ipv6_address}},
                        {{inet, parse_ipv6strict_address, 1}, {inet_parse, ipv6strict_address}},
                        {{inet, parse_strict_address, 1}, otpbp_inet},
                        {{inet_parse, strict_address, 1}, otpbp_inet},
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
                        {{gen_server, system_get_state, 1}, otpbp_gen_fsm},
                        {{gen_server, system_replace_state, 2}, otpbp_gen_fsm},
                        {{io_lib, deep_latin1_char_list, 1}, {io_lib, deep_char_list}},
                        {{lists, droplast, 1}, otpbp_lists},
                        {{lists, filtermap, 2}, {lists, zf}},
                        {{orddict, is_empty, 1}, otpbp_orddict},
                        {{os, system_time, 1}, otpbp_os},
                        {{os, getenv, 2}, otpbp_os}]).

transform_list() ->
    lists:foreach(fun({{F, A}, S}) -> erlang:is_builtin(erlang, F, A) orelse put({erlang, F, A}, S) end, ?TRANSFORM_BIF),
    lists:foreach(fun({{M, F, A} = MFA, S}) ->
                      erlang:is_builtin(M, F, A) orelse
                          (catch lists:member({F, A}, M:module_info(exports))) =:= true orelse
                              put(MFA, S)
                  end, ?TRANSFORM_FUN).

do_transform({call, L, {remote, _, {atom, _, M}, {atom, _, N}}, A}) -> try_transform(M, N, length(A), L, make_call);

do_transform({call, L, {atom, _, N}, A}) -> do_transform(make_call(N, A, L));

do_transform({'fun', L, {function, {atom, _, M}, {atom, _, N}, {integer, _, A}}}) -> try_transform(M, N, A, L, make_fun);

do_transform({'fun', L, {function, N, A}}) -> do_transform(make_fun(N, A, L));

do_transform(_) -> continue.

make_mn(M, N) when is_atom(M) -> {M, N};
make_mn(MN, _) when tuple_size(MN) =:= 2 -> MN.

make_fun({M, N}, A, L) -> {'fun', L, {function, {atom, L, M}, {atom, L, N}, {integer, L, A}}};
make_fun(N, A, L) -> make_fun({erlang, N}, A, L).

make_call({M, N}, A, L) -> {call, L, {remote, L, {atom, L, M}, {atom, L, N}}, A};
make_call(N, A, L) -> make_call({erlang, N}, A, L).

try_transform(M, N, A, L, F) ->
    case get({M, N, A}) of
        undefined -> continue;
        MN -> F(make_mn(MN, N), A, L)
    end.
