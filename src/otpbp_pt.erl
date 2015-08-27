-module(otpbp_pt).
-export([parse_transform/2]).

-define(TRCALL(M, N, A), do_transform({call, L, {remote, _, {atom, _, M}, {atom, _, N}}, Args}) when length(Args) =:= A).
-define(TLCALL(N, A), do_transform({call, L, {atom, _, N}, Args}) when length(Args) =:= A).
-define(RCALLA(M, N, A), {call, L, {remote, L, {atom, L, M}, {atom, L, N}}, A}).
-define(LCALLA(N, A), {call, L, {atom, L, N}, A}).
-define(RCALL(M, N), ?RCALLA(M, N, do_parse_args(Args))).
-define(LCALL(N), ?LCALLA(N, do_parse_args(Args))).
-define(TRF(OM, ON, NM, NN, A), ?TRCALL(OM, ON, A) -> ?RCALL(NM, NN)).
-define(TLF(ON, NM, NN, A), ?TLCALL(ON, A) -> ?RCALL(NM, NN)).
-define(TRFM(OM, NM, N, A), ?TRF(OM, N, NM, N, A)).
-define(TLFM(M, N, A), ?TLF(N, M, N, A)).
-define(TRFN(M, ON, NN, A), ?TRF(M, ON, M, NN, A)).
-define(RFUN(M, N, A), {'fun', L, {function, {atom, L, M}, {atom, L, N}, {integer, L, A}}}).
-define(LFUN(N, A), {'fun', L, {function, N, A}}).
-define(TRFUN(OM, ON, NM, NN, A), do_transform({'fun', L, {function, {atom, _, OM}, {atom, _, ON}, {integer, _, A}}}) -> ?RFUN(NM, NN, A)).
-define(TLFUN(ON, M, NN, A), do_transform(?LFUN(ON, A)) -> ?RFUN(M, NN, A)).
-define(TRFUNM(OM, NM, N, A), ?TRFUN(OM, N, NM, N, A)).
-define(TRFUNN(M, ON, NN, A), ?TRFUN(M, ON, M, NN, A)).
-define(TLFUNM(N, M, A), ?TLFUN(N, M, N, A)).

parse_transform(Forms, _Options) -> parse_trans:plain_transform(fun do_transform/1, Forms).

?TRFM(application, otpbp_application, ensure_started, 1);
?TRFUNM(application, otpbp_application, ensure_started, 1);
?TRFM(application, otpbp_application, ensure_started, 2);
?TRFUNM(application, otpbp_application, ensure_started, 2);
?TRFM(application, otpbp_application, ensure_all_started, 1);
?TRFUNM(application, otpbp_application, ensure_all_started, 1);
?TRFM(application, otpbp_application, ensure_all_started, 2);
?TRFUNM(application, otpbp_application, ensure_all_started, 2);
?TRFM(application, otpbp_application, get_env, 3);
?TRFUNM(application, otpbp_application, get_env, 3);
?TRFM(error_handler, otpbp_error_handler, raise_undef_exception, 3);
?TRFUNM(error_handler, otpbp_error_handler, raise_undef_exception, 3);
?TRFM(file, otpbp_file, list_dir_all, 1);
?TRFUNM(file, otpbp_file, list_dir_all, 1);
?TRFM(file, otpbp_file, read_link_all, 1);
?TRFUNM(file, otpbp_file, read_link_all, 1);
%?TRFM(inet, otpbp_inet, ntoa, 1);
%?TRFUNM(inet, otpbp_inet, ntoa, 1);
?TRFM(inet, inet_parse, ntoa, 1);
?TRFUNM(inet, inet_parse, ntoa, 1);
%?TRFM(inet, otpbp_inet, parse_address, 1);
%?TRFUNM(inet, otpbp_inet, parse_address, 1);
?TRF(inet, parse_address, inet_parse, address, 1);
?TRFUN(inet, parse_address, inet_parse, address, 1);
%?TRFM(inet, otpbp_inet, parse_ipv4_address, 1);
%?TRFUNM(inet, otpbp_inet, parse_ipv4_address, 1);
?TRF(inet, parse_ipv4_address, inet_parse, ipv4_address, 1);
?TRFUN(inet, parse_ipv4_address, inet_parse, ipv4_address, 1);
%?TRFM(inet, otpbp_inet, parse_ipv4strict_address, 1);
%?TRFUNM(inet, otpbp_inet, parse_ipv4strict_address, 1);
?TRF(inet, parse_ipv4strict_address, inet_parse, ipv4strict_address, 1);
?TRFUN(inet, parse_ipv4strict_address, inet_parse, ipv4strict_address, 1);
%?TRFM(inet, otpbp_inet, parse_ipv6_address, 1);
%?TRFUNM(inet, otpbp_inet, parse_ipv6_address, 1);
?TRF(inet, parse_ipv6_address, inet_parse, ipv6_address, 1);
?TRFUN(inet, parse_ipv6_address, inet_parse, ipv6_address, 1);
%?TRFM(inet, otpbp_inet, parse_ipv6strict_address, 1);
%?TRFUNM(inet, otpbp_inet, parse_ipv6strict_address, 1);
?TRF(inet, parse_ipv6strict_address, inet_parse, ipv6strict_address, 1);
?TRFUN(inet, parse_ipv6strict_address, inet_parse, ipv6strict_address, 1);
?TRF(inet, parse_strict_address, otpbp_inet_parse, strict_address, 1);
?TRFUN(inet, parse_strict_address, otpbp_inet_parse, strict_address, 1);
?TRFM(inet_parse, otpbp_inet_parse, strict_address, 1);
?TRFUNM(inet_parse, otpbp_inet_parse, strict_address, 1);
?TRFM(edlin, otpbp_edlin, current_chars, 1);
?TRFUNM(edlin, otpbp_edlin, current_chars, 1);

?TRFM(erl_scan, otpbp_erl_scan, category, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, category, 1);
?TRFM(erl_scan, otpbp_erl_scan, column, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, column, 1);
?TRFM(erl_scan, otpbp_erl_scan, line, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, line, 1);
?TRFM(erl_scan, otpbp_erl_scan, location, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, location, 1);
?TRFM(erl_scan, otpbp_erl_scan, symbol, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, symbol, 1);
?TRFM(erl_scan, otpbp_erl_scan, text, 1);
?TRFUNM(erl_scan, otpbp_erl_scan, text, 1);

?TRFM(lists, otpbp_lists, droplast, 1);
?TRFUNM(lists, otpbp_lists, droplast, 1);
?TRFN(lists, filtermap, zf, 2);
?TRFUNN(lists, filtermap, zf, 1);
?TRFM(os, otpbp_os, system_time, 1);
?TRFUNM(os, otpbp_os, system_time, 1);
?TRFM(os, otpbp_os, getenv, 2);
?TRFUNM(os, otpbp_os, getenv, 2);

?TRFM(erlang, otpbp_erlang, integer_to_binary, 1);
?TLFM(otpbp_erlang, integer_to_binary, 1);
?TRFUNM(erlang, otpbp_erlang, integer_to_binary, 1);
?TLFUNM(integer_to_binary, otpbp_erlang, 1);
?TRFM(erlang, otpbp_erlang, integer_to_binary, 2);
?TLFM(otpbp_erlang, integer_to_binary, 2);
?TRFUNM(erlang, otpbp_erlang, integer_to_binary, 2);
?TLFUNM(integer_to_binary, otpbp_erlang, 2);
?TRFM(erlang, otpbp_erlang, float_to_binary, 1);
?TLFM(otpbp_erlang, float_to_binary, 1);
?TRFUNM(erlang, otpbp_erlang, float_to_binary, 1);
?TLFUNM(float_to_binary, otpbp_erlang, 1);
?TRFM(erlang, otpbp_erlang, binary_to_integer, 1);
?TLFM(otpbp_erlang, binary_to_integer, 1);
?TRFUNM(erlang, otpbp_erlang, binary_to_integer, 1);
?TLFUNM(binary_to_integer, otpbp_erlang, 1);
?TRFM(erlang, otpbp_erlang, binary_to_integer, 2);
?TLFM(otpbp_erlang, binary_to_integer, 2);
?TRFUNM(erlang, otpbp_erlang, binary_to_integer, 2);
?TLFUNM(binary_to_integer, otpbp_erlang, 2);
?TRFM(erlang, otpbp_erlang, binary_to_float, 1);
?TLFM(otpbp_erlang, binary_to_float, 1);
?TRFUNM(erlang, otpbp_erlang, binary_to_float, 1);
?TLFUNM(binary_to_float, otpbp_erlang, 1);
%?TRCALL(erlang, integer_to_binary, 1) -> ?RCALLA(erlang, list_to_binary, [?RCALL(erlang, integer_to_list)]);
%?TLCALL(integer_to_binary, 1) -> ?LCALLA(integer_to_binary, [?LCALL(integer_to_list)]);
%?TRCALL(erlang, integer_to_binary, 2) -> ?RCALLA(erlang, list_to_binary, [?RCALL(erlang, integer_to_list)]);
%?TLCALL(integer_to_binary, 2) -> ?LCALLA(integer_to_binary, [?LCALL(integer_to_list)]);
%?TRCALL(erlang, float_to_binary, 1) -> ?RCALLA(erlang, list_to_binary, [?RCALL(erlang, float_to_list)]);
%?TLCALL(float_to_binary, 1) -> ?LCALLA(list_to_binary, [?LCALL(float_to_list)]);
%?TRCALL(erlang, binary_to_integer, 1) -> ?RCALLA(erlang, list_to_integer, [?RCALL(erlang, binary_to_list)]);
%?TLCALL(binary_to_integer, 1) -> ?LCALLA(list_to_integer, [?LCALL(binary_to_list)]);
%?TRCALL(erlang, binary_to_integer, 2) -> ?RCALLA(erlang, list_to_integer, [?RCALL(erlang, binary_to_list)]);
%?TLCALL(binary_to_integer, 2) -> ?LCALLA(list_to_integer, [?LCALL(binary_to_list)]);
%?TRCALL(erlang, binary_to_float, 1) -> ?RCALLA(erlang, list_to_float, [?RCALL(erlang, binary_to_list)]);
%?TLCALL(binary_to_float, 1) -> ?LCALLA(list_to_float, [?LCALL(binary_to_list)]);
?TRFM(erlang, otpbp_erlang, get_keys, 0);
?TLFM(otpbp_erlang, get_keys, 0);
?TRFUNM(erlang, otpbp_erlang, get_keys, 0);
?TLFUNM(get_keys, otpbp_erlang, 0);
?TRFM(erlang, otpbp_erlang, delete_element, 2);
?TRFUNM(erlang, otpbp_erlang, delete_element, 2);
?TRFM(erlang, otpbp_erlang, insert_element, 3);
?TRFUNM(erlang, otpbp_erlang, insert_element, 3);
do_transform(_) -> continue.

do_parse_args(Args) when is_list(Args) -> [hd(parse_trans:plain_transform(fun do_transform/1, [E])) || E <- Args].
