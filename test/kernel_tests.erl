-module(kernel_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

code_test() ->
    ?assertEqual(code:module_status(?MODULE), loaded),
    ?assertEqual(code:module_status('TEST'), not_loaded),
    ?assert(is_list(code:modified_modules())).


os_test() ->
    Env = os:env(),
    ?assertEqual(Env, os:list_env_vars()),
    ?assertEqual(lists:map(fun({N, V}) -> N ++ [$=|V] end, Env), os:getenv()),
    ok.
