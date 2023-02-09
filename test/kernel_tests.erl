-module(kernel_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

application_test() ->
    ?assertEqual(undefined, application:get_supervisor(stdlib)),
    ?assertEqual({ok, whereis(kernel_sup)}, application:get_supervisor(kernel)),
    ok.

code_test() ->
    ?assertEqual(loaded, code:module_status(?MODULE)),
    ?assertEqual(not_loaded, code:module_status('TEST')),
    ?assert(is_list(code:modified_modules())),
    ok.

os_test() ->
    Env = os:env(),
    ?assertEqual(Env, os:list_env_vars()),
    ?assertEqual(lists:map(fun({N, V}) -> N ++ [$=|V] end, Env), os:getenv()),
    ok.
