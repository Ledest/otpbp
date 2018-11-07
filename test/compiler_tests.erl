-module(compiler_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    ?assert(is_list(compile:env_compiler_options())).
