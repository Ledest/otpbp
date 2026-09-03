-module(versions_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

branch_base_test() ->
    ?assertEqual(<<"0.0">>, versions:branch_base(<<"0.">>)),
    ?assertEqual(<<"35.3.1">>, versions:branch_base(<<"35.3.1.">>)),
    ?assertEqual(<<"35.3">>, versions:branch_base(<<"35.3.0.">>)),
    ?assertEqual(<<"35.3.0.2">>, versions:branch_base(<<"35.3.0.2.">>)),
    ?assertEqual(<<"35.3.0.2.3">>, versions:branch_base(<<"35.3.0.2.3.">>)),
    ?assertEqual(<<"40.0">>, versions:branch_base(<<"40.0.0.">>)),
    ?assertEqual(<<"40.0">>, versions:branch_base(<<"40.0.0.0.">>)),
    ?assertEqual(<<"40.0">>, versions:branch_base(<<"40.0.0.0.0.">>)),
    ?assertEqual(<<"18.2.4">>, versions:branch_base(<<"18.2.4.">>)),
    ?assertEqual(<<"18.2.4">>, versions:branch_base(<<"18.2.4.0.">>)),
    ?assertEqual(<<"18.2.4">>, versions:branch_base(<<"18.2.4.0.0.">>)),
    ?assertEqual(<<"0.0">>, versions:branch_base(versions:branch(<<"0.0">>))),
    ok.

branch_test() ->
    ?assertEqual(<<"0.">>, versions:branch(<<"0.0">>)),
    ?assertEqual(<<"0.">>, versions:branch(<<"0.3">>)),
    ?assertEqual(<<"0.">>, versions:branch(<<"0.3.1">>)),
    ?assertEqual(<<"0.">>, versions:branch(<<"17.0">>)),
    ?assertEqual(<<"0.">>, versions:branch(<<"35.2.7">>)),
    ?assertEqual(<<"0.">>, versions:branch(<<"35.3">>)),
    ?assertEqual(<<"35.3.1.">>, versions:branch(<<"35.3.1.1">>)),
    ?assertEqual(<<"35.3.1.">>, versions:branch(<<"35.3.1.2">>)),
    ?assertEqual(<<"35.3.0.">>, versions:branch(<<"35.3.0.1">>)),
    ?assertEqual(<<"35.3.0.">>, versions:branch(<<"35.3.0.2">>)),
    ?assertEqual(<<"35.3.0.2.">>, versions:branch(<<"35.3.0.2.3">>)),
    ?assertEqual(<<"35.3.0.2.">>, versions:branch(<<"35.3.0.2.2">>)),
    ?assertEqual(<<"35.3.0.2.3.">>, versions:branch(<<"35.3.0.2.3.1">>)),
    ?assertEqual(<<"35.3.0.2.3.">>, versions:branch(<<"35.3.0.2.3.2">>)),
    ?assertEqual(<<"40.0.0.">>, versions:branch(<<"40.0.0.1">>)),
    ?assertEqual(<<"40.0.0.0.">>, versions:branch(<<"40.0.0.0.1">>)),
    ?assertEqual(<<"40.0.0.0.0.">>, versions:branch(<<"40.0.0.0.0.1">>)),
    ?assertEqual(<<"18.2.4.">>, versions:branch(<<"18.2.4.1">>)),
    ?assertEqual(<<"18.2.4.0.">>, versions:branch(<<"18.2.4.0.1">>)),
    ?assertEqual(<<"18.2.4.0.0.">>, versions:branch(<<"18.2.4.0.0.1">>)),
    ok.
