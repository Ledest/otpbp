-module(io_lib_tests).

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

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
format_neg_zero_test() ->
    <<NegZero/float>> = <<16#8000000000000000:64>>,
    ?assertEqual("-0.000000", fmt("~f", [NegZero])),
    ?assertEqual("-0.00000e+0", fmt("~g", [NegZero])),
    ?assertEqual("-0.00000e+0", fmt("~e", [NegZero])),
    ?assertEqual("-0.0",  io_lib_format:fwrite_g(NegZero)),
    ok.
-endif.
-endif.

float_g_test() ->
    ?assertEqual(["5.00000e-2",
                  "0.500000",
                  "5.00000",
                  "50.0000",
                  "500.000",
                  "5000.00",
                  "5.00000e+4",
                  "5.00000e+5"],
                 float_g_1("~g", 5.0, -2, 5)),
    ?assertEqual(["-5.0000e-2",
                  "-0.50000",
                  "-5.0000",
                  "-50.000",
                  "-500.00",
                  "-5000.0",
                  "-5.0000e+4",
                  "-5.0000e+5"],
                 float_g_1("~.5g", -5.0, -2, 5)),
    ?assertEqual(["5.000e-2",
                  "0.5000",
                  "5.000",
                  "50.00",
                  "500.0",
                  "5.000e+3",
                  "5.000e+4",
                  "5.000e+5"],
                 float_g_1("~.4g", 5.0, -2, 5)),
    ?assertEqual(["-5.00e-2",
                  "-0.500",
                  "-5.00",
                  "-50.0",
                  "-5.00e+2",
                  "-5.00e+3",
                  "-5.00e+4",
                  "-5.00e+5"],
                 float_g_1("~.3g", -5.0, -2, 5)),
    ?assertEqual(["5.0e-2",
                  "0.50",
                  "5.0",
                  "5.0e+1",
                  "5.0e+2",
                  "5.0e+3",
                  "5.0e+4",
                  "5.0e+5"],
                 float_g_1("~.2g", 5.0, -2, 5)),
    ?assertEqual(["5.0e-2",
                  "0.5",
                  "5.0e+0",
                  "5.0e+1",
                  "5.0e+2",
                  "5.0e+3",
                  "5.0e+4",
                  "5.0e+5"],
                 float_g_1("~.1g", 5.0, -2, 5)),
    ?assertEqual(["4.99999e-2",
                  "0.499999",
                  "4.99999",
                  "49.9999",
                  "499.999",
                  "4999.99",
                  "4.99999e+4",
                  "4.99999e+5"],
                 float_g_1("~g", 4.9999949999, -2, 5)),
    ?assertEqual(["-5.00000e-2",
                  "-0.500000",
                  "-5.00000",
                  "-50.0000",
                  "-500.000",
                  "-5000.00",
                  "-5.00000e+4",
                  "-5.00000e+5"],
                 float_g_1("~g", -4.9999950001, -2, 5)),
    ok.

float_w_test() ->
    ?assertEqual("-9007199254740991.0", fmt("~w", [-float((1 bsl 53) -1)])),
    ?assertEqual("-9.007199254740992e15", fmt("~w", [-float(1 bsl 53)])),
    ?assertEqual("-9.007199254740992e15", fmt("~w", [-float((1 bsl 53) + 1)])),
    ?assertEqual("9007199254740991.0", fmt("~w", [float((1 bsl 53) -1)])),
    ?assertEqual("9.007199254740992e15", fmt("~w", [float(1 bsl 53)])),
    ?assertEqual("9.007199254740992e15", fmt("~w", [float((1 bsl 53) + 1)])),
    ok.

otp_5403_test() ->
    ?assertEqual("atom", fmt("~s", [atom])),
    ?assertEqual("binary", fmt("~s", [<<"binary">>])),
    ?assertEqual("atail", fmt("~s", [["a" | <<"tail">>]])),
    ?assertEqual("deepcharlist", fmt("~s", [["deep",["char",["list"]]]])),
    ?assertEqual("somebinaries", fmt("~s", [[<<"some">>,[<<"binaries">>]]])),
    ok.

otp_6230_test() ->
    ?assertEqual("<<>>", fmt("~P", [<<"">>, -1])),
    ?assertEqual("<<\"hej\">>", fmt("~P", [<<"hej">>, -1])),
    ?assertEqual("{hej,...}", fmt("~P", [{hej,<<"hej">>}, 2])),
    ?assertEqual("{hej,<<...>>}", fmt("~P", [{hej,<<"hej">>}, 3])),
    ?assertEqual("{hej,<<\"hejs\"...>>}", fmt("~P", [{hej,<<"hejsan">>}, 4])),
    ?assertEqual("{hej,<<\"hej\">>}", fmt("~P", [{hej, <<"hej">>}, 6])),
    ?assertEqual("<<...>>", fmt("~P", [<<"hej">>, 1])),
    ?assertEqual("<<\"hejs\"...>>", fmt("~P", [<<"hejsan">>, 2])),
    ?assertEqual("<<\"hej\">>", fmt("~P", [<<"hej">>, 4])),
    ?assertEqual("{hej,<<127,...>>}", fmt("~P", [{hej, <<127:8, <<"hej">>/binary>>}, 4])),
    ?assertEqual("{hej,<<127,104,101,...>>}", fmt("~P", [{hej, <<127:8, <<"hej">>/binary>>}, 6])),
    ?assertMatch("<<\"aaaa"++_, fmt("~P", [list_to_binary(lists:duplicate(30000, $a)), 20000])),
    ok.

otp_6517_test() ->
    ?assertMatch("string", fmt(<<"~s">>, [<<"string">>])),
    ok.

otp_6502_test() ->
    ?assertEqual(binary_to_list(<<"[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]"
                                  "<<0,0,8,\n"
                                  "                                                                     "
                                  "  1:1>>">>),
                 fmt("~w~p", [lists:seq(0, 25), <<17:25>>])),
    ok.

otp_8989_test() ->
    ?assertEqual(" Hello", fmt("~6.6s", ["Hello"])),
    ?assertEqual(" Hello", fmt("~*.6s", [6, "Hello"])),
    ?assertEqual(" Hello", fmt("~6.*s", [6, "Hello"])),
    ?assertEqual(" Hello", fmt("~*.*s", [6, 6, "Hello"])),
    ?assertEqual(" Hello", fmt("~6.5s", ["Hello"])),
    ?assertEqual(" Hello", fmt("~*.5s", [6, "Hello"])),
    ?assertEqual(" Hello", fmt("~6.*s", [5, "Hello"])),
    ?assertEqual(" Hello", fmt("~*.*s", [6, 5, "Hello"])),
    ?assertEqual("  Hell", fmt("~6.4s", ["Hello"])),
    ?assertEqual("  Hell", fmt("~*.4s", [6, "Hello"])),
    ?assertEqual("  Hell", fmt("~6.*s", [4, "Hello"])),
    ?assertEqual("  Hell", fmt("~*.*s", [6, 4, "Hello"])),
    ?assertEqual("Hello", fmt("~5.5s", ["Hello"])),
    ?assertEqual("Hello", fmt("~*.5s", [5, "Hello"])),
    ?assertEqual("Hello", fmt("~5.*s", [5, "Hello"])),
    ?assertEqual("Hello", fmt("~*.*s", [5, 5, "Hello"])),
    ?assertEqual(" Hell", fmt("~5.4s", ["Hello"])),
    ?assertEqual(" Hell", fmt("~*.4s", [5, "Hello"])),
    ?assertEqual(" Hell", fmt("~5.*s", [4, "Hello"])),
    ?assertEqual(" Hell", fmt("~*.*s", [5, 4, "Hello"])),
    ?assertEqual("Hell", fmt("~4.4s", ["Hello"])),
    ?assertEqual("Hell", fmt("~*.4s", [4, "Hello"])),
    ?assertEqual("Hell", fmt("~4.*s", [4, "Hello"])),
    ?assertEqual("Hell", fmt("~*.*s", [4, 4, "Hello"])),
    ?assertEqual(" Hel", fmt("~4.3s", ["Hello"])),
    ?assertEqual(" Hel", fmt("~*.3s", [4, "Hello"])),
    ?assertEqual(" Hel", fmt("~4.*s", [3, "Hello"])),
    ?assertEqual(" Hel", fmt("~*.*s", [4, 3, "Hello"])),
    ?assertEqual("Hello ", fmt("~-6.6s", ["Hello"])),
    ?assertEqual("Hello ", fmt("~*.6s", [-6, "Hello"])),
    ?assertEqual("Hello ", fmt("~-6.*s", [6, "Hello"])),
    ?assertEqual("Hello ", fmt("~*.*s", [-6, 6, "Hello"])),
    ?assertEqual("Hello ", fmt("~-6.5s", ["Hello"])),
    ?assertEqual("Hello ", fmt("~*.5s", [-6, "Hello"])),
    ?assertEqual("Hello ", fmt("~-6.*s", [5, "Hello"])),
    ?assertEqual("Hello ", fmt("~*.*s", [-6, 5, "Hello"])),
    ?assertEqual("Hell  ", fmt("~-6.4s", ["Hello"])),
    ?assertEqual("Hell  ", fmt("~*.4s", [-6, "Hello"])),
    ?assertEqual("Hell  ", fmt("~-6.*s", [4, "Hello"])),
    ?assertEqual("Hell  ", fmt("~*.*s", [-6, 4, "Hello"])),
    ?assertEqual("Hello", fmt("~-5.5s", ["Hello"])),
    ?assertEqual("Hello", fmt("~*.5s", [-5, "Hello"])),
    ?assertEqual("Hello", fmt("~-5.*s", [5, "Hello"])),
    ?assertEqual("Hello", fmt("~*.*s", [-5, 5, "Hello"])),
    ?assertEqual("Hell ", fmt("~-5.4s", ["Hello"])),
    ?assertEqual("Hell ", fmt("~*.4s", [-5, "Hello"])),
    ?assertEqual("Hell ", fmt("~-5.*s", [4, "Hello"])),
    ?assertEqual("Hell ", fmt("~*.*s", [-5, 4, "Hello"])),
    ?assertEqual("Hell", fmt("~-4.4s", ["Hello"])),
    ?assertEqual("Hell", fmt("~*.4s", [-4, "Hello"])),
    ?assertEqual("Hell", fmt("~-4.*s", [4, "Hello"])),
    ?assertEqual("Hell", fmt("~*.*s", [-4, 4, "Hello"])),
    ?assertEqual("Hel ", fmt("~-4.3s", ["Hello"])),
    ?assertEqual("Hel ", fmt("~*.3s", [-4, "Hello"])),
    ?assertEqual("Hel ", fmt("~-4.*s", [3, "Hello"])),
    ?assertEqual("Hel ", fmt("~*.*s", [-4, 3, "Hello"])),
    ok.

io_lib_print_binary_depth_one_test() ->
    ?assertEqual("<<>>", fmt("~W", [<<>>, 1])),
    ?assertEqual("<<>>", fmt("~P", [<<>>, 1])),
    ?assertEqual("<<...>>", fmt("~W", [<<1>>, 1])),
    ?assertEqual("<<...>>", fmt("~P", [<<1>>, 1])),
    ?assertEqual("<<...>>", fmt("~W", [<<1:7>>, 1])),
    ?assertEqual("<<...>>", fmt("~P", [<<1:7>>, 1])),
    ok.

otp_10836_test() ->
    ?assertEqual("äppleäpple", fmt("~ts", [[<<"äpple"/utf8>>, <<"äpple">>]])),
    ok.

io_lib_width_too_small_test() ->
    ?assertEqual("**", fmt("~2.3w", [3.14])),
    ?assertEqual("**", fmt("~2.5w", [3.14])),
    ok.

float_g_1(Fmt, V, Min, Max) -> [fmt(Fmt, [V * math:pow(10, E)]) || E <- lists:seq(Min, Max)].

fmt(Fmt, Args) ->
    check_bin_fmt(lists:flatten(io_lib:build_text(io_lib:scan_format(Fmt, Args))), Fmt, Args, []).

fmt(Fmt, Args, Opts) -> check_bin_fmt(lists:flatten(io_lib:format(Fmt, Args, Opts)), Fmt, Args, Opts).

check_bin_fmt(OrigRes, Fmt, Args, Opts) ->
    Res = unicode:characters_to_list(io_lib:bformat(Fmt, Args, Opts)),
    ?assertEqual(OrigRes, Res),
    Res.
