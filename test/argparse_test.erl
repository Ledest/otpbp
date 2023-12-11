%%
%% Copyright Maxim Fedorov
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(argparse_test).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

readme_test() ->
    Rm = #{arguments => [#{name => dir},
                         #{name => force, short => $f, type => boolean, default => false},
                         #{name => recursive, short => $r, type => boolean}]},
    ?assertEqual({ok, #{dir => "dir", force => true, recursive => true}, [prog()], Rm},
                 argparse:parse(["-rf", "dir"], Rm)),
    %% override progname
    ?assertEqual("Usage:\n  readme\n", unicode:characters_to_list(argparse:help(#{}, #{progname => "readme"}))),
    ?assertEqual("Usage:\n  readme\n", unicode:characters_to_list(argparse:help(#{}, #{progname => readme}))),
    ?assertEqual("Usage:\n  readme\n", unicode:characters_to_list(argparse:help(#{}, #{progname => <<"readme">>}))),
    %% test that command has priority over just a positional argument:
    %%  - parsing "opt sub" means "find positional argument "pos", then enter subcommand
    %%  - parsing "sub opt" means "enter sub-command, and then find positional argument"
    Cmd = #{commands => #{"sub" => #{}}, arguments => [#{name => pos}]},
    ?assertEqual(parse("opt sub", Cmd), parse("sub opt", Cmd)).

basic_test() ->
    Prog = prog(),
    %% empty command, with full options path
    ?assertMatch({ok, #{}, [Prog, "cmd"], #{}}, argparse:parse(["cmd"], #{commands => #{"cmd" => #{}}})),
    %% sub-command, with no path, but user-supplied argument
    ?assertEqual({ok, #{}, [Prog, "cmd", "sub"], #{attr => pos}},
                 argparse:parse(["cmd", "sub"], #{commands => #{"cmd" => #{commands => #{"sub" => #{attr => pos}}}}})),
    %% command with positional argument
    PosCmd = #{arguments => [#{name => pos}]},
    ?assertEqual({ok, #{pos => "arg"}, [Prog, "cmd"], PosCmd},
                 argparse:parse(["cmd", "arg"], #{commands => #{"cmd" => PosCmd}})),
    %% command with optional argument
    OptCmd = #{arguments => [#{name => force, short => $f, type => boolean}]},
    ?assertEqual({ok, #{force => true}, [Prog, "rm"], OptCmd},
                 parse(["rm -f"], #{commands => #{"rm" => OptCmd}})),
    %% command with optional and positional argument
    PosOptCmd = #{arguments => [#{name => force, short => $f, type => boolean}, #{name => dir}]},
    ?assertEqual({ok, #{force => true, dir => "dir"}, [Prog, "rm"], PosOptCmd},
                 parse(["rm -f dir"], #{commands => #{"rm" => PosOptCmd}})),
    %% no command, just argument list
    KernelCmd = #{arguments => [#{name => kernel, long => "kernel", type => atom, nargs => 2}]},
    ?assertEqual({ok, #{kernel => [port, dist]}, [Prog], KernelCmd}, parse(["-kernel port dist"], KernelCmd)),
    %% same but positional
    ArgListCmd = #{arguments => [#{name => arg, nargs => 2, type => boolean}]},
    ?assertEqual({ok, #{arg => [true, false]}, [Prog], ArgListCmd}, parse(["true false"], ArgListCmd)).

long_form_eq_test() ->
    %% cmd --arg=value
    PosOptCmd = #{arguments => [#{name => arg, long => "-arg"}]},
    ?assertEqual({ok, #{arg => "value"}, [prog(), "cmd"], PosOptCmd},
                 parse(["cmd --arg=value"], #{commands => #{"cmd" => PosOptCmd}})),
    %% --integer=10
    ?assertMatch({ok, #{int := 10}, _, _},
                 parse(["--int=10"], #{arguments => [#{name => int, type => integer, long => "-int"}]})).

built_in_types_test() ->
    Prog = [prog()],
    Bool = #{arguments => [#{name => meta, type => boolean, short => $b, long => "-boolean"}]},
    ?assertEqual({ok, #{}, Prog, Bool}, parse([""], Bool)),
    ?assertEqual({ok, #{meta => true}, Prog, Bool}, parse(["-b"], Bool)),
    ?assertEqual({ok, #{meta => true}, Prog, Bool}, parse(["--boolean"], Bool)),
    ?assertEqual({ok, #{meta => false}, Prog, Bool}, parse(["--boolean false"], Bool)),
    %% integer tests
    Int = #{arguments => [#{name => int, type => integer, short => $i, long => "-int"}]},
    ?assertEqual({ok, #{int => 1}, Prog, Int}, parse([" -i 1"], Int)),
    ?assertEqual({ok, #{int => 1}, Prog, Int}, parse(["--int 1"], Int)),
    ?assertEqual({ok, #{int => -1}, Prog, Int}, parse(["-i -1"], Int)),
    %% floating point
    Float = #{arguments => [#{name => f, type => float, short => $f}]},
    ?assertEqual({ok, #{f => 44.44}, Prog, Float}, parse(["-f 44.44"], Float)),
    %% atoms, existing
    Atom = #{arguments => [#{name => atom, type => atom, short => $a, long => "-atom"}]},
    ?assertEqual({ok, #{atom => atom}, Prog, Atom}, parse(["-a atom"], Atom)),
    ?assertEqual({ok, #{atom => atom}, Prog, Atom}, parse(["--atom atom"], Atom)).

type_validators_test() ->
    %% successful string regexes
    ?assertMatch({ok, #{str := "me"}, _, _}, parse_opts("me", [#{name => str, type => {string, "m."}}])),
    ?assertMatch({ok, #{str := "me"}, _, _}, parse_opts("me", [#{name => str, type => {string, "m.", []}}])),
    ?assertMatch({ok, #{"str" := "me"}, _, _},
                 parse_opts("me", [#{name => "str", type => {string, "m.", [{capture, none}]}}])),
    %% and binary too...
    ?assertMatch({ok, #{bin := <<"me">>}, _, _}, parse_opts("me", [#{name => bin, type => {binary, <<"m.">>}}])),
    ?assertMatch({ok, #{<<"bin">> := <<"me">>}, _, _},
                 parse_opts("me", [#{name => <<"bin">>, type => {binary, <<"m.">>, []}}])),
    ?assertMatch({ok, #{bin := <<"me">>}, _, _},
                 parse_opts("me", [#{name => bin, type => {binary, <<"m.">>, [{capture, none}]}}])),
    %% successful integer with range validators
    ?assertMatch({ok, #{int := 5}, _, _}, parse_opts("5", [#{name => int, type => {integer, [{min, 0}, {max, 10}]}}])),
    ?assertMatch({ok, #{bin := <<"5">>}, _, _}, parse_opts("5", [#{name => bin, type => binary}])),
    ?assertMatch({ok, #{str := "011"}, _, _},
                 parse_opts("11", [#{name => str, type => {custom, fun(S) -> [$0|S] end}}])),
    %% choices: valid
    ?assertMatch({ok, #{bin := <<"K">>}, _, _}, parse_opts("K", [#{name => bin, type => {binary, [<<"M">>, <<"K">>]}}])),
    ?assertMatch({ok, #{str := "K"}, _, _}, parse_opts("K", [#{name => str, type => {string, ["K", "N"]}}])),
    ?assertMatch({ok, #{atom := one}, _, _}, parse_opts("one", [#{name => atom, type => {atom, [one, two]}}])),
    ?assertMatch({ok, #{int := 12}, _, _}, parse_opts("12", [#{name => int, type => {integer, [10, 12]}}])),
    ?assertMatch({ok, #{float := 1.3}, _, _}, parse_opts("1.3", [#{name => float, type => {float, [1.3, 1.4]}}])),
    %% test for unsafe atom
    %% ensure the atom does not exist
    ?assertException(error, badarg, list_to_existing_atom("$can_never_be")),
    {ok, ArgMap, _, _} = parse_opts("$can_never_be", [#{name => atom, type => {atom, unsafe}}]),
    argparse:validate(#{arguments => [#{name => atom, type => {atom, unsafe}}]}),
    %% now that atom exists, because argparse created it (in an unsafe way!)
    ?assertEqual(list_to_existing_atom("$can_never_be"), maps:get(atom, ArgMap)),
    %% test successful user-defined conversion
    ?assertMatch({ok, #{user := "VER"}, _, _},
                 parse_opts("REV", [#{name => user, type => {custom, fun lists:reverse/1}}])).

invalid_arguments_test() ->
    %% {float, [{min, float()} | {max, float()}]} |
    Prog = [prog()],
    MinFloat = #{name => float, type => {float, [{min, 1.0}]}},
    ?assertEqual({error, {Prog, MinFloat, "0.0", <<"is less than accepted minimum">>}}, parse_opts("0.0", [MinFloat])),
    MaxFloat = #{name => float, type => {float, [{max, 1.0}]}},
    ?assertEqual({error, {Prog, MaxFloat, "2.0", <<"is greater than accepted maximum">>}},
                 parse_opts("2.0", [MaxFloat])),
    %% {int, [{min, integer()} | {max, integer()}]} |
    MinInt = #{name => int, type => {integer, [{min, 20}]}},
    ?assertEqual({error, {Prog, MinInt, "10", <<"is less than accepted minimum">>}}, parse_opts("10", [MinInt])),
    MaxInt = #{name => int, type => {integer, [{max, -10}]}},
    ?assertEqual({error, {Prog, MaxInt, "-5", <<"is greater than accepted maximum">>}}, parse_opts("-5", [MaxInt])),
    %% string: regex & regex with options
    %% {string, string()} | {string, string(), []}
    StrRegex = #{name => str, type => {string, "me.me"}},
    ?assertEqual({error, {Prog, StrRegex, "me", <<"does not match">>}}, parse_opts("me", [StrRegex])),
    StrRegexOpt = #{name => str, type => {string, "me.me", []}},
    ?assertEqual({error, {Prog, StrRegexOpt, "me", <<"does not match">>}}, parse_opts("me", [StrRegexOpt])),
    %% {binary, {re, binary()} | {re, binary(), []}
    BinRegex = #{name => bin, type => {binary, <<"me.me">>}},
    ?assertEqual({error, {Prog, BinRegex, "me", <<"does not match">>}}, parse_opts("me", [BinRegex])),
    BinRegexOpt = #{name => bin, type => {binary, <<"me.me">>, []}},
    ?assertEqual({error, {Prog, BinRegexOpt, "me", <<"does not match">>}}, parse_opts("me", [BinRegexOpt])),
    %% invalid integer (comma , is not parsed)
    ?assertEqual({error, {Prog, MinInt, "1,", <<"is not an integer">>}}, parse_opts(["1,"], [MinInt])),
    %% test invalid choices
    BinChoices = #{name => bin, type => {binary, [<<"M">>, <<"N">>]}},
    ?assertEqual({error, {Prog, BinChoices, "K", <<"is not one of the choices">>}}, parse_opts("K", [BinChoices])),
    StrChoices = #{name => str, type => {string, ["M", "N"]}},
    ?assertEqual({error, {Prog, StrChoices, "K", <<"is not one of the choices">>}}, parse_opts("K", [StrChoices])),
    AtomChoices = #{name => atom, type => {atom, [one, two]}, dummy => 'K'},
    ?assertEqual({error, {Prog, AtomChoices, "K", <<"is not one of the choices">>}}, parse_opts("K", [AtomChoices])),
    IntChoices = #{name => int, type => {integer, [10, 11]}},
    ?assertEqual({error, {Prog, IntChoices, "12", <<"is not one of the choices">>}}, parse_opts("12", [IntChoices])),
    FloatChoices = #{name => float, type => {float, [1.2, 1.4]}},
    ?assertEqual({error, {Prog, FloatChoices, "1.3", <<"is not one of the choices">>}},
                 parse_opts("1.3", [FloatChoices])),
    %% unsuccessful user-defined conversion
    ?assertMatch({error, {Prog, _, "REV", <<"failed faildation">>}},
                 parse_opts("REV", [#{name => user, type => {custom, fun integer_to_binary/1}}])).

complex_command_test() ->
    Command = #{arguments => [%% options
                              #{name => string, short => $s, long => "-string", action => append,
                                help => "String list option"},
                              #{name => boolean, type => boolean, short => $b, action => append,
                                help => "Boolean list option"},
                              #{name => float, type => float, short => $f, long => "-float", action => append,
                                help => "Float option"},
                              %% positional args
                              #{name => integer, type => integer, help => "Integer variable"},
                              #{name => string, help => "alias for string option", action => extend, nargs => list}]},
    ?assertEqual({ok,
                  #{float => [1.04, 112], boolean => [true, true], integer => 42, string => ["s1", "s2", "s3", "s4"]},
                  [prog(), "start"], Command},
                 argparse:parse(string:lexemes("start --float 1.04 -f 112 -b -b -s s1 42 --string s2 s3 s4", " "),
                                #{commands => #{"start" => Command}})).

unicode_test() ->
    %% test unicode short & long
    ?assertMatch({ok, #{one := true}, _, _}, parse(["-Ф"],
                 #{arguments => [#{name => one, short => $Ф, type => boolean}]})),
    ?assertMatch({ok, #{long := true}, _, _}, parse(["--åäö"],
                 #{arguments => [#{name => long, long => "-åäö", type => boolean}]})),
    %% test default, help and value in unicode
    Cmd = #{arguments => [#{name => text, type => binary, help => "åäö", default => <<"★"/utf8>>}]},
    Expected = #{text => <<"★"/utf8>>},
    Prog = [prog()],
    ?assertEqual({ok, Expected, Prog, Cmd}, argparse:parse([], Cmd)), %% default
    ?assertEqual({ok, Expected, Prog, Cmd}, argparse:parse(["★"], Cmd)), %% specified in the command line
    ?assert(lists:member(unicode:characters_to_list(argparse:help(Cmd)),
                         ["Usage:\n  " ++ prog() ++ " <text>\n\nArguments:\n  text åäö (binary, ★)\n",
                          "Usage:\n  " ++ prog() ++ " <text>\n\nArguments:\n  text åäö (binary), default: ★\n"])),
    %% test command name and argument name in unicode
    Uni = #{commands => #{"åäö" => #{help => "öФ"}}, handler => optional,
            arguments => [#{name => "Ф", short => $ä, long => "åäö"}]},
    ?assertEqual("Usage:\n  " ++ prog() ++
                 " {åäö} [-ä <Ф>] [-åäö <Ф>]\n\nSubcommands:\n  åäö      öФ\n\nOptional arguments:\n  -ä, -åäö Ф\n",
                 unicode:characters_to_list(argparse:help(Uni))),
    ?assertEqual({ok, #{"Ф" => "öФ"}, Prog, Uni}, argparse:parse(["-ä", "öФ"], Uni)).

parser_error_test() ->
    Prog = prog(),
    %% unknown option at the top of the path
    ?assertEqual({error, {[Prog], undefined, "arg", <<>>}}, parse_cmd(["arg"], #{})),
    %% positional argument missing in a sub-command
    ?assertMatch({error, {[Prog, "start"], _, undefined, <<>>}},
                 parse_cmd(["start"], #{"start" => #{arguments => [#{name => mode, required => true}]}})),
    %% optional argument missing in a sub-command
    ?assertMatch({error, {[Prog, "start"], _, undefined, <<>>}},
                 parse_cmd(["start"], #{"start" => #{arguments => [#{name => mode, short => $o, required => true}]}})),
    %% positional argument: an atom that does not exist
    Opt2 = #{name => atom, type => atom},
    ?assertEqual({error, {[Prog], Opt2, "boo-foo", <<"is not an existing atom">>}}, parse_opts(["boo-foo"], [Opt2])),
    %% optional argument missing some items
    Opt3 = #{name => kernel, long => "kernel", type => atom, nargs => 2},
    ?assertEqual({error, {[Prog], Opt3, ["port"], "expected 2, found 1 argument(s)"}},
                 parse_opts(["-kernel port"], [Opt3])),
    %% positional argument missing some items
    Opt4 = #{name => arg, type => atom, nargs => 3},
    ?assertEqual({error, {[Prog], Opt4, ["p1"], "expected 3, found 1 argument(s)"}}, parse_opts(["p1"], [Opt4])),
    %% short option with no argument, when it's needed
    ?assertMatch({error, {_, _, undefined, <<"expected argument">>}},
                 parse("-1", #{arguments => [#{name => short49, short => 49}]})).

nargs_test() ->
    Prog = [prog()],
    %% consume optional list arguments
    ?assertMatch({ok, #{arg := [1, 2, 3], bool := true}, _, _},
                 parse_opts(["-s 1 2 3 -b"],
                            [#{name => arg, short => $s, nargs => list, type => integer}, #{name => bool, short => $b,
                               type => boolean}])),
    %% consume one_or_more arguments in an optional list
    Opts2 = [#{name => arg, short => $s, nargs => nonempty_list}, #{name => extra, short => $x}],
    ?assertMatch({ok, #{extra := "X", arg := ["a","b","c"]}, _, _}, parse_opts(["-s port -s a b c -x X"], Opts2)),
    %% error if there is no argument to consume
    ?assertMatch({error, {_, _, ["-x"], <<"expected argument">>}}, parse_opts(["-s -x"], Opts2)),
    %% error when positional has nargs = nonempty_list or pos_integer
    ?assertMatch({error, {_, _, undefined, <<>>}}, parse_opts([""], [#{name => req, nargs => nonempty_list}])),
    %% positional arguments consumption: one or more positional argument
    OptsPos1 = #{arguments => [#{name => arg, nargs => nonempty_list}, #{name => extra, short => $x}]},
    ?assertEqual({ok, #{extra => "X", arg => ["b","c"]}, Prog, OptsPos1}, parse(["-x port -x a b c -x X"], OptsPos1)),
    %% positional arguments consumption, any number (maybe zero)
    OptsPos2 = #{arguments => [#{name => arg, nargs => list}, #{name => extra, short => $x}]},
    ?assertEqual({ok, #{extra => "X", arg => ["a","b","c"]}, Prog, OptsPos2}, parse(["-x port a b c -x X"], OptsPos2)),
    %% positional: consume ALL arguments!
    OptsAll = #{arguments => [#{name => arg, nargs => all}, #{name => extra, short => $x}]},
    ?assertEqual({ok, #{extra => "port", arg => ["a","b","c", "-x", "X"]}, Prog, OptsAll},
                 parse(["-x port a b c -x X"], OptsAll)),
    %% maybe with a specified default
    OptMaybe = [#{name => foo, long => "-foo", nargs => {'maybe', c}, default => d},
                #{name => bar, nargs => 'maybe', default => d}],
    ?assertMatch({ok, #{foo := "YY", bar := "XX"}, Prog, _}, parse_opts(["XX --foo YY"], OptMaybe)),
    ?assertMatch({ok, #{foo := c, bar := "XX"}, Prog, _}, parse_opts(["XX --foo"], OptMaybe)),
    ?assertMatch({ok, #{foo := d, bar := d}, Prog, _}, parse_opts([""], OptMaybe)),
    %% maybe with default provided by argparse
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := ok}, _, _},
                 parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, default => ok} | OptMaybe])),
    %% maybe arg - with no default given
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := 0}, _, _},
                 parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => integer} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := ""}, _, _},
                 parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => string} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := undefined}, _, _},
                 parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => atom} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := <<"">>}, _, _},
                 parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => binary} | OptMaybe])),
    %% nargs: optional list, yet it still needs to be 'not required'!
    OptList = [#{name => arg, nargs => list, required => false, type => integer}],
    ?assertEqual({ok, #{}, Prog, #{arguments => OptList}}, parse_opts("", OptList)),
    %% tests that action "count" with nargs "maybe" counts two times, first time
    %% consuming an argument (for "maybe"), second time just counting
    Cmd = #{arguments => [#{name => short49, short => $1, long => "-force", action => count, nargs => 'maybe'}]},
    ?assertEqual({ok, #{short49 => 2}, Prog, Cmd}, parse("-1 arg1 --force", Cmd)).

argparse_test() ->
    Prog = [prog()],
    Parser = #{arguments => [#{name => sum, long => "-sum", action => {store, sum}, default => max},
                             #{name => integers, type => integer, nargs => nonempty_list}]},
    ?assertEqual({ok, #{integers => [1, 2, 3, 4], sum => max}, Prog, Parser}, parse("1 2 3 4", Parser)),
    ?assertEqual({ok, #{integers => [1, 2, 3, 4], sum => sum}, Prog, Parser}, parse("1 2 3 4 --sum", Parser)),
    ?assertEqual({ok, #{integers => [7, -1, 42], sum => sum}, Prog, Parser}, parse("--sum 7 -1 42", Parser)),
    %% name or flags
    Parser2 = #{arguments => [#{name => bar, required => true}, #{name => foo, short => $f, long => "-foo"}]},
    ?assertEqual({ok, #{bar => "BAR"}, Prog, Parser2}, parse("BAR", Parser2)),
    ?assertEqual({ok, #{bar => "BAR", foo => "FOO"}, Prog, Parser2}, parse("BAR --foo FOO", Parser2)),
    %PROG: error: the following arguments are required: bar
    ?assertMatch({error, {Prog, _, undefined, <<>>}}, parse("--foo FOO", Parser2)),
    %% action tests: default
    ?assertMatch({ok, #{foo := "1"}, Prog, _}, parse("--foo 1", #{arguments => [#{name => foo, long => "-foo"}]})),
    %% action test: store
    ?assertMatch({ok, #{foo := 42}, Prog, _},
                 parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, 42}}]})),
    %% action tests: boolean (variants)
    ?assertMatch({ok, #{foo := true}, Prog, _},
                 parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, true}}]})),
    ?assertMatch({ok, #{foo := 42}, Prog, _},
                 parse("--foo",
                       #{arguments => [#{name => foo, long => "-foo", type => boolean, action => {store, 42}}]})),
    ?assertMatch({ok, #{foo := true}, Prog, _},
                 parse("--foo", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertMatch({ok, #{foo := true}, Prog, _},
                 parse("--foo true", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertMatch({ok, #{foo := false}, Prog, _},
                 parse("--foo false", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    %% action tests: append & append_const
    ?assertMatch({ok, #{all := [1, "1"]}, Prog, _},
                 parse("--x 1 -x 1", #{arguments => [#{name => all, long => "-x", type => integer, action => append},
                                                     #{name => all, short => $x, action => append}]})),
    ?assertMatch({ok, #{all := ["Z", 2]}, Prog, _},
                 parse("--x -x", #{arguments => [#{name => all, long => "-x", action => {append, "Z"}},
                                                 #{name => all, short => $x, action => {append, 2}}]})),
    %% count:
    ?assertMatch({ok, #{v := 3}, Prog, _},
                 parse("-v -v -v", #{arguments => [#{name => v, short => $v, action => count}]})).

negative_test() ->
    Parser = #{arguments => [#{name => x, short => $x, type => integer, action => store},
                             #{name => foo, nargs => 'maybe', required => false}]},
    ?assertMatch({ok, #{x := -1}, _, _}, parse("-x -1", Parser)),
    ?assertMatch({ok, #{x := -1, foo := "-5"}, _, _}, parse("-x -1 -5", Parser)),
    %%
    Parser2 = #{arguments => [#{name => one, short => $1}, #{name => foo, nargs => 'maybe', required => false}]},
    %% negative number options present, so -1 is an option
    ?assertMatch({ok, #{one := "X"}, _, _}, parse("-1 X", Parser2)),
    %% negative number options present, so -2 is an option
    ?assertMatch({error, {_, undefined, "-2", _}}, parse("-2", Parser2)),
    %% negative number options present, so both -1s are options
    ?assertMatch({error, {_, _, undefined, _}}, parse("-1 -1", Parser2)),
    %% no "-" prefix, can only be an integer
    ?assertMatch({ok, #{foo := "-1"}, _, _}, argparse:parse(["-1"], Parser2, #{prefixes => "+"})),
    %% no "-" prefix, can only be an integer, but just one integer!
    ?assertMatch({error, {_, undefined, "-1", _}}, argparse:parse(["-2", "-1"], Parser2, #{prefixes => "+"})),
    %% just in case, floats work that way too...
    ?assertMatch({error, {_, undefined, "-2", _}}, parse("-2", #{arguments => [#{name => one, long => "1.2"}]})).

nodigits_test() ->
    %% verify nodigits working as expected
    Parser3 = #{arguments => [#{name => extra, short => $3}, #{name => arg, nargs => list}]},
    %% ensure not to consume optional prefix
    ?assertEqual({ok, #{extra => "X", arg => ["a","b","3"]}, [prog()], Parser3},
                 argparse:parse(string:lexemes("-3 port a b 3 +3 X", " "), Parser3, #{prefixes => "-+"})).

pos_mixed_with_opt_test() ->
    Parser = #{arguments => [#{name => pos},
                             #{name => opt, default => 24, type => integer, long => "-opt"},
                             #{name => vars, nargs => list}]},
    ?assertEqual({ok, #{pos => "1", opt => 8, vars => ["8", "9"]}, [prog()], Parser}, parse("1 2 --opt 8 8 9", Parser)).

default_for_not_required_test() ->
    ?assertMatch({ok, #{def := 1}, _, _},
                 parse("", #{arguments => [#{name => def, short => $d, required => false, default => 1}]})),
    ?assertMatch({ok, #{def := 1}, _, _}, parse("", #{arguments => [#{name => def, required => false, default => 1}]})).

global_default_test() ->
    ?assertMatch({ok, #{def := "global"}, _, _},
                 argparse:parse("", #{arguments => [#{name => def, type => integer, required => false}]},
                                #{default => "global"})).

subcommand_test() ->
    TwoCmd = #{arguments => [#{name => bar}]},
    Cmd = #{arguments => [#{name => force, type => boolean, short => $f}],
            commands => #{"one" => #{arguments => [#{name => foo, type => boolean, long => "-foo"}, #{name => baz}],
                                     commands => #{"two" => TwoCmd}}}},
    ?assertEqual({ok, #{force => true, baz => "N1O1O", foo => true, bar => "bar"}, [prog(), "one", "two"], TwoCmd},
                 parse("one N1O1O -f two --foo bar", Cmd)),
    %% it is an error not to choose subcommand
    ?assertEqual({error, {[prog(), "one"], undefined, undefined, <<"subcommand expected">>}},
                 parse("one N1O1O -f", Cmd)).

very_short_test() -> ?assertMatch({ok, #{x := "V"}, _, _}, parse("-xV", #{arguments => [#{name => x, short => $x}]})).

multi_short_test() ->
    %% ensure non-flammable argument does not explode, even when it's possible
    ?assertMatch({ok, #{v := "xv"}, _, _},
                 parse("-vxv", #{arguments => [#{name => v, short => $v}, #{name => x, short => $x}]})),
    %% ensure 'verbosity' use-case works
    ?assertMatch({ok, #{v := 3}, _, _}, parse("-vvv", #{arguments => [#{name => v, short => $v, action => count}]})),
    ?assertMatch({ok, #{recursive := true, force := true, path := "dir"}, _, _},
                 parse("-rf dir",
                       #{arguments => [#{name => recursive, short => $r, type => boolean},
                                       #{name => force, short => $f, type => boolean},
                                       #{name => path}]})).

proxy_arguments_test() ->
    Cmd = #{commands => #{"start" => #{arguments => [#{name => shell, short => $s, long => "-shell", type => boolean},
                                                     #{name => skip, short => $x, long => "-skip", type => boolean},
                                                     #{name => args, required => false, nargs => all}]},
                          "stop" => #{},
                          "status" => #{arguments => [#{name => skip, required => false, default => "ok"},
                                                      #{name => args, required => false, nargs => all}]},
                          "state" => #{arguments => [#{name => skip, required => false},
                                                     #{name => args, required => false, nargs => all}]}},
            arguments => [#{name => node}],
            handler => fun(#{}) -> ok end},
    Prog = prog(),
    ?assertMatch({ok, #{node := "node1"}, _, _}, parse("node1", Cmd)),
    ?assertMatch({ok, #{node := "node1"}, [Prog, "stop"], #{}}, parse("node1 stop", Cmd)),
    ?assertMatch({ok, #{node := "node2.org", shell := true, skip := true}, _, _}, parse("node2.org start -x -s", Cmd)),
    ?assertMatch({ok, #{args := ["-app","key","value"],node := "node1.org"}, [Prog, "start"], _},
                 parse("node1.org start -app key value", Cmd)),
    ?assertMatch({ok, #{args := ["-app","key","value", "-app2", "key2", "value2"], node := "node3.org", shell := true},
                  [Prog, "start"], _},
                 parse("node3.org start -s -app key value -app2 key2 value2", Cmd)),
    %% test that any non-required positionals are skipped
    ?assertMatch({ok, #{args := ["-a","bcd"], node := "node2.org", skip := "ok"}, _, _},
                 parse("node2.org status -a bcd", Cmd)),
    ?assertMatch({ok, #{args := ["-app", "key"], node := "node2.org"}, _, _}, parse("node2.org state -app key", Cmd)).

usage_test() ->
    Cmd = ubiq_cmd(),
    Float = "(float, 3.14)\n",
    Float1 = "(float), default: 3.14\n",
    Usage = "Usage:\n"
            "  erl start {crawler|doze} [-rfvl] [--force] [-i <interval>] [--req <weird>]\n"
            "      [--float <float>] [-s <shard>...] [-z <z>] [-m <more>] [-b <bin>] [-g <g>]\n"
            "      [-t <t>] ---maybe-req -y <y> --yyy <y> [-u <u>] [-c <choice>] [-q <fc>]\n"
            "      [-w <ac>] [--unsafe <au>] [--safe <as>] [-foobar <long>] <server> [<optpos>]\n"
            "\n"
            "Subcommands:\n"
            "  crawler      controls crawler behaviour\n"
            "  doze         dozes a bit\n\n"
            "Arguments:\n"
            "  server       server to start\n"
            "  optpos       optional positional (int)\n\n"
            "Optional arguments:\n"
            "  -r           recursive\n"
            "  -f, --force  force\n"
            "  -v           verbosity level\n"
            "  -i           interval set (int >= 1)\n"
            "  --req        required optional, right?\n"
            "  --float      floating-point long form argument (float), default: 3.14\n"
            "  -s           initial shards (int)\n"
            "  -z           between (1 <= int <= 10)\n"
            "  -l           maybe lower (int <= 10)\n"
            "  -m           less than 10 (int <= 10)\n"
            "  -b           binary with re (binary re: m)\n"
            "  -g           binary with re (binary re: m)\n"
            "  -t           string with re (string re: m)\n"
            "  ---maybe-req maybe required int (int)\n"
            "  -y, --yyy    string with re (string re: m)\n"
            "  -u           string choices (choice: 1, 2)\n"
            "  -c           tough choice (choice: 1, 2, 3)\n"
            "  -q           floating choice (choice: 2.10000, 1.20000)\n"
            "  -w           atom choice (choice: one, two)\n"
            "  --unsafe     unsafe atom (atom)\n"
            "  --safe       safe atom (existing atom)\n"
            "  -foobar      foobaring option\n",
    ?assert(lists:member(unicode:characters_to_list(argparse:help(Cmd, #{progname => "erl", command => ["start"]})),
                         [Usage, unicode:characters_to_list(string:replace(Usage, Float1, Float))])),
    FullCmd = "Usage:\n  erl"
              " <command> [-rfv] [--force] [-i <interval>] [--req <weird>] [--float <float>]\n\n"
              "Subcommands:\n"
              "  start       verifies configuration and starts server\n"
              "  status      prints server status\n"
              "  stop        stops running server\n\n"
              "Optional arguments:\n"
              "  -r          recursive\n"
              "  -f, --force force\n"
              "  -v          verbosity level\n"
              "  -i          interval set (int >= 1)\n"
              "  --req       required optional, right?\n"
              "  --float     floating-point long form argument (float), default: 3.14\n",
    ?assert(lists:member(unicode:characters_to_list(argparse:help(Cmd, #{progname => erl})),
                         [FullCmd, unicode:characters_to_list(string:replace(FullCmd, Float1, Float))])),
    CrawlerStatus = "Usage:\n  erl status crawler [-rfv] [--force] [-i <interval>] [--req <weird>]\n"
                    "      [--float <float>] [---extra <extra>]\n\n"
                    "Optional arguments:\n"
                    "  -r          recursive\n"
                    "  -f, --force force\n"
                    "  -v          verbosity level\n"
                    "  -i          interval set (int >= 1)\n"
                    "  --req       required optional, right?\n"
                    "  --float     floating-point long form argument (float), default: 3.14\n"
                    "  ---extra    extra option very deep\n",
    ?assert(lists:member(unicode:characters_to_list(argparse:help(Cmd, #{progname => "erl",
                                                                         command => ["status", "crawler"]})),
                         [CrawlerStatus, unicode:characters_to_list(string:replace(CrawlerStatus, Float1, Float))])).

usage_required_args_test() ->
    ?assertEqual("Usage:\n  " ++ prog() ++ " test --req <required>\n\nOptional arguments:\n  --req required\n",
                 unicode:characters_to_list(argparse:help(#{commands => #{"test" => #{arguments => [#{name => required,
                                                                                                      required => true,
                                                                                                      long => "-req"}]}}},
                                                          #{command => ["test"]}))).

usage_template_test() ->
    %% Argument (positional)
    ?assertEqual("Usage:\n  " ++ prog() ++ " [-s SHARD]\n\nArguments:\n  shard initial number, int with a default value of 0\n",
        unicode:characters_to_list(argparse:help(#{arguments => [#{name => shard, type => integer, default => 0,
                                                                   help => {"[-s SHARD]",
                                                                            ["initial number, ", type,
                                                                              <<" with a default value of ">>,
                                                                              default]}}]},
                                                 #{}))),
    %% Optional
    ?assertEqual("Usage:\n  " ++ prog() ++ " [-s SHARD]\n\nOptional arguments:\n  -s initial number\n",
                 unicode:characters_to_list(argparse:help(#{arguments => [#{name => shard, short => $s,
                                                                            type => integer, default => 0,
                                                                            help => {<<"[-s SHARD]">>, ["initial number"]}}]},
                                                          #{}))),
    %% ISO Date example
    DefaultRange = {{2020, 1, 1}, {2020, 6, 22}},
    F = fun() ->
            {{FY, FM, FD}, {TY, TM, TD}} = DefaultRange,
            lists:flatten(io_lib:format("date range, ~b-~b-~b..~b-~b-~b", [FY, FM, FD, TY, TM, TD]))
        end,
    ?assertEqual("Usage:\n  " ++ prog() ++
                 " [--range RNG]\n\nOptional arguments:\n  -r, --range date range, 2020-1-1..2020-6-22\n",
                 unicode:characters_to_list(argparse:help(#{arguments => [#{name => range, long => "-range", short => $r,
                                                                            help => {"[--range RNG]", F},
                                                                            type => {custom,
                                                                                     fun(S) -> [S, DefaultRange] end},
                                                                            default => DefaultRange}]},
                                                          #{}))).

parser_error_usage_test() ->
    %% unknown arguments
    Prog = prog(),
    ?assertEqual(Prog ++ ": unknown argument: arg", parser_error(["arg"], #{})),
    ?assertEqual(Prog ++ ": unknown argument: -a", parser_error(["-a"], #{})),
    %% missing argument
    ?assertEqual(Prog ++ ": required argument missing: need", parser_error([""], #{arguments => [#{name => need}]})),
    ?assertEqual(Prog ++ ": required argument missing: need",
                 parser_error([""], #{arguments => [#{name => need, short => $n, required => true}]})),
    %% invalid value
    ?assertEqual(Prog ++ ": invalid argument for need: foo is not an integer",
                 parser_error(["foo"], #{arguments => [#{name => need, type => integer}]})),
    ?assertEqual(Prog ++ ": invalid argument for need: cAnNotExIsT is not an existing atom",
                 parser_error(["cAnNotExIsT"], #{arguments => [#{name => need, type => atom}]})).

command_usage_test() ->
    ?assertEqual("Options:\n  -o  option help\n  arg argument help\nNOTAUSAGE  " ++ prog() ++ " [-o <opt>] <arg>\n",
                 unicode:characters_to_list(argparse:help(#{arguments => [#{name => arg, help => "argument help"},
                                                                          #{name => opt, short => $o,
                                                                            help => "option help"}],
                                                            help => ["Options:\n", options, arguments, <<"NOTAUSAGE">>,
                                                                     usage, "\n"]},
                                                          #{}))).

usage_width_test() ->
    Cmd = #{arguments => [#{name => arg,
                            help => "argument help that spans way over allowed viewport width, wrapping words"},
                          #{name => opt, short => $o, long => "-option_long_name",
                            help => "another quite long word wrapped thing spanning over several lines"},
                          #{name => v, short => $v, type => boolean},
                          #{name => q, short => $q, type => boolean}],
            commands => #{"cmd1" => #{help => "Help for command number 1, not fitting at all"},
                          "cmd2" => #{help => <<"Short help">>},
                          "cmd3" => #{help => "Yet another instance of a very long help message"}},
            help => "  Very long help line taking much more than 40 characters allowed by the test case.\n"
                    "Also containing a few newlines.\n\n"
                    "   Indented new lines must be honoured!"},
    ?assertEqual("Usage:\n  erl {cmd1|cmd2|cmd3} [-vq] [-o <opt>]\n"
                 "      [--option_long_name <opt>] <arg>\n\n"
                 "  Very long help line taking much more\n"
                 "than 40 characters allowed by the test\n"
                 "case.\n"
                 "Also containing a few newlines.\n\n"
                 "   Indented new lines must be honoured!\n\n"
                 "Subcommands:\n"
                 "  cmd1                   Help for\n"
                 "                         command number\n"
                 "                         1, not fitting\n"
                 "                         at all\n"
                 "  cmd2                   Short help\n"
                 "  cmd3                   Yet another\n"
                 "                         instance of a\n"
                 "                         very long help\n"
                 "                         message\n\n"
                 "Arguments:\n"
                 "  arg                    argument help\n"
                 "                         that spans way\n"
                 "                         over allowed\n"
                 "                         viewport width,\n"
                 "                         wrapping words\n\n"
                 "Optional arguments:\n"
                 "  -o, --option_long_name another quite\n"
                 "                         long word\n"
                 "                         wrapped thing\n"
                 "                         spanning over\n"
                 "                         several lines\n"
                 "  -v                     v\n"
                 "  -q                     q\n",
                 unicode:characters_to_list(argparse:help(Cmd, #{columns => 40, progname => "erl"}))).

validator_exception_test() ->
    Prg = [prog()],
    %% conflicting option names
    ?assertException(error, {_argparse, argument, Prg, short, "short conflicting with previously defined short for one"},
                     argparse:validate(#{arguments => [#{name => one, short => $$}, #{name => two, short => $$}]})),
    ?assertException(error, {_argparse, argument, Prg, long, "long conflicting with previously defined long for one"},
                     argparse:validate(#{arguments => [#{name => one, long => "a"}, #{name => two, long => "a"}]})),
    %% broken options
    %% long must be a string
    ?assertException(error, {_argparse, argument, Prg, long, _},
                     argparse:validate(#{arguments => [#{name => one, long => ok}]})),
    %% short must be a printable character
    ?assertException(error, {_argparse, argument, Prg, short, _},
                     argparse:validate(#{arguments => [#{name => one, short => ok}]})),
    ?assertException(error, {_argparse, argument, Prg, short, _},
                     argparse:validate(#{arguments => [#{name => one, short => 7}]})),
    %% required is a boolean
    ?assertException(error, {_argparse, argument, Prg, required, _},
                     argparse:validate(#{arguments => [#{name => one, required => ok}]})),
    ?assertException(error, {_argparse, argument, Prg, help, _},
                     argparse:validate(#{arguments => [#{name => one, help => ok}]})),
    %% broken commands
    try
        argparse:help(#{}, #{progname => 123}),
        ?assert(false)
    catch error:badarg:Stack ->
        [{_, _, _, Ext} | _] = Stack,
        case proplists:get_value(error_info, Ext) of
            #{cause := #{2 := Detail}} -> ?assertEqual(<<"progname is not valid">>, Detail);
            undefined -> ok
        end
    end,
    %% not-a-list of arguments provided to a subcommand
    Prog = prog(),
    ?assertException(error, {_argparse, command, [Prog, "start"], arguments, <<"expected a list, [argument()]">>},
                     argparse:validate(#{commands => #{"start" => #{arguments => atom}}})),
    %% command is not a map
    ?assertException(error, {_argparse, command, Prg, commands, <<"expected map of #{string() => command()}">>},
                     argparse:validate(#{commands => []})),
    %% invalid commands field
    ?assertException(error, {_argparse, command, Prg, commands, _}, argparse:validate(#{commands => ok})),
    ?assertException(error, {_argparse, command, _, commands, _}, argparse:validate(#{commands => #{ok => #{}}})),
    ?assertException(error,
                     {_argparse, command, _, help, <<"must be a printable unicode list, or a command help template">>},
                     argparse:validate(#{commands => #{"ok" => #{help => ok}}})),
    F = fun() -> [{doc, "Tests that the validator throws expected exceptions"}] end,
    ?assertException(error, {_argparse, command, _, handler, _},
                     argparse:validate(#{commands => #{"ok" => #{handler => F}}})),
    %% extend + maybe: validator exception
    ?assertException(error, {_argparse, argument, _, action, <<"extend action works only with lists">>},
                     parse("-1 -1",
                           #{arguments => [#{action => extend, name => short49, nargs => 'maybe', short => 49}]})).

validator_exception_format_test() ->
    %% set up as a contract: test that EEP-54 transformation is done (but don't check strings)
    try
        argparse:validate(#{commands => #{"one" => #{commands => #{"two" => atom}}}}),
        ?assert(false)
    catch
        error:R1:S1 ->
            #{1 := Cmd, reason := RR1, general := G} = argparse:format_error(R1, S1),
            ?assertEqual("command specification is invalid", unicode:characters_to_list(G)),
            ?assertEqual("command \"" ++ prog() ++ " one two\": invalid field 'commands', reason: expected command()",
                         unicode:characters_to_list(RR1)),
            ?assertEqual(["atom"], Cmd)
    end,
    %% check argument
    try
        argparse:validate(#{arguments => [#{}]}),
        ?assert(false)
    catch
        error:R2:S2 ->
            #{1 := Cmd2, reason := RR2, general := G2} = argparse:format_error(R2, S2),
            ?assertEqual("argument specification is invalid", unicode:characters_to_list(G2)),
            ?assertEqual("command \"" ++ prog() ++
                         "\", argument '', invalid field 'name': argument must be a map containing 'name' field",
                         unicode:characters_to_list(RR2)),
            ?assertEqual(["#{}"], Cmd2)
    end.

run_handle_test() ->
    %% no subcommand, basic fun handler with argmap
    ?assertEqual(6, argparse:run(["-i", "3"], #{handler => fun(#{in := Val}) -> Val * 2 end,
                                 arguments => [#{name => in, short => $i, type => integer}]}, #{})),
    %% subcommand, positional fun() handler
    ?assertEqual(6, argparse:run(["mul", "2", "3"],
                                 #{commands => #{"mul" => #{handler => {fun(match, L, R) -> L * R end, match},
                                                            arguments => [#{name => opt, short => $o},
                                                                          #{name => l, type => integer},
                                                                          #{name => r, type => integer}]}}},
                                 #{})),
    %% no subcommand, positional module-based function
    ?assertEqual(6, argparse:run(["2", "3"],
                                 #{handler => {erlang, '*', undefined},
                                   arguments => [#{name => l, type => integer}, #{name => r, type => integer}]},
                                 #{})),
    %% subcommand, module-based function accepting argmap
    ?assertEqual([{arg, "arg"}],
                 argparse:run(["map", "arg"],
                              #{commands => #{"map" => #{handler => {maps, to_list}, arguments => [#{name => arg}]}}},
                              #{})).

prog() -> {ok, [[ProgStr]]} = init:get_argument(progname), ProgStr.

parser_error(CmdLine, CmdMap) ->
    {error, Reason} = parse(CmdLine, CmdMap),
    unicode:characters_to_list(argparse:format_error(Reason)).

parse(Args, Command) -> argparse:parse(string:lexemes(Args, " "), Command).

parse_opts(Args, Opts) -> parse(Args, #{arguments => Opts}).

parse_cmd(Args, Command) -> parse(Args, #{commands => Command}).

%% ubiquitous command, containing sub-commands, and all possible option types
%% with all nargs. Not all combinations though.
ubiq_cmd() ->
    #{arguments => [#{name => r, short => $r, type => boolean, help => "recursive"},
                    #{name => f, short => $f, type => boolean, long => "-force", help => "force"},
                    #{name => v, short => $v, type => boolean, action => count, help => "verbosity level"},
                    #{name => interval, short => $i, type => {integer, [{min, 1}]}, help => "interval set"},
                    #{name => weird, long => "-req", help => "required optional, right?"},
                    #{name => float, long => "-float", type => float, default => 3.14,
                      help => "floating-point long form argument"}],
      commands => #{"start" => #{help => "verifies configuration and starts server",
                                 arguments => [#{name => server, help => "server to start"},
                                               #{name => shard, short => $s, type => integer, nargs => nonempty_list,
                                                 help => "initial shards"},
                                               #{name => part, short => $p, type => integer, nargs => list,
                                                 help => hidden},
                                               #{name => z, short => $z, type => {integer, [{min, 1}, {max, 10}]},
                                                 help => "between"},
                                               #{name => l, short => $l, type => {integer, [{max, 10}]},
                                                 nargs => 'maybe', help => "maybe lower"},
                                               #{name => more, short => $m, type => {integer, [{max, 10}]},
                                                 help => "less than 10"},
                                               #{name => optpos, required => false, type => {integer, []},
                                                 help => "optional positional"},
                                               #{name => bin, short => $b, type => {binary, <<"m">>},
                                                 help => "binary with re"},
                                               #{name => g, short => $g, type => {binary, <<"m">>, []},
                                                 help => "binary with re"},
                                               #{name => t, short => $t, type => {string, "m"},
                                                 help => "string with re"},
                                               #{name => e, long => "--maybe-req", required => true, type => integer,
                                                 nargs => 'maybe', help => "maybe required int"},
                                               #{name => y, required => true, long => "-yyy", short => $y,
                                                 type => {string, "m", []}, help => "string with re"},
                                               #{name => u, short => $u, type => {string, ["1", "2"]},
                                                 help => "string choices"},
                                               #{name => choice, short => $c, type => {integer, [1,2,3]},
                                                 help => "tough choice"},
                                               #{name => fc, short => $q, type => {float, [2.1,1.2]},
                                                 help => "floating choice"},
                                               #{name => ac, short => $w, type => {atom, [one, two]},
                                                 help => "atom choice"},
                                               #{name => au, long => "-unsafe", type => {atom, unsafe},
                                                 help => "unsafe atom"},
                                               #{name => as, long => "-safe", type => atom, help => <<"safe atom">>},
                                               #{name => name, required => false, nargs => list, help => hidden},
                                               #{name => long, long => "foobar", required => false,
                                                 help => [<<"foobaring option">>]}],
                                 commands => #{"crawler" => #{arguments => [#{name => extra, long => "--extra",
                                                                              help => "extra option very deep"}],
                                                              help => "controls crawler behaviour"},
                                                "doze" => #{help => "dozes a bit"}}},
                    "stop" => #{help => <<"stops running server">>, arguments => []},
                    "status" => #{help => "prints server status", arguments => [],
                                  commands => #{"crawler" => #{arguments => [#{name => extra, long => "--extra",
                                                                               help => "extra option very deep"}],
                                                               help => "crawler status"}}},
                    "restart" => #{help => hidden,
                                   arguments => [#{name => server, help => "server to restart"},
                                                 #{name => duo, short => $d, long => "-duo", help => "dual option"}]}}}.
