-module(otpbp_logger).

-ifndef(HAVE_logger__set_application_level_2).
% OTP 21.1
-ifdef(HAVE_logger__set_module_level_2).
-export([set_application_level/2]).
-endif.
-endif.
-ifndef(HAVE_logger__unset_application_level_1).
% OTP 21.1
-ifdef(HAVE_logger__unset_module_level_1).
-export([unset_application_level/1]).
-endif.
-endif.
-ifndef(HAVE_logger__i_0).
% OTP 21.3
-ifdef(HAVE_logger__get_config_0).
-export([i/0]).
-endif.
-endif.
-ifndef(HAVE_logger__i_1).
% OTP 21.3
-ifdef(HAVE_logger__get_handler_config_0).
-export([i/1]).
-endif.
-endif.
-ifndef(HAVE_logger__timestamp_0).
% OTP 21.3
-export([timestamp/0]).
-endif.
-ifndef(HAVE_logger__reconfigure_0).
% OTP 24.2
-ifdef(HAVE_logger__get_handler_config_0).
-export([reconfigure/0]).
-endif.
-endif.

-ifndef(HAVE_logger__set_application_level_2).
-ifdef(HAVE_logger__set_module_level_2).
set_application_level(App, Level) ->
    case application:get_key(App, modules) of
        {ok, Modules} -> logger:set_module_level(Modules, Level);
        undefined -> {error, {not_loaded, App}}
    end.
-endif.
-endif.

-ifndef(HAVE_logger__unset_application_level_1).
-ifdef(HAVE_logger__unset_module_level_1).
unset_application_level(App) ->
    case application:get_key(App, modules) of
        {ok, Modules} -> logger:unset_module_level(Modules);
        undefined -> {error, {not_loaded, App}}
    end.
-endif.
-endif.

-ifndef(HAVE_logger__i_0).
-ifdef(HAVE_logger__get_config_0).
i() ->
    #{primary := Primary, handlers := HandlerConfigs, module_levels := Modules} = logger:get_config(),
    M = modifier(),
    i_primary(Primary, M),
    i_handlers(HandlerConfigs, M),
    i_modules(Modules, M).

-ifndef(NEED_modifier_0).
-define(NEED_modifier_0, true).
-endif.
-endif.
-endif.

-ifndef(HAVE_logger__i_1).
-ifdef(HAVE_logger__get_handler_config_0).
i(primary) -> i_primary(logger:get_primary_config(), modifier());
i(handlers) -> i_handlers(logger:get_handler_config(), modifier());
i(proxy) -> ok;
i(modules) -> i_modules(logger:get_module_level(), modifier());
i(HandlerId) when is_atom(HandlerId) ->
    case logger:get_handler_config(HandlerId) of
        {ok, HandlerConfig} -> i_handlers([HandlerConfig], modifier());
        Error -> Error
    end;
i(What) -> error(badarg, [What]).

-ifndef(NEED_modifier_0).
-define(NEED_modifier_0, true).
-endif.
-endif.
-endif.

-ifdef(NEED_modifier_0).
i_primary(#{level := Level, filters := Filters, filter_default := FilterDefault}, M) ->
    io:format("Primary configuration: ~n", []),
    io:format("    Level: ~p~n", [Level]),
    io:format("    Filter Default: ~p~n", [FilterDefault]),
    io:format("    Filters: ~n", []),
    print_filters("        ", Filters, M).

i_handlers(HandlerConfigs, M) ->
    io:format("Handler configuration: ~n", []),
    print_handlers(HandlerConfigs, M).

i_modules(Modules, M) ->
    io:format("Level set per module: ~n", []),
    print_module_levels(Modules,M).

encoding() ->
    case lists:keyfind(encoding, 1, io:getopts()) of
        {_encoding, Enc} -> Enc;
        _false -> latin1
    end.

modifier() -> modifier(encoding()).

modifier(latin1) -> "";
modifier(_) -> "t".

print_filters(Indent, {Id, {Fun, Arg}}, M) ->
    io:format("~sId: ~" ++ M ++ "p~n"
              "~s    Fun: ~" ++ M ++ "p~n"
              "~s    Arg: ~" ++ M ++ "p~n",
              [Indent, Id, Indent, Fun, Indent, Arg]);
print_filters(Indent, [], _M) -> io:format("~s(none)~n", [Indent]);
print_filters(Indent, Filters, M) -> lists:foreach(fun(Filter) -> print_filters(Indent, Filter, M) end, Filters).

print_handlers(#{id := Id, module := Module, level := Level, filters := Filters, filter_default := FilterDefault,
                 formatter := {FormatterModule, FormatterConfig}} = Config,
               M) ->
    io:format("    Id: ~" ++ M ++ "p~n"
              "        Module: ~p~n"
              "        Level:  ~p~n"
              "        Formatter:~n"
              "            Module: ~p~n"
              "            Config:~n",
              [Id, Module, Level, FormatterModule]),
    print_custom("                ", FormatterConfig, M),
    io:format("        Filter Default: ~p~n"
              "        Filters:~n",
              [FilterDefault]),
    print_filters("            ", Filters, M),
    case Config of
        #{config := HandlerConfig} ->
            io:format("        Handler Config:~n"),
            print_custom("            ", HandlerConfig, M);
        _ -> ok
    end,
    case maps:without([filter_default, filters, formatter, level, module, id, config], Config) of
        Empty when map_size(Empty) =:= 0 -> ok;
        Unhandled ->
            io:format("        Custom Config:~n"),
            print_custom("            ", Unhandled, M)
    end;
print_handlers([], _M) -> io:format("    (none)~n");
print_handlers(HandlerConfigs, M) ->
    lists:foreach(fun(HandlerConfig) -> print_handlers(HandlerConfig, M) end, HandlerConfigs).

print_custom(Indent, {Key, Value}, M) -> io:format("~s~" ++ M ++ "p: ~" ++ M ++ "p~n", [Indent, Key, Value]);
print_custom(Indent, Map, M) when is_map(Map) -> print_custom(Indent, lists:keysort(1, maps:to_list(Map)), M);
print_custom(Indent, List, M) when is_list(List), is_tuple(hd(List)) ->
    lists:foreach(fun(X) -> print_custom(Indent, X, M) end, List);
print_custom(Indent, Value, M) -> io:format("~s~" ++ M ++ "p~n", [Indent, Value]).

print_module_levels({Module,Level},M) ->
    io:format("    Module: ~" ++ M ++ "p~n"
              "        Level: ~p~n",
              [Module, Level]);
print_module_levels([],_M) -> io:format("    (none)~n");
print_module_levels(Modules,M) -> lists:foreach(fun(Module) -> print_module_levels(Module, M) end, Modules).
-endif.

-ifndef(HAVE_logger__timestamp_0).
timestamp() -> os:system_time(microsecond).
-endif.

-ifndef(HAVE_logger__reconfigure_0).
-ifdef(HAVE_logger__get_handler_config_0).
-define(DEFAULT_HANDLER_FILTERS, ?DEFAULT_HANDLER_FILTERS([otp])).
-define(DEFAULT_HANDLER_FILTERS(Domain),
        [{remote_gl, {fun logger_filters:remote_gl/2, stop}},
         {domain, {fun logger_filters:domain/2, {log, super, Domain}}},
         {no_domain, {fun logger_filters:domain/2, {log, undefined, []}}}]).

reconfigure() ->
    F = fun(#{id := Id}) ->
            case logger:remove_handler(Id) of
                ok -> ok;
                {error, Reason} -> throw({remove, Id, Reason})
            end;
           (_) -> ok
        end,
    try lists:foreach(F, logger:get_handler_config()),
        ok = logger:add_handler(simple, logger_simple_h, #{filter_default => stop, filters => ?DEFAULT_HANDLER_FILTERS}),
        logger:unset_module_level(),
        logger:internal_init_logger() of
        ok -> logger:add_handlers(kernel);
        Error -> Error
    catch
        throw:Reason -> {error, Reason}
    end.
-endif.
-endif.
