-module(otpbp_erl_internal).

-ifndef(HAVE_erl_internal__add_predefined_functions_1).
% OTP 20.0
-export([add_predefined_functions/1]).
-endif.

-ifndef(HAVE_erl_internal__add_predefined_functions_1).
add_predefined_functions(Forms) -> Forms ++ predefined_functions(Forms).

-compile({inline, predefined_functions/1}).
predefined_functions(Forms) ->
    Attrs = [{Name, Val} || {attribute, _, Name, Val} <- Forms],
    {module, Mod} = lists:keyfind(module, 1, Attrs),
    Mpf = lists:map(fun erl_parse:new_anno/1, module_predef_func_beh_info(Attrs) ++ module_predef_funcs_mod_info(Mod)),
    [{attribute, 0, export, [{F, A} || {function, _, F, A, _} <- Mpf]}|Mpf].

-compile({inline, module_predef_func_beh_info/1}).
module_predef_func_beh_info(Attrs) ->
    case [Callback || {callback, Callback} <- Attrs] of
        [] -> [];
        Callbacks ->
            [{function, 0, behaviour_info, 1,
              [{clause, 0, [{atom, 0, callbacks}], [], [make_list([FA || {{_, _} = FA, _} <- Callbacks])]},
               {clause, 0, [{atom, 0, optional_callbacks}], [], [make_list(get_optional_callbacks(Attrs))]}]}]
    end.

-compile({inline, get_optional_callbacks/1}).
get_optional_callbacks(Attrs) ->
    lists:foldr(fun({optional_callbacks, O}, A) ->
                    case is_fa_list(O) of
                        true -> O ++ A;
                        _false -> A
                    end;
                   (_, A) -> A
                end, [], Attrs).

-compile({inline, is_fa_list/1}).
is_fa_list(L) ->
    is_list(L) andalso
        lists:all(fun({FuncName, Arity}) -> is_atom(FuncName) andalso is_integer(Arity) andalso Arity >= 0;
                     (_) -> false
                  end, L).

make_list(L) ->
    lists:foldr(fun({Name, Arity}, A) -> {cons, 0, {tuple, 0, [{atom, 0, Name}, {integer, 0, Arity}]}, A} end,
                {nil, 0}, L).

-compile({inline, module_predef_funcs_mod_info/1}).
module_predef_funcs_mod_info(Mod) ->
    ModAtom = {atom, 0, Mod},
    [{function, 0, module_info, 0,
      [{clause, 0, [], [], [{call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, get_module_info}}, [ModAtom]}]}]},
     {function, 0, module_info, 1,
      [{clause, 0, [{var, 0, 'X'}], [],
        [{call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, get_module_info}}, [ModAtom, {var, 0, 'X'}]}]}]}].
-endif.
