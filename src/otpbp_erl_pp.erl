-module(otpbp_erl_pp).

-ifndef(HAVE_erl_pp__legalize_vars_1).
% OTP 25.0
-export([legalize_vars/1]).
-endif.

-ifndef(HAVE_erl_pp__legalize_vars_1).
legalize_vars({function,ANNO,Name0,Arity,Clauses}) ->
    %% Collect all used variables in this function and classify them as either syntactically valid or not.
    {Valid, Invalid} = fold_vars(fun({var, _Anno, Name}, {Valid, Invalid}) ->
                                     case atom_to_list(Name) of
                                         [X|_] = Str when X >= $a, X =< $z -> {Valid, Invalid#{Name => Str}};
                                         _ -> {Valid#{Name => Name}, Invalid}
                                     end
                                 end,
                                 {#{}, #{}}, Clauses),
    %% Make up an unique variable name for each key in Invalid, then replace all invalid names.
    {function, ANNO, Name0, Arity,
     map_vars(fun({var, Anno, Name}) -> {var, Anno, maps:get(Name, maps:fold(fun legalize_name/3, Valid, Invalid))} end,
              Clauses)};
legalize_vars(Form) -> error(badarg, [Form]).

legalize_name(InvalidName, StringName, Used) ->
    NewName = list_to_atom(string:to_upper(StringName)),
    case Used of
        #{NewName := _} -> legalize_name(InvalidName, [$X|StringName], Used);
        #{} -> Used#{InvalidName => NewName}
    end.

fold_vars(F, Acc0, Forms) when is_list(Forms) -> lists:foldl(fun(Elem, Acc) -> fold_vars(F, Acc, Elem) end, Acc0, Forms);
fold_vars(F, Acc0, {var, _, _} = V) -> F(V, Acc0);
fold_vars(F, Acc0, Form) when is_tuple(Form) ->
    lists:foldl(fun(Elem, Acc) -> fold_vars(F, Acc, Elem) end, Acc0, tuple_to_list(Form));
fold_vars(_, Acc, _) -> Acc.

map_vars(F, Forms) when is_list(Forms) -> [map_vars(F, Form) || Form <- Forms];
map_vars(F, {var, _, _} = V) -> F(V);
map_vars(F, Form) when is_tuple(Form) -> list_to_tuple([map_vars(F, Elem) || Elem <- tuple_to_list(Form)]);
map_vars(_, Form) -> Form.
-endif.
