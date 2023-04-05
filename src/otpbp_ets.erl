-module(otpbp_ets).

-ifndef(HAVE_ets__whereis_1).
% OTP 21.0
-export([whereis/1]).
-endif.
-ifndef(HAVE_ets__lookup_element_4).
% OTP 26.0
-export([lookup_element/4]).
-endif.

-ifndef(HAVE_ets__whereis_1).
whereis(T) ->
    is_atom(T) orelse error(badarg, [T]),
    case ets:info(T, named_table) of
        true -> ets:info(T, name);
        _ -> undefined
    end.
-endif.

-ifndef(HAVE_ets__lookup_element_4).
lookup_element(Table, Key, Pos, Default) ->
    case ets:lookup(Table, Key) of
        [Object|_] = Objects ->
            case ets:info(Table, type) of
                T when T =:= bag; T =:= duplicate_bag ->
                    lists:map(fun(O) when tuple_size(O) >= Pos -> element(Pos, O);
                                 (_) -> error(badarg, [Table, Key, Pos, Default])
                              end, Objects);
                _ when tuple_size(Object) >= Pos -> element(Pos, Object);
                _ -> error(badarg, [Table, Key, Pos, Default])
            end;
        _ -> Default
    end.
-endif.
