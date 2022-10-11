-module(otpbp_ets).

-ifndef(HAVE_ets_lookup_element_4).
% OTP 26.0
-export([lookup_element/4]).
-endif.

-ifndef(HAVE_ets_lookup_element_4).
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
