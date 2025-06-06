-module(otpbp_ets).

-ifndef(HAVE_ets__lookup_element_4).
% OTP 26.0
-export([lookup_element/4]).
-endif.
-ifndef(HAVE_ets__first_lookup_1).
% OTP 27.0
-export([first_lookup/1]).
-endif.
-ifndef(HAVE_ets__last_lookup_1).
% OTP 27.0
-export([last_lookup/1]).
-endif.
-ifndef(HAVE_ets__next_lookup_2).
% OTP 27.0
-export([next_lookup/2]).
-endif.
-ifndef(HAVE_ets__prev_lookup_2).
% OTP 27.0
-export([prev_lookup/2]).
-endif.

-ifndef(HAVE_ets__lookup_element_4).
lookup_element(Table, Key, Pos, Default) ->
    case ets:lookup(Table, Key) of
        [Object|_] = Objects ->
            F = fun(O) when tuple_size(O) >= Pos -> element(Pos, O);
                   (_) -> error(badarg, [Table, Key, Pos, Default])
                end,
            case ets:info(Table, type) of
                T when T =:= bag; T =:= duplicate_bag -> lists:map(F, Objects);
                _ -> F(Object)
            end;
        _ -> Default
    end.
-endif.

-ifndef(HAVE_ets__first_lookup_1).
first_lookup(Tab) ->
    case ets:first(Tab) of
        '$end_of_table' -> '$end_of_table';
        Key -> ets:lookup(Tab, Key)
    end.
-endif.

-ifndef(HAVE_ets__last_lookup_1).
last_lookup(Tab) ->
    case ets:last(Tab) of
        '$end_of_table' -> '$end_of_table';
        Key -> ets:lookup(Tab, Key)
    end.
-endif.

-ifndef(HAVE_ets__next_lookup_2).
next_lookup(Tab, Key) ->
    case ets:next(Tab, Key) of
        '$end_of_table' -> '$end_of_table';
        KeyN -> ets:lookup(Tab, KeyN)
    end.
-endif.

-ifndef(HAVE_ets__prev_lookup_2).
prev_lookup(Tab, Key) ->
    case ets:prev(Tab, Key) of
        '$end_of_table' -> '$end_of_table';
        KeyN -> ets:lookup(Tab, KeyN)
    end.
-endif.
