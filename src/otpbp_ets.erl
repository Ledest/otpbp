-module(otpbp_ets).

-ifndef(HAVE_ets__update_counter_4).
-export([update_counter/4]).
-endif.

-ifndef(HAVE_ets__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_ets__update_counter_4).
update_counter(Tab, Key, Incr, Default) when is_tuple(Default) ->
    ets:insert_new(Tab, Default),
    ets:update_counter(Tab, Key, Incr).
-endif.

-ifndef(HAVE_ets__take_2).
take(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] -> [];
        Objects ->
            ets:delete(Tab, Key),
            Objects
    end.
-endif.
