-module(otpbp_persistent_term).

-ifndef(HAVE_persistent_term__put_new_2).
% OTP >= 29.0
-ifdef(HAVE_persistent_term__get_1).
-ifdef(HAVE_persistent_term__put_2).
-export([put_new/2]).
-endif.
-endif.
-endif.

-ifndef(HAVE_persistent_term__put_new_2).
-ifdef(HAVE_persistent_term__get_1).
-ifdef(HAVE_persistent_term__put_2).
put_new(Key, Value) ->
    try persistent_term:get(Key) of
        Value -> ok;
        _ -> error(badarg)
    catch
        error:badarg -> persistent_term:put(Key, Value)
    end.
-endif.
-endif.
-endif.
