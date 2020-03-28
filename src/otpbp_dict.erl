-module(otpbp_dict).

-ifndef(HAVE_dict__take_2).
% OTP 20.0
-export([take/2]).
-endif.

-ifndef(HAVE_dict__take_2).
take(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, V} -> {V, dict:erase(Key, Dict)};
        error -> error
    end.
-endif.
