-module(otpbp_dict).

-ifndef(HAVE_dict__take_2).
% OTP 20.0
-export([take/2]).
-endif.
-ifndef(HAVE_dict__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_dict__take_2).
take(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, V} -> {V, dict:erase(Key, Dict)};
        error -> error
    end.
-endif.

-ifndef(HAVE_dict__foreach_2).
foreach(F, D) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, dict:to_list(D));
foreach(F, D) -> error(badarg, [F, D]).
-endif.
