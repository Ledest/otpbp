-module(otpbp_dict).

-ifndef(HAVE_dict__is_empty_1).
-export([is_empty/1]).
-endif.

-ifndef(HAVE_dict__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_dict__is_empty_1).
is_empty(Dict) -> dict:size(Dict) =:= 0.
-endif.

-ifndef(HAVE_dict__take_2).
take(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, V} -> {V, dict:erase(Key, Dict)};
        error -> error
    end.
-endif.
