-module(otpbp_orddict).

-ifndef(HAVE_orddict__is_empty_1).
-export([is_empty/1]).
-endif.

-ifndef(HAVE_orddict__take_2).
-export([take/2]).
-endif.

-ifndef(HAVE_orddict__is_empty_1).
is_empty(Dict) -> orddict:size(Dict) =:= 0.
-endif.

-ifndef(HAVE_orddict__take_2).
take(Key, Dict) -> take(Key, Dict, []).

take(Key, [{K, _}|_], _Acc) when Key < K -> error;
take(Key, [{K, _} = P|D], Acc) when Key > K -> take(Key, D, [P|Acc]);
take(_Key, [{_K, Value}|D], Acc) -> {Value, lists:reverse(Acc, D)};
take(_, [], _) -> error.
-endif.
