-module(otpbp_orddict).

-ifndef(HAVE_orddict__take_2).
% OTP 20.0
-export([take/2]).
-endif.
-ifndef(HAVE_orddict__foreach_2).
-export([foreach/2]).
-endif.

-ifndef(HAVE_orddict__take_2).
take(Key, Dict) -> take(Key, Dict, []).

take(Key, [{K, _}|_], _Acc) when Key < K -> error;
take(Key, [{K, _} = P|D], Acc) when Key > K -> take(Key, D, [P|Acc]);
take(_Key, [{_K, Value}|D], Acc) -> {Value, lists:reverse(Acc, D)};
take(_, [], _) -> error.
-endif.

-ifndef(HAVE_orddict__foreach_2).
foreach(F, D) when is_function(F, 2) -> lists:foreach(fun({K, V}) -> F(K, V) end, orddict:to_list(D)).
-endif.
