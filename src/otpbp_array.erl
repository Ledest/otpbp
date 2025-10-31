-module(otpbp_array).

-ifndef(HAVE_array__from_2).
% OTP 29.0
-export([from/2]).
-endif.
-ifndef(HAVE_array__from_3).
% OTP 29.0
-export([from/3]).
-endif.

-ifdef(HAVE_array__from_3).
-import(array, [from/3]).
-endif.

-ifndef(HAVE_array__from_2).
from(Fun, State) -> from(Fun, State, undefined).
-endif.

-ifndef(HAVE_array__from_3).
-define(LEAFSIZE, 10).
-type element_tuple() :: {_, _, _, _, _, _, _, _, _, _} |
                         {element_tuple(), element_tuple(), element_tuple(), element_tuple(), element_tuple(),
                          element_tuple(), element_tuple(), element_tuple(), element_tuple(), element_tuple(),
                          non_neg_integer()}.
-type elements() :: non_neg_integer() | element_tuple() | nil().
-record(array, {size :: non_neg_integer(), max :: non_neg_integer(), default, elements :: elements()}).

from(Fun, S0, Default) ->
    is_function(Fun, 1) orelse error(badarg),
    {E, N, M} = from(?LEAFSIZE, Default, Fun, Fun(S0), 0, [], []),
    #array{size = N, max = M, default = Default, elements = E}.

from(0, D, Fun, VS, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    if
        VS =:= done ->
            if
                Es =:= [] -> {E, N, ?LEAFSIZE};
                true -> from_list(N, [E|Es], ?LEAFSIZE)
            end;
        true -> from(?LEAFSIZE, D, Fun, VS, N, [], [E|Es])
    end;
from(I, D, Fun, done, N, As, Es) -> from(I - 1, D, Fun, done, N, [D|As], Es);
from(I, D, Fun, {X, S}, N, As, Es) -> from(I - 1, D, Fun, Fun(S), N + 1, [X|As], Es);
from(_I, _D, _Fun, _VS, _N, _As, _Es) -> error(badarg).

-define(NODESIZE, ?LEAFSIZE).
-define(extend(X), ((X) * (?NODESIZE))).

from_list(N, Es, S) -> from_list(?NODESIZE, pad((N - 1) div S + 1, ?NODESIZE, S, Es), S, N, [S], []).

from_list(0, Xs, S, N, As, Es) -> from_list(list_to_tuple(As), Xs, S, N, Es);
from_list(I, [X|Xs], S, N, As, Es) -> from_list(I - 1, Xs, S, N, [X|As], Es).

-compile({inline, from_list/5}).
from_list(E, [], S, N, []) -> {E, N, ?extend(S)};
from_list(E, [], S, N, Es) -> from_list(N, lists:reverse(Es, [E]), ?extend(S));
from_list(E, Xs, S, N, Es) -> from_list(?NODESIZE, Xs, S, N, [S], [E|Es]).

-compile({inline, pad/4}).
pad(N, K, P, Es) -> push((K - (N rem K)) rem K, P, Es).

push(0, _E, L) -> L;
push(N, E, L) -> push(N - 1, E, [E|L]).
-endif.
