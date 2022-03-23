-module(otpbp_lists).

-ifndef(HAVE_lists__join_2).
% OTP 19.0
-export([join/2]).
-endif.
-ifndef(HAVE_lists__search_2).
% OTP 21.0
-export([search/2]).
-endif.
-ifndef(HAVE_lists__enumerate_1).
% OTP 25.0
-export([enumerate/1]).
-endif.
-ifndef(HAVE_lists__enumerate_2).
% OTP 25.0
-export([enumerate/2]).
-endif.
-ifndef(HAVE_lists__uniq_1).
% OTP 25.0
-export([uniq/1]).
-endif.
-ifndef(HAVE_lists__uniq_2).
% OTP 25.0
-export([uniq/2]).
-endif.

-ifndef(HAVE_lists__enumerate_1).
-ifdef(HAVE_lists__enumerate_2).
-import(lists, [enumerate/2]).
-endif.
-endif.

-ifndef(HAVE_lists__join_2).
join(_, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep, H|join_prepend(Sep, T)].
-endif.

-ifndef(HAVE_lists__search_2).
search(F, [H|T]) ->
    case F(H) of
        true -> {value, H};
        false -> search(F, T)
    end;
search(F, []) when is_function(F, 1) -> false.
-endif.

-ifndef(HAVE_lists__enumerate_1).
enumerate(List) -> enumerate(1, List).
-endif.

-ifndef(HAVE_lists__enumerate_2).
enumerate(Index, [H|T]) when is_integer(Index) -> [{Index, H}|enumerate(Index + 1, T)];
enumerate(Index, []) when is_integer(Index) -> [].
-endif.

-ifndef(HAVE_lists__uniq_1).
uniq(L) -> uniq_1(L, #{}).

uniq_1([X|Xs], M) ->
    case maps:is_key(X, M) of
        true -> uniq_1(Xs, M);
        false -> [X|uniq_1(Xs, M#{X => true})]
    end;
uniq_1([], _) -> [].
-endif.

-ifndef(HAVE_lists__uniq_2).
uniq(F, L) when is_function(F, 1) -> uniq_2(L, F, #{}).

uniq_2([X|Xs], F, M) ->
    Key = F(X),
    case maps:is_key(Key, M) of
        true -> uniq_2(Xs, F, M);
        false -> [X|uniq_2(Xs, F, M#{Key => true})]
    end;
uniq_2([], _, _) -> [].
-endif.
