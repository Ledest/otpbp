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
-ifndef(HAVE_lists__enumerate_3).
% OTP 26.0
-export([enumerate/3]).
-endif.
-ifndef(HAVE_lists__uniq_1).
% OTP 25.0
-export([uniq/1]).
-endif.
-ifndef(HAVE_lists__uniq_2).
% OTP 25.0
-export([uniq/2]).
-endif.
-ifndef(HAVE_lists__zip_3).
% OTP 26.0
-export([zip/3]).
-endif.
-ifndef(HAVE_lists__zip3_4).
% OTP 26.0
-export([zip3/4]).
-endif.
-ifndef(HAVE_lists__zipwith_4).
% OTP 26.0
-export([zipwith/4]).
-endif.
-ifndef(HAVE_lists__zipwith3_5).
% OTP 26.0
-export([zipwith3/5]).
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
enumerate(List) ->
    {L, _} = lists:mapfoldl(fun(E, A) -> {{A, E}, A + 1} end, 1, List),
    L.
-endif.

-ifndef(HAVE_lists__enumerate_2).
enumerate(Index, List) when is_integer(Index) ->
    {L, _} = lists:mapfoldl(fun(E, A) -> {{A, E}, A + 1} end, Index, List),
    L.
-endif.

-ifndef(HAVE_lists__enumerate_3).
enumerate(Index, Step, List) when is_integer(Index), is_integer(Step) ->
    {L, _} = lists:mapfoldl(fun(E, A) -> {{A, E}, A + Step} end, Index, List),
    L.
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

-ifndef(HAVE_lists__zip_3).
zip(Xs, Ys, fail) -> lists:zip(Xs, Ys);
zip(Xs, Ys, trim) -> zip(Xs, Ys);
zip(Xs, Ys, {pad, {X, Y}}) -> zip(Xs, Ys, X, Y).

zip([X|Xs], [Y|Ys]) -> [{X, Y}|zip(Xs, Ys)];
zip(Xs, Ys) when is_list(Xs), is_list(Ys) -> [].

zip([X|Xs], [Y|Ys], PX, PY) -> [{X, Y}|zip(Xs, Ys, PX, PY)];
zip([], [], _, _) -> [];
zip([], [_|_] = Ys, PX, _) -> [{PX, Y} || Y <- Ys];
zip([_|_] = Xs, [], _, PY) -> [{X, PY} || X <- Xs].
-endif.

-ifndef(HAVE_lists__zip3_4).
zip3(Xs, Ys, Zs, fail) -> lists:zip3(Xs, Ys, Zs);
zip3(Xs, Ys, Zs, trim) -> zip3(Xs, Ys, Zs);
zip3(Xs, Ys, Zs, {pad, {X, Y, Z}}) -> zip3(Xs, Ys, Zs, X, Y, Z).

zip3([X|Xs], [Y|Ys], [Z|Zs]) -> [{X, Y, Z}|zip3(Xs, Ys, Zs)];
zip3(Xs, Ys, Zs) when is_list(Xs), is_list(Ys), is_list(Zs) -> [].

zip3([X|Xs], [Y|Ys], [Z|Zs], PX, PY, PZ) -> [{X, Y, Z}|zip3(Xs, Ys, Zs, PX, PY, PZ)];
zip3([], [], [], _, _, _) -> [];
zip3([], [], [_|_] = Zs, PX, PY, _) -> [{PX, PY, Z} || Z <- Zs];
zip3([], [_|_] = Ys, [], PX, _, PZ) -> [{PX, Y, PZ} || Y <- Ys];
zip3([_|_] = Xs, [], [], _, PY, PZ) -> [{X, PY, PZ} || X <- Xs];
zip3([], [Y|Ys], [Z|Zs], PX, PY, PZ) -> [{PX, Y, Z}|zip3([], Ys, Zs, PX, PY, PZ)];
zip3([X|Xs], [], [Z|Zs], PX, PY, PZ) -> [{X, PY, Z}|zip3(Xs, [], Zs, PX, PY, PZ)];
zip3([X|Xs], [Y|Ys], [], PX, PY, PZ) -> [{X, Y, PZ}|zip3(Xs, Ys, [], PX, PY, PZ)].
-endif.

-ifndef(HAVE_lists__zipwith_4).
zipwith(F, Xs, Ys, fail) -> lists:zipwith(F, Xs, Ys);
zipwith(F, Xs, Ys, trim) -> zipwith(F, Xs, Ys);
zipwith(F, Xs, Ys, {pad, {X, Y}}) -> zipwith(F, Xs, Ys, X, Y).

zipwith(F, [X|Xs], [Y|Ys]) -> [F(X, Y)|zipwith(F, Xs, Ys)];
zipwith(F, Xs, Ys) when is_function(F, 2), is_list(Xs), is_list(Ys) -> [].

zipwith(F, [X|Xs], [Y|Ys], PX, PY) -> [F(X, Y)|zipwith(F, Xs, Ys, PX, PY)];
zipwith(F, [], [], _, _) when is_function(F, 2) -> [];
zipwith(F, [], [_|_] = Ys, X, _) -> [F(X, Y) || Y <- Ys];
zipwith(F, [_|_] = Xs, [], _, Y) -> [F(X, Y) || X <- Xs].
-endif.

-ifndef(HAVE_lists__zipwith3_5).
zipwith3(F, Xs, Ys, Zs, fail) -> lists:zipwith3(F, Xs, Ys, Zs);
zipwith3(F, Xs, Ys, Zs, trim) -> zipwith3(F, Xs, Ys, Zs);
zipwith3(F, Xs, Ys, Zs, {pad, {X, Y, Z}}) -> zipwith3(F, Xs, Ys, Zs, X, Y, Z).

zipwith3(F, [X|Xs], [Y|Ys], [Z|Zs]) -> [F(X, Y, Z)|zipwith3(F, Xs, Ys, Zs)];
zipwith3(F, Xs, Ys, Zs) when is_function(F, 3), is_list(Xs), is_list(Ys), is_list(Zs) -> [].

zipwith3(F, [X|Xs], [Y|Ys], [Z|Zs], PX, PY, PZ) -> [F(X, Y, Z)|zipwith3(F, Xs, Ys, Zs, PX, PY, PZ)];
zipwith3(F, [], [], [], _, _, _) when is_function(F, 3) -> [];
zipwith3(F, [], [], [_|_] = Zs, X, Y, _) -> [F(X, Y, Z) || Z <- Zs];
zipwith3(F, [], [_|_] = Ys, [], X, _, Z) -> [F(X, Y, Z) || Y <- Ys];
zipwith3(F, [_|_] = Xs, [], [], _, Y, Z) -> [F(X, Y, Z) || X <- Xs];
zipwith3(F, [], [Y|Ys], [Z|Zs], PX, PY, PZ) -> [F(PX, Y, Z)|zipwith3(F, [], Ys, Zs, PX, PY, PZ)];
zipwith3(F, [X|Xs], [], [Z|Zs], PX, PY, PZ) -> [F(X, PY, Z)|zipwith3(F, Xs, [], Zs, PX, PY, PZ)];
zipwith3(F, [X|Xs], [Y|Ys], [], PX, PY, PZ) -> [F(X, Y, PZ)|zipwith3(F, Xs, Ys, [], PX, PY, PZ)].
-endif.
