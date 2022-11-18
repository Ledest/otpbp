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

-ifndef(HAVE_lists__zip_3).
zip([X|Xs], [Y|Ys], How) -> [{X, Y}|zip(Xs, Ys, How)];
zip([], [], {pad, {_, _}}) -> [];
zip([], [], How) when How =:= fail; How =:= trim -> [];
zip([_|_], [], trim) -> [];
zip([], [_|_], trim) -> [];
zip([], [_|_] = Ys, {pad, {X, _}}) -> [{X, Y} || Y <- Ys];
zip([_|_] = Xs, [], {pad, {_, Y}}) -> [{X, Y} || X <- Xs].
-endif.

-ifndef(HAVE_lists__zip3_4).
zip3([X|Xs], [Y|Ys], [Z|Zs], How) -> [{X, Y, Z} | zip3(Xs, Ys, Zs, How)];
zip3([], [], [], How) when How =:= fail; How =:= trim -> [];
zip3(Xs, Ys, Zs, trim) when is_list(Xs), is_list(Ys), is_list(Zs) -> [];
zip3([], [], [], {pad, {_, _, _}}) -> [];
zip3([], [], [_|_] = Zs, {pad, {X, Y, _}}) -> [{X, Y, Z} || Z <- Zs];
zip3([], [_|_] = Ys, [], {pad, {X, _, Z}}) -> [{X, Y, Z} || Y <- Ys];
zip3([_|_] = Xs, [], [], {pad, {_, Y, Z}}) -> [{X, Y, Z} || X <- Xs];
zip3([], [Y|Ys], [Z|Zs], {pad, {X, _, _}} = How) -> [{X, Y, Z} | zip3([], Ys, Zs, How)];
zip3([X|Xs], [], [Z|Zs], {pad, {_, Y, _}} = How) -> [{X, Y, Z} | zip3(Xs, [], Zs, How)];
zip3([X|Xs], [Y|Ys], [], {pad, {_, _, Z}} = How) -> [{X, Y, Z} | zip3(Xs, Ys, [], How)].
-endif.

-ifndef(HAVE_lists__zipwith_4).
zipwith(F, [X|Xs], [Y|Ys], How) -> [F(X, Y)|zipwith(F, Xs, Ys, How)];
zipwith(F, [], [], How) when is_function(F, 2), How =:= fail orelse How =:= trim -> [];
zipwith(F, [], [], {pad, {_, _}}) when is_function(F, 2) -> [];
zipwith(F, [_|_], [], trim) when is_function(F, 2) -> [];
zipwith(F, [], [_|_], trim) when is_function(F, 2) -> [];
zipwith(F, [], [_|_] = Ys, {pad, {X, _}}) -> [F(X, Y) || Y <- Ys];
zipwith(F, [_|_] = Xs, [], {pad, {_, Y}}) -> [F(X, Y) || X <- Xs].
-endif.

-ifndef(HAVE_lists__zipwith3_5).
zipwith3(F, [X|Xs], [Y|Ys], [Z|Zs], How) -> [F(X, Y, Z)|zipwith3(F, Xs, Ys, Zs, How)];
zipwith3(F, [], [], [], How) when is_function(F, 3), How =:= fail orelse How =:= trim -> [];
zipwith3(F, Xs, Ys, Zs, trim) when is_function(F, 3), is_list(Xs), is_list(Ys), is_list(Zs) -> [];
zipwith3(F, [], [], [], {pad, {_, _, _}}) when is_function(F, 3) -> [];
zipwith3(F, [], [], [_|_] = Zs, {pad, {X, Y, _}}) -> [F(X, Y, Z) || Z <- Zs];
zipwith3(F, [], [_|_] = Ys, [], {pad, {X, _, Z}}) -> [F(X, Y, Z) || Y <- Ys];
zipwith3(F, [_|_] = Xs, [], [], {pad, {_, Y, Z}}) -> [F(X, Y, Z) || X <- Xs];
zipwith3(F, [], [Y|Ys], [Z|Zs], {pad, {X, _, _}} = How) -> [F(X, Y, Z) | zipwith3(F, [], Ys, Zs, How)];
zipwith3(F, [X|Xs], [], [Z|Zs], {pad, {_, Y, _}} = How) -> [F(X, Y, Z) | zipwith3(F, Xs, [], Zs, How)];
zipwith3(F, [X|Xs], [Y|Ys], [], {pad, {_, _, Z}} = How) -> [F(X, Y, Z) | zipwith3(F, Xs, Ys, [], How)].
-endif.
