-module(otpbp_queue).

-ifndef(HAVE_queue__all_2).
% OTP 24.0 ?
-export([all/2]).
-endif.

-ifndef(HAVE_queue__any_2).
% OTP 24.0 ?
-export([any/2]).
-endif.

-ifndef(HAVE_queue__fold_3).
% OTP 24.0 ?
-export([fold/3]).
-endif.
-ifndef(HAVE_queue__filtermap_2).
% OTP 24.0 ?
-export([filtermap/2]).
-endif.

-ifndef(HAVE_queue__all_2).
all(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) -> lists:all(Pred, F) andalso lists:all(Pred, R);
all(Pred, Q) -> erlang:error(badarg, [Pred, Q]).
-endif.

-ifndef(HAVE_queue__any_2).
any(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) -> lists:any(Pred, F) orelse lists:any(Pred, R);
any(Pred, Q) -> erlang:error(badarg, [Pred, Q]).
-endif.

-ifndef(HAVE_queue__fold_3).
fold(Fun, Acc, {R, F}) when is_function(Fun, 2), is_list(R), is_list(F) -> lists:foldr(Fun, lists:foldl(Fun, Acc, F), R);
fold(Fun, Acc0, Q) -> erlang:error(badarg, [Fun, Acc0, Q]).
-endif.

-ifndef(HAVE_queue__filtermap_2).
filtermap(Fun, {R0, F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    case {filtermap_r(Fun, R0), lists:filtermap(Fun, F0)} of
        {[], F} -> f2r(F);
        {R, []} -> r2f(R);
        RF -> RF
    end;
filtermap(Fun, Q) -> erlang:error(badarg, [Fun,Q]).

%% Call Fun in reverse order, i.e tail to head
filtermap_r(Fun, [X|R0]) ->
    R = filtermap_r(Fun, R0),
    case Fun(X) of
        true -> [X|R];
        {true, Y} -> [Y|R];
        false -> R
    end;
filtermap_r(_, []) -> [].

-compile({inline, [{r2f,1},{f2r,1}]}).

%% Move half of elements from R to F, if there are at least three
r2f([]) -> {[], []};
r2f([_] = R) -> {[], R};
r2f([X, Y]) -> {[X], [Y]};
r2f(List) ->
    {FF, RR} = lists:split(length(List) div 2 + 1, List),
    {FF, lists:reverse(RR, [])}.

%% Move half of elements from F to R, if there are enough
f2r([]) -> {[], []};
f2r([_] = F) -> {F, []};
f2r([X, Y]) -> {[Y], [X]};
f2r(List) ->
    {FF, RR} = lists:split(length(List) div 2 + 1, List),
    {lists:reverse(RR, []), FF}.
-endif.
