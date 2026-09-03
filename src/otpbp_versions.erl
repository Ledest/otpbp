-module(otpbp_versions).

-ifndef(HAVE_versions__branch_1).
-export([branch/1,
         branch_base/1,
         check/1,
         compare/2,
         list_check/1,
         list_compare/2,
         list_to_string/1,
         string_to_list/1]).

branch(V) ->
    try
        case vs2vl(V) of
            [_, _] -> <<"0.">>;
            [_, _, _] -> <<"0.">>;
            VL -> unicode:characters_to_binary([vl2vs(lists:reverse(tl(lists:reverse(VL))), true), $.])
        end
    catch
        _:_ -> error(badarg, [V])
    end.

branch_base(B) ->
    try
        case bsplit2bl(split(B), 0, []) of
            [0] -> <<"0.0">>;
            RBL ->
                vl2vs(case lists:reverse(drop_zeros(RBL)) of
                          [X] -> [X, 0];
                          Xs -> Xs
                      end,
                      false)
        end
    catch
        _:_ -> error(badarg, [B])
    end.

check(V) ->
    try vs2vl(V) of
        _ -> true
    catch
        _:_ -> false
    end.

compare(V1, V2) ->
    try
        cmp(vs2vl(V1), vs2vl(V2))
    catch
        _:_ -> error(badarg, [V1, V2])
    end.

list_check(VL) ->
    try chk_vl(VL) of
        _ -> true
    catch
        _:_ -> false
    end.

list_compare(VL1, VL2) ->
    try
        cmp(chk_vl(VL1), chk_vl(VL2))
    catch
        _:_ -> error(badarg, [VL1, VL2])
    end.

list_to_string(VL) ->
    try
        vl2vs(VL, false)
    catch
        _:_ -> error(badarg, [VL])
    end.

string_to_list(V) ->
    try
        vs2vl(V)
    catch
        _:_ -> error(badarg, [V])
    end.

%%%
%%% Internal helper functions
%%%

bsplit2bl([X], N, RXs) ->
    true = string:is_empty(X),
    if
        N == 1 -> [0] = RXs;
        N == 2 -> error(invalid_branch_id);
        true -> RXs
    end;
bsplit2bl([X|Xs], N, RXs) ->
    I = str2int(X),
    true = I >= 0,
    bsplit2bl(Xs, N + 1, [I|RXs]).

vs2vl(V) ->
    case lists:map(fun str2int/1, split(V)) of
        [X, Y] = VL when X >= 0, Y >= 0 -> VL;
        [X, Y, Z] = VL when X >= 0, Y >= 0, Z > 0 -> VL;
        [X, Y, Z|[_|_] = Vs] = VL when X >= 0, Y >= 0, Z >= 0 ->
            chk_i(Vs),
            VL
    end.

str2int(S) ->
    {I, R} = string:to_integer(case unicode:characters_to_binary(S) of
                                   <<"0"/utf8, _/utf8, _/binary>> -> error(leading_zero_in_integer);
                                   BS -> BS
                               end),
    true = string:is_empty(R),
    I.

chk_i([]) -> ok;
chk_i([0]) -> error(trailing_zero);
chk_i([X|Xs]) when X >= 0 -> chk_i(Xs).

cmp(V1, V2) -> cmp(V1, V2, 1).

cmp([XY|Xs], [XY|Ys], N) -> cmp(Xs, Ys, N+1);
cmp([], [], _N) -> same;
cmp([], [_Y|_Ys], _N) -> ancestor;
cmp([_X|_Xs], [], _N) -> descendant;
cmp([X, _X2], [Y|_Ys], 1) when X < Y -> ancestor;
cmp([X, _X2, _X3], [Y|_Ys], 1) when X < Y -> ancestor;
cmp([X], [Y|_Ys], 2) when X < Y -> ancestor;
cmp([X, _X2], [Y|_Ys], 2) when X < Y -> ancestor;
cmp([X|_Xs], [Y, _Y1], 1) when X > Y -> descendant;
cmp([X|_Xs], [Y, _Y1, _Y2], 1) when X > Y -> descendant;
cmp([X|_Xs], [Y], 2) when X > Y -> descendant;
cmp([X|_Xs], [Y, _Y1], 2) when X > Y -> descendant;
cmp([X], [Y|_Ys], N) when X < Y, N >= 3 -> ancestor;
cmp([X|_Xs], [Y], N) when X > Y, N >= 3 -> descendant;
cmp(_Xs, _Ys, _N) -> undefined.

chk_vl([V1, V2|Vs] = VL) when is_integer(V1), V1 >= 0, is_integer(V2), V2 >= 0 ->
    true = chk_vl_tail(Vs),
    VL.

chk_vl_tail([]) -> true;
chk_vl_tail([V]) -> true = (is_integer(V) andalso V > 0);
chk_vl_tail([V|Vs]) ->
    true = (is_integer(V) andalso V >= 0),
    chk_vl_tail(Vs).

vl2vs([V1, V2 | Vs], TZ) when is_integer(V1), V1 >= 0, is_integer(V2), V2 >= 0 ->
    unicode:characters_to_binary([integer_to_binary(V1), $., integer_to_binary(V2)|vl2vs_tail(Vs,TZ)]).

vl2vs_tail([], _TZ) -> [];
vl2vs_tail([V], false) ->
    true = (is_integer(V) andalso V > 0),
    [$., integer_to_binary(V)];
vl2vs_tail([V], true) ->
    true = (is_integer(V) andalso V >= 0),
    [$., integer_to_binary(V)];
vl2vs_tail([V|Vs], TZ) ->
    true = (is_integer(V) andalso V >= 0),
    [$., integer_to_binary(V)|vl2vs_tail(Vs, TZ)].

drop_zeros([0|Xs]) -> drop_zeros(Xs);
drop_zeros(Xs) -> Xs.

split(V) -> string:split(V, <<$.>>, all).
-endif.
