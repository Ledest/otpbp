-module(otpbp_maps).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_maps__filter_2).
% OTP 18.0
-export([filter/2]).
-endif.
-ifndef(HAVE_maps__with_2).
% OTP 17.3
-export([with/2]).
-endif.
-ifndef(HAVE_maps__update_with_3).
% OTP 19.0
-export([update_with/3]).
-endif.
-ifndef(HAVE_maps__update_with_4).
% OTP 19.0
-export([update_with/4]).
-endif.
-ifndef(HAVE_maps__take_2).
% OTP 19.0
-export([take/2]).
-endif.

-ifdef(HAVE_MAP_SYNTAX_6).
-define(PUT(K, V, M), maps:put(K, V, M)).
-define(UPDATE_WITH(K, F, I, M, V),
        case maps:find(K, M) of
            {ok, V} -> maps:update(K, F(V), M);
            error -> maps:put(K, I, M)
        end).
-define(UPDATE_WITH(K, F, M, V),
        case maps:find(K, M) of
            {ok, V} -> maps:update(K, F(V), M);
            error -> error({badkey, K}, [K, F, M])
        end).
-else.
-define(PUT(K, V, M), M#{K => V}).
-define(UPDATE_WITH(K, F, I, M, V),
        case M of
            #{K := V} -> maps:update(K, F(V), M);
            #{} -> M#{K => I}
        end).
-define(UPDATE_WITH(K, F, M, V),
        case M of
            #{K := V} -> maps:update(K, F(V), M);
            #{} -> error({badkey, K}, [K, F, M])
        end).
-endif.

-ifndef(HAVE_maps__filter_2).
filter(Fun, M) ->
    is_map(M) orelse error({badmap, M}, [Fun, M]),
    is_function(Fun, 2) orelse error(badarg, [Fun, M]),
    maps:without(maps:fold(fun(K, V, A) ->
                               case Fun(K, V) of
                                   false -> [K|A];
                                   true -> A
                               end
                           end, [], M),
                 M).
-endif.

-ifndef(HAVE_maps__with_2).
with(Ks, M) ->
    is_map(M) orelse error({badmap, M}, [Ks, M]),
    is_list(Ks) orelse error(badarg, [Ks, M]),
    maps:from_list(lists:foldl(fun(K, A) ->
                                   case maps:find(K, M) of
                                       {ok, V} -> [{K, V}|A];
                                       _error -> A
                                   end
                               end, [], Ks)).
-endif.

-ifndef(HAVE_maps__update_with_4).
update_with(K, Fun, Init, M) ->
    is_map(M) orelse error({badmap, M}, [K, Fun, Init, M]),
    is_function(Fun, 1) orelse error(badarg, [K, Fun, Init, M]),
    ?UPDATE_WITH(K, Fun, Init, M, V).
-endif.

-ifndef(HAVE_maps__update_with_3).
update_with(K, Fun, M) ->
    is_map(M) orelse error({badmap, M}, [K, Fun, M]),
    is_function(Fun, 1) orelse error(badarg, [K, Fun, M]),
    ?UPDATE_WITH(K, Fun, M, V).
-endif.

-ifndef(HAVE_maps__take_2).
-ifdef(HAVE_MAP_SYNTAX_6).
take(K, M) ->
    is_map(M) orelse error({badmap, M}, [K, M]),
    case maps:find(K, M) of
        {ok, V} -> {V, maps:remove(K, M)};
        error -> error
    end.
-else.
take(K, M) ->
    case M of
        #{K := V} -> {V, maps:remove(K, M)};
        #{} -> error;
        _ -> error({badmap, M}, [K, M])
    end.
-endif.
-endif.
