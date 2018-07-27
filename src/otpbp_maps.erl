-module(otpbp_maps).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_maps__size_1).
-export([size/1]).
-endif.
-ifndef(HAVE_maps__from_list_1).
-export([from_list/1]).
-endif.
-ifndef(HAVE_maps__to_list_1).
-export([to_list/1]).
-endif.
-ifndef(HAVE_maps__filter_2).
-export([filter/2]).
-endif.
-ifndef(HAVE_maps__map_2).
-export([map/2]).
-endif.
-ifndef(HAVE_maps__fold_3).
-export([fold/3]).
-endif.
-ifndef(HAVE_maps__is_key_2).
-export([is_key/2]).
-endif.
-ifndef(HAVE_maps__find_2).
-export([find/2]).
-endif.
-ifndef(HAVE_maps__get_2).
-export([get/2]).
-endif.
-ifndef(HAVE_maps__get_3).
-export([get/3]).
-endif.
-ifndef(HAVE_maps__remove_2).
-export([remove/2]).
-endif.
-ifndef(HAVE_maps__merge_2).
-export([merge/2]).
-endif.
-ifndef(HAVE_maps__put_3).
-export([put/3]).
-endif.
-ifndef(HAVE_maps__update_3).
-export([update/3]).
-endif.
-ifndef(HAVE_maps__keys_1).
-export([keys/1]).
-endif.
-ifndef(HAVE_maps__values_1).
-export([values/1]).
-endif.
-ifndef(HAVE_maps__with_2).
-export([with/2]).
-endif.
-ifndef(HAVE_maps__without_2).
-export([without/2]).
-endif.
-ifndef(HAVE_maps__update_with_3).
-export([update_with/3]).
-endif.
-ifndef(HAVE_maps__update_with_4).
-export([update_with/4]).
-endif.
-ifndef(HAVE_maps__take_2).
-export([take/2]).
-endif.

-ifndef(DICT_RECORD_SIZE).
-define(DICT_RECORD_SIZE, tuple_size(dict:new())).
-endif.
-define(IS_DICT(D), is_record(D, dict, ?DICT_RECORD_SIZE)).

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

-ifndef(HAVE_maps__size_1).
size(Map) ->
    case ?IS_DICT(Map) of
        true -> dict:size(Map);
        _ -> error({badmap, Map}, [Map])
    end.
-endif.

-ifndef(HAVE_maps__from_list_1).
from_list(List) when is_list(List) -> dict:from_list(List);
from_list(List) -> error(badarg, [List]).
-endif.

-ifndef(HAVE_maps__to_list_1).
to_list(Map) ->
    case ?IS_DICT(Map) of
        true -> dict:to_list(Map);
        _ -> error({badmap, Map}, [Map])
    end.
-endif.

-ifndef(HAVE_maps__is_key_2).
is_key(Key, Map) ->
    case ?IS_DICT(Map) of
        true -> dict:is_key(Key, Map);
        _ -> error({badmap, Map}, [Key, Map])
    end.
-endif.

-ifndef(HAVE_maps__find_2).
find(Key, Map) ->
    case ?IS_DICT(Map) of
        true -> dict:find(Key, Map);
        _ -> error({badmap, Map}, [Key, Map])
    end.
-endif.

-ifndef(HAVE_maps__get_2).
get(Key, Map) ->
    case  ?IS_DICT(Map) of
        true -> case dict:find(Key, Map) of
                    {ok, V} -> V;
                    error -> error({badkey, Key}, [Key, Map])
                end;
        _ -> error({badmap, Map}, [Key, Map])
    end.
-endif.

-ifndef(HAVE_maps__get_3).
get(Key, Map, Default) ->
    case ?IS_DICT(Map) of
        true -> case dict:find(Key, Map) of
                    {ok, V} -> V;
                    error -> Default
                end;
        _ -> error({badmap, Map}, [Key, Map, Default])
    end.
-endif.

-ifndef(HAVE_maps__remove_2).
remove(Key, Map) ->
    case ?IS_DICT(Map) of
        true -> dict:erase(Key, Map);
        _ -> error({badmap, Map}, [Key, Map])
    end.
-endif.

-ifndef(HAVE_maps__map_2).
map(Fun, Map) ->
    case ?IS_DICT(Map) of
        true when is_function(Fun, 1) -> dict:map(Fun, Map);
        true -> error(badarg, [Fun, Map]);
        _ -> error({badmap, Map}, [Fun, Map])
    end.
-endif.

-ifndef(HAVE_maps__fold_3).
fold(Fun, Init, Map) ->
    case ?IS_DICT(Map) of
        true when is_function(Fun, 2) -> dict:fold(Fun, Init, Map);
        true -> error(badarg, [Fun, Init, Map]);
        _ -> error({badmap, Map}, [Fun, Init, Map])
    end.
-endif.

-ifndef(HAVE_maps__filter_2).
-ifdef(HAVE_maps__fold_3).
filter(Fun, Map) when is_map(Map) ->
    if
        is_function(Fun, 2) -> maps:fold(fun(Key, Value, M) ->
                                             case Fun(Key, Value) of
                                                 true -> ?PUT(Key, Value, M);
                                                 false -> M
                                             end
                                         end, #{}, Map);
        true -> error(badarg, [Fun, Map])
    end;
filter(Fun, Map) -> error({badmap, Map}, [Fun, Map]).
-else.
filter(Fun, Map) ->
    case ?IS_DICT(Map) of
        true when is_function(Fun, 1) -> dict:filter(Fun, Map);
        true -> error(badarg, [Fun, Map]);
        _ -> error({badmap, Map}, [Fun, Map])
    end.
-endif.
-endif.

-ifndef(HAVE_maps__merge_2).
merge(Map1, Map2) ->
    case ?IS_DICT(Map1) of
        true -> case ?IS_DICT(Map2) of
                    true -> dict:merge(fun(_, _, V2) -> V2 end, Map1, Map2);
                    _ -> error({badmap, Map2}, [Map1, Map2])
                end;
        _ -> error({badmap, Map1}, [Map1, Map2])
    end.
-endif.

-ifndef(HAVE_maps__put_3).
put(Key, Value, Map) ->
    case ?IS_DICT(Map) of
        true -> dict:store(Key, Value, Map);
        _ -> error({badmap, Map})
    end.
-endif.

-ifndef(HAVE_maps__update_3).
update(Key, Value, Map) ->
    case ?IS_DICT(Map) of
        true -> case dict:is_key(Key, Map) of
                    true -> dict:store(Key, Value, Map);
                    _ -> error({badkey, Key})
                end;
        _ -> error({badmap, Map})
    end.
-endif.

-ifndef(HAVE_maps__keys_1).
keys(Map) ->
    case ?IS_DICT(Map) of
        true -> dict:fetch_keys(Map);
        _ -> error({badmap, Map}, [Map])
    end.
-endif.

-ifndef(HAVE_maps__values_1).
values(Map) ->
    case ?IS_DICT(Map) of
        true -> [V || {_, V} <- dict:to_list(Map)];
        _ -> error({badmap, Map}, [Map])
    end.
-endif.

-ifndef(HAVE_maps__with_2).
with(Ks, Map) ->
    case ?IS_DICT(Map) of
        true when is_list(Ks) -> dict:from_list(lists:foldl(fun(K, A) ->
                                                                case dict:find(K, Map) of
                                                                    {ok, V} -> [{K, V}|A];
                                                                    error -> A
                                                                end
                                                            end, [], Ks));
        true -> error(badarg, [Ks, Map]);
        _ -> error({badmap, Map}, [Ks, Map])
    end.
-endif.

-ifndef(HAVE_maps__without_2).
without(Ks, Map) ->
    case ?IS_DICT(Map) of
        true when is_list(Ks) -> lists:foldl(fun dict:erase/2, Map, Ks);
        true -> error(badarg, [Ks, Map]);
        _ -> error({badmap, Map}, [Ks, Map])
    end.
-endif.

-ifndef(HAVE_maps__update_with_4).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Init, Map) when is_map(Map) ->
    if
        is_function(Fun, 1) -> ?UPDATE_WITH(Key, Fun, Init, Map, Value);
        true -> error(badarg, [Key, Fun, Init, Map])
    end;
update_with(Key, Fun, Init, Map) -> error({badmap, Map}, [Key, Fun, Init, Map]).
-else.
update_with(Key, Fun, Init, Map) ->
    case ?IS_DICT(Map) of
        true when is_function(Fun, 1) -> dict:update(Key, Fun, Init, Map);
        true -> error(badarg, [Key, Fun, Init, Map]);
        _ -> error({badmap, Map})
    end.
-endif.
-endif.

-ifndef(HAVE_maps__update_with_3).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Map) when is_map(Map) ->
    if
        is_function(Fun, 1) -> ?UPDATE_WITH(Key, Fun, Map, Value);
        true -> error(badarg, [Key, Fun, Map])
    end;
update_with(Key, Fun, Map) -> error({badmap, Map}, [Key, Fun, Map]).
-else.
update_with(Key, Fun, Map) ->
    case ?IS_DICT(Map) of
        true when is_function(Fun, 1) -> dict:update(Key, Fun, Map);
        true -> error(badarg, [Key, Fun, Map]);
        _ -> error({badmap, Map}, [Key, Fun, Map])
    end.
-endif.
-endif.

-ifndef(HAVE_maps__take_2).
-ifdef(HAVE_MAP_SYNTAX_6).
take(Key, Map) when is_map(Map) ->
    case maps:find(Map) of
        {ok, Value} -> {Value, maps:remove(Key, Map)};
        error -> error
    end;
take(Key, Map) -> error({badmap, Map}, [Key, Map]).
-else.
-ifdef(HAVE_maps__find_2).
take(Key, Map) ->
    case Map of
        #{Key := Value} -> {Value, maps:remove(Key, Map)};
        #{} -> error;
        _ -> error({badmap, Map}, [Key, Map])
    end.
-else.
take(Key, Map) ->
    case ?IS_DICT(Map) of
        true -> case dict:find(Key, Map) of
                    {ok, Value} -> {Value, dict:erase(Key, Map)};
                    error -> error
                end;
        _ -> error({badmap, Map}, [Key, Map])
    end.
-endif.
-endif.
-endif.
