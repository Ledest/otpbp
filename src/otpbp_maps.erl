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

-define(IS_DICT(D), is_record(D, dict, tuple_size(dict:new()))).

-ifndef(HAVE_maps__size_1).
size(Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Map]),
    dict:size(Map).
-endif.

-ifndef(HAVE_maps__from_list_1).
from_list(List) when is_list(List) -> dict:from_list(List);
from_list(List) -> error(badarg, [List]).
-endif.

-ifndef(HAVE_maps__to_list_1).
to_list(Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Map]),
    dict:to_list(Map).
-endif.

-ifndef(HAVE_maps__is_key_2).
is_key(Key, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Map]),
    dict:is_key(Key, Map).
-endif.

-ifndef(HAVE_maps__find_2).
find(Key, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Map]),
    dict:find(Key, Map).
-endif.

-ifndef(HAVE_maps__get_2).
get(Key, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Map]),
    case dict:find(Key, Map) of
        {ok, V} -> V;
        error -> error({badkey, Key}, [Key, Map])
    end.
-endif.

-ifndef(HAVE_maps__get_3).
get(Key, Map, Default) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Map, Default]),
    case dict:find(Key, Map) of
        {ok, V} -> V;
        error -> Default
    end.
-endif.

-ifndef(HAVE_maps__remove_2).
remove(Key, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Map]),
    dict:erase(Key, Map).
-endif.

-ifndef(HAVE_maps__map_2).
map(Fun, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Fun, Map]),
    is_function(Fun, 1) orelse error(badarg, [Fun, Map]),
    dict:map(Fun, Map).
-endif.

-ifndef(HAVE_maps__fold_3).
fold(Fun, Init, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Fun, Init, Map]),
    is_function(Fun, 2) orelse error(badarg, [Fun, Init, Map]),
    dict:fold(Fun, Init, Map).
-endif.

-ifndef(HAVE_maps__filter_2).
-ifdef(HAVE_maps__fold_3).
filter(Fun, Map) ->
    is_map(Map) orelse error({badmap, Map}, [Fun, Map]),
    is_function(Fun, 2) orelse error(badarg, [Fun, Map]),
    maps:fold(fun(K, V, A) ->
                  case Fun(K, V) of
                      true -> A#{K => V};
                      false -> A
                  end
              end, #{}, Map).
-else.
filter(Fun, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Fun, Map]),
    is_function(Fun, 1) orelse error(badarg, [Fun, Map]),
    dict:filter(Fun, Map).
-endif.
-endif.

-ifndef(HAVE_maps__merge_2).
merge(Map1, Map2) ->
    ?IS_DICT(Map1) orelse error({badmap, Map1}, [Map1, Map2]),
    ?IS_DICT(Map2) orelse error({badmap, Map2}, [Map1, Map2]),
    dict:merge(fun(_, _, V2) -> V2 end, Map1, Map2).
-endif.

-ifndef(HAVE_maps__put_3).
put(Key, Value, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}),
    dict:store(Key, Value, Map).
-endif.

-ifndef(HAVE_maps__update_3).
update(Key, Value, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}),
    dict:is_key(Key, Map) orelse error({badkey, Key}),
    dict:store(Key, Value, Map).
-endif.

-ifndef(HAVE_maps__keys_1).
keys(Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Map]),
    dict:fetch_keys(Map).
-endif.

-ifndef(HAVE_maps__values_1).
values(Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Map]),
    [V || {_, V} <- dict:to_list(Map)].
-endif.

-ifndef(HAVE_maps__with_2).
with(Ks, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Ks, Map]),
    is_list(Ks) orelse error(badarg, [Ks, Map]),
    dict:from_list(lists:foldl(fun(K, A) ->
                                   case dict:find(K, Map) of
                                       {ok, V} -> [{K, V}|A];
                                       error -> A
                                   end
                               end, [], Ks).
-endif.

-ifndef(HAVE_maps__without_2).
without(Ks, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Ks, Map]),
    is_list(Ks) orelse error(badarg, [Ks, Map]),
    lists:foldl(fun dict:erase/2, Map, Ks).
-endif.

-ifndef(HAVE_maps__update_with_4).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Init, Map) ->
    is_map(Map) orelse error({badmap, Map}, [Key, Fun, Init, Map]),
    is_function(Fun, 1) orelse error(badarg, [Key, Fun, Init, Map]),
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> Map#{Key => Init}
    end.
-else.
update_with(Key, Fun, Init, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}),
    is_function(Fun, 1) orelse error(badarg, [Key, Fun, Init, Map]),
    dict:update(Key, Fun, Init, Map).
-endif.
-endif.

-ifndef(HAVE_maps__update_with_3).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Map) ->
    is_map(Map) orelse error({badmap, Map}, [Key, Fun, Map]),
    is_function(Fun, 1) orelse error(badarg, [Key, Fun, Map]),
    case maps:find(Key, Map) of
        {ok, Val} -> maps:update(Key, Fun(Val), Map);
        error -> error({badkey, Key}, [Key, Fun, Map])
    end.
-else.
update_with(Key, Fun, Map) ->
    ?IS_DICT(Map) orelse error({badmap, Map}, [Key, Fun, Map]),
    is_function(Fun, 1) orelse error(badarg, [Key, Fun, Map]),
    dict:update(Key, Fun, Map).
-endif.
-endif.

-ifndef(HAVE_maps__take_2).
take(Key, Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> {Val, maps:remove(Key, Map)};
        error -> error
    end;
take(Key, Map) -> error({badmap, Map}, [Key, Map]).
-endif.
