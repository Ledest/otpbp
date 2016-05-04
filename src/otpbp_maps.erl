-module(otpbp_maps).

-ifndef(HAVE_maps__filter_2).
-export([filter/2]).
-endif.
-ifndef(HAVE_maps__get_3).
-export([get/3]).
-endif.
-ifndef(HAVE_maps__merge_2).
-export([merge/2]).
-endif.
-ifndef(HAVE_maps__update_3).
-export([update/3]).
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

-ifndef(HAVE_maps__get_3).
get(Key, Map, Default) ->
    case dict:find(Key, Map) of
        {ok, V} -> V;
        error -> Default
    end.
-endif.

-ifndef(HAVE_maps__filter_2).
-ifdef(HAVE_maps__fold_3).
filter(F, Map) when is_function(F, 2) ->
    maps:fold(fun(K, V, A) ->
                  case F(K, V) of
                      true -> A#{K => V};
                      false -> A
                  end
              end, #{}, Map).
-else.
filter(F, Map) -> dict:filter(F, Map).
-endif.
-endif.

-ifndef(HAVE_maps__merge_2).
merge(Map1, Map2) -> dict:merge(fun(_, _, V2) -> V2 end, Map1, Map2).
-endif.

-ifndef(HAVE_maps__update_3).
update(Key, Value, Map) ->
    is_record(Map, dict, tuple_size(dict:new())) =:= true orelse error({badmap, Map}),
    dict:is_key(Key, Map) orelse error({badkey, Key}),
    dict:update(Key, fun(_) -> Value end, Map).
-endif.

-ifndef(HAVE_maps__values_1).
values(Map) -> [V || {_, V} <- dict:to_list(Map)].
-endif.

-ifndef(HAVE_maps__with_2).
with(Ks, Map) -> dict:filter(fun(K, _) -> lists:member(K, Ks) end, Map).
-endif.

-ifndef(HAVE_maps__without_2).
without(Ks, Map) -> dict:filter(fun(K, _) -> not lists:member(K, Ks) end, Map).
-endif.

-ifndef(HAVE_maps__update_with_4).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Init, Map) when is_function(Fun, 1), is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> maps:update(Key, Fun(Val), Map);
        error -> maps:put(Key, Init, Map)
    end;
update_with(Key, Fun, Init, Map) ->
     erlang:error(if
                      is_map(Map) -> badarg;
                      true -> {badmap, Map}
                 end,
                 [Key, Fun, Init, Map]).
-else.
update_with(Key, Fun, Init, Map) -> dict:update(Key, Fun, Init, Map).
-endif.
-endif.

-ifndef(HAVE_maps__update_with_3).
-ifdef(HAVE_maps__update_3).
update_with(Key, Fun, Map) when is_function(Fun, 1), is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> maps:update(Key, Fun(Val), Map);
        error -> erlang:error({badkey, Key}, [Key, Fun, Map])
    end;
update_with(Key, Fun, Map) ->
     erlang:error(if
                      is_map(Map) -> badarg;
                      true -> {badmap, Map}
                 end,
                 [Key, Fun, Map]).
-else.
update_with(Key, Fun, Map) -> dict:update(Key, Fun, Map).
-endif.
-endif.

-ifndef(HAVE_maps__take_2).
take(Key, Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> {Val, maps:remove(Key, Map)};
        error -> error
    end;
take(Key, Map) -> erlang:error({badmap, Map}, [Key, Map]).
-endif.
