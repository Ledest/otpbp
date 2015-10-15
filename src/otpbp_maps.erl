-module(otpbp_maps).

-ifndef(HAVE_maps__get_3).
-export([get/3]).
-endif.
-ifndef(HAVE_maps__merge_2).
-export([merge/2]).
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

-ifndef(HAVE_maps__get_3).
get(Key, Map, Default) ->
    case dict:find(Key, Map) of
        {ok, V} -> V;
        error -> Default
    end.
-endif.

-ifndef(HAVE_maps__merge_2).
merge(Map1, Map2) -> dict:merge(fun(_, _, V2) -> V2 end, Map1, Map2).
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
