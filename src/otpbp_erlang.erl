-module(otpbp_erlang).

-ifndef(HAVE_erlang__binary_to_integer_1).
-export([binary_to_integer/1]).
-endif.
-ifndef(HAVE_erlang__binary_to_integer_2).
-export([binary_to_integer/2]).
-endif.
-ifndef(HAVE_erlang__binary_to_float_1).
-export([binary_to_float/1]).
-endif.
-ifndef(HAVE_erlang__integer_to_binary_1).
-export([integer_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__integer_to_binary_2).
-export([integer_to_binary/2]).
-endif.
-ifndef(HAVE_erlang__float_to_binary_1).
-export([float_to_binary/1]).
-endif.
-ifndef(HAVE_erlang__delete_element_2).
-export([delete_element/2]).
-endif.
-ifndef(HAVE_erlang__insert_element_3).
-export([insert_element/3]).
-endif.
-ifndef(HAVE_erlang__get_keys_0).
-export([get_keys/0]).
-endif.

-ifndef(HAVE_erlang__binary_to_integer_1).
binary_to_integer(Binary) -> list_to_integer(binary_to_list(Binary)).
-endif.

-ifndef(HAVE_erlang__binary_to_integer_2).
binary_to_integer(Binary, Base) -> list_to_integer(binary_to_list(Binary), Base).
-endif.

-ifndef(HAVE_erlang__binary_to_float_1).
binary_to_float(Binary) -> list_to_float(binary_to_list(Binary)).
-endif.

-ifndef(HAVE_erlang__integer_to_binary_1).
integer_to_binary(Integer) -> list_to_binary(integer_to_list(Integer)).
-endif.

-ifndef(HAVE_erlang__integer_to_binary_2).
integer_to_binary(Integer, Base) -> list_to_binary(integer_to_list(Integer, Base)).
-endif.

-ifndef(HAVE_erlang__float_to_binary_1).
float_to_binary(Float) -> list_to_binary(float_to_list(Float)).
-endif.

-ifndef(HAVE_erlang__delete_element_2).
delete_element(Index, Tuple) when is_integer(Index), Index > 0, Index =< tuple_size(Tuple) ->
    delete_element(Index, Tuple, [], tuple_size(Tuple));
delete_element(Index, Tuple) -> error(badarg).

delete_element(_, _, List, 0) -> list_to_tuple(List);
delete_element(Index, Tuple, List, Index) -> delete_element(Index, Tuple, List, Index - 1);
delete_element(Index, Tuple, List, I) -> delete_element(Index, Tuple, [element(I, Tuple)|List], I - 1).
-endif.

-ifndef(HAVE_erlang__insert_element_3).
insert_element(Index, Tuple, Term) when Index =:= tuple_size(Tuple) + 1 -> erlang:append_element(Tuple, Term);
insert_element(Index, Tuple, Term) when is_integer(Index), Index > 0, Index =< tuple_size(Tuple) ->
    insert_element(Index, Tuple, Term, [], tuple_size(Tuple));
insert_element(Index, Tuple, Term) -> error(badarg).

insert_element(_, _, _, List, 0) -> list_to_tuple(List);
insert_element(Index, Tuple, Term, List, Index) ->
    insert_element(Index, Tuple, Term, [Term, element(Index, Tuple)|List], Index - 1);
insert_element(Index, Tuple, Term, List, I) -> insert_element(Index, Tuple, Term, [element(I, Tuple)|List], I - 1).
-endif.

-ifndef(HAVE_erlang__get_keys_0).
get_keys() -> proplists:get_keys(get()).
-endif.
