-module(otpbp_lists).

-ifndef(HAVE_lists__droplast_1).
-export([droplast/1]).
-endif.

-ifndef(HAVE_lists__join_2).
-export([join/2]).
-endif.
-ifndef(HAVE_lists__search_2).
-export([search/2]).
-endif.

-ifndef(HAVE_lists__droplast_1).
droplast([_])  -> [];
droplast([H|T]) -> [H|droplast(T)].
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
