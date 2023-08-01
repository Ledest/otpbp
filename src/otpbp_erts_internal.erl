-module(otpbp_erts_internal).

-ifndef(HAVE_erts_internal__map_next_3).
% OTP 21.0
-export([map_next/3]).
-endif.
-ifndef(HAVE_erts_internal__binary_to_integer_2).
% OTP 26.1
-export([binary_to_integer/2]).
-endif.
-ifndef(HAVE_erts_internal__list_to_integer_2).
% OTP 26.1
-export([list_to_integer/2]).
-endif.

-ifndef(HAVE_erts_internal__map_next_3).
% OTP 21.0
map_next(0, M, iterator) when is_map(M) -> lists:foldr(fun({K, V}, A) -> {K, V, A} end, none, maps:to_list(M));
map_next(0, M, L) when is_list(L) -> lists:reverse(maps:fold(fun(K, V, A) -> [{K, V}|A] end, [], M), L).
-endif.

-define(DIGITS_PER_SINT,
        {{30, 18, 14, 12, 10, 10, 9, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4},
         {62, 38, 30, 26, 23, 21, 20, 18, 17, 17, 16, 16, 15, 15, 14, 14, 14,
          13, 13, 13, 13, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11, 11}}).

-ifndef(HAVE_erts_internal__binary_to_integer_2).
-compile({no_auto_import, [binary_to_integer/2]}).
binary_to_integer(Bin, Base) when is_binary(Bin), Base >= 2, Base =< 36 ->
    case element(Base - 1, element(erlang:system_info(wordsize) div 4, ?DIGITS_PER_SINT)) of
        S when S < byte_size(Bin) -> big;
        _ ->
            try
                erlang:binary_to_integer(Bin, Base)
            catch
                error:badarg -> badarg
            end
    end;
binary_to_integer(_Bin, _Base) -> badarg.
-endif.

-ifndef(HAVE_erts_internal__list_to_integer_2).
-compile({no_auto_import, [list_to_integer/2]}).
list_to_integer(List, Base) when is_list(List), Base >= 2, Base =< 36 ->
    case element(Base - 1, element(erlang:system_info(wordsize) div 4, ?DIGITS_PER_SINT)) of
        S when S < length(List) -> big;
        _ ->
            try
                erlang:list_to_integer(List, Base)
            catch
                error:badarg -> badarg
            end
    end;
list_to_integer(_List, _Base) -> badarg.
-endif.
