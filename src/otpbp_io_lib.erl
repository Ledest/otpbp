-module(otpbp_io_lib).

-ifndef(HAVE_io_lib__limit_term_2).
% OTP 20.0
-export([limit_term/2]).
-endif.
-ifndef(HAVE_io_lib__format_3).
% OTP 20.0
-export([format/3]).
-endif.
-ifndef(HAVE_io_lib__write_atom_as_latin1_1).
% OTP 20.0
-export([write_atom_as_latin1/1]).
-endif.

-ifndef(HAVE_io_lib__limit_term_2).
%% The intention is to mimic the depth limitation of io_lib:write()
%% and io_lib_pretty:print(). The leaves ('...') should never be
%% seen when printed with the same depth. Bitstrings are never
%% truncated, which is OK as long as they are not sent to other nodes.
-spec limit_term(term(), non_neg_integer()) -> term().
limit_term(Term, Depth) ->
    try test_limit(Term, Depth) of
        ok -> Term
    catch
        throw:limit -> limit(Term, Depth)
    end.

limit(_, 0) -> '...';
limit([_|_], 1) -> '...';
limit([H|T] = L, D) ->
    case io_lib:printable_list(L) of
        true -> L;
        false ->
            D1 = D - 1,
            [limit(H, D1)|limit_tail(T, D1)]
    end;
limit({}, _D) -> {};
limit(T, D) when is_tuple(T) -> limit_tuple(T, D);
limit(Term, D) when is_map(Term) -> limit_map(Term, D);
limit(Term, D) when is_bitstring(Term) -> limit_bitstring(Term, D);
limit(Term, _D) -> Term.

limit_tail([], _D) -> [];
limit_tail(_, 1) -> ['...'];
limit_tail([H|T], D) ->
    D1 = D - 1,
    [limit(H, D1)|limit_tail(T, D1)];
limit_tail(Other, D) -> limit(Other, D - 1).

limit_tuple(T, 1) when is_tuple(T) -> '...';
limit_tuple(T, D) when is_tuple(T) ->
    D1 = D - 1,
    [H|R] = tuple_to_list(T),
    list_to_tuple([limit(H, D1)|limit_tail(R, D1)]).

limit_map(Map, D) when map_size(Map) =< D -> Map;
limit_map(Map, D) -> limit_map(maps:to_list(Map), D, #{}).

limit_map(_, 0, A) -> A;
limit_map([{K, V}|T], D, A) -> limit_map(T, D - 1, A#{K => V}).

%% limit_map_body(_, 0) -> [{'...', '...'}];
%% limit_map_body([], _) -> [];
%% limit_map_body([{K,V}], D) -> [limit_map_assoc(K, V, D)];
%% limit_map_body([{K,V}|KVs], D) ->
%%     [limit_map_assoc(K, V, D) | limit_map_body(KVs, D-1)].

%% limit_map_assoc(K, V, D) ->
%%     {limit(K, D-1), limit(V, D-1)}.
-compile({inline, [limit_tuple/2]}).

limit_bitstring(B, _D) -> B. %% Keeps all printable binaries.

test_limit(_, 0) -> throw(limit);
test_limit([_|_], 1) -> throw(limit);
test_limit([H|T] = L, D) when is_integer(D) ->
    case io_lib:printable_list(L) of
        true -> ok;
        false ->
            D1 = D - 1,
            test_limit(H, D1),
            test_limit_tail(T, D1)
    end;
test_limit({}, _D) -> ok;
test_limit(T, D) when is_tuple(T) -> test_limit_tuple(T, D);
test_limit(Term, D) when is_map(Term) -> test_limit_map(Term, D);
test_limit(Term, D) when is_bitstring(Term) -> test_limit_bitstring(Term, D);
test_limit(_Term, _D) -> ok.

test_limit_tail([], _D) -> ok;
test_limit_tail(_, 1) -> throw(limit);
test_limit_tail([H|T], D) ->
    D1 = D - 1,
    test_limit(H, D1),
    test_limit_tail(T, D1);
test_limit_tail(Other, D) -> test_limit(Other, D - 1).

test_limit_tuple(T, D) when is_tuple(T) -> test_limit_tuple(T, 1, tuple_size(T), D).

test_limit_tuple(_T, I, Sz, _D) when I > Sz -> ok;
test_limit_tuple(_, _, _, 1) -> throw(limit);
test_limit_tuple(T, I, Sz, D) ->
    D1 = D - 1,
    test_limit(element(I, T), D1),
    test_limit_tuple(T, I + 1, Sz, D1).

test_limit_map(_Map, _D) -> ok.
%%     test_limit_map_body(erts_internal:maps_to_list(Map, D), D).

%% test_limit_map_body(_, 0) -> throw(limit);
%% test_limit_map_body([], _) -> ok;
%% test_limit_map_body([{K,V}], D) -> test_limit_map_assoc(K, V, D);
%% test_limit_map_body([{K,V}|KVs], D) ->
%%     test_limit_map_assoc(K, V, D),
%%     test_limit_map_body(KVs, D-1).

%% test_limit_map_assoc(K, V, D) ->
%%     test_limit(K, D-1),
%%     test_limit(V, D-1).
-compile({inline, [test_limit_tuple/2]}).

test_limit_bitstring(_, _) -> ok.
-endif.

-ifndef(HAVE_io_lib__format_3).
format(Format, Data, []) -> io_lib:format(Format, Data);
format(Format, Data, [{chars_limit, L}]) when is_integer(L), L >= -1 -> io_lib:format(Format, Data).
-endif.

-ifndef(HAVE_io_lib__write_atom_as_latin1_1).
write_atom_as_latin1(Atom) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
        true -> io_lib:write_string_as_latin1(Chars, $');
        false -> Chars
    end.

-compile({inline, [quote_atom/2]}).
quote_atom(_Atom, []) -> true;
quote_atom(Atom, [C|Cs]) when is_integer(C) ->
    if
        C >= $a, C =< $z; C >= $ß, C =< $ÿ, C =/= $÷ -> not name_chars(Cs) orelse erl_scan:reserved_word(Atom);
        true -> true
    end.

name_chars([C|Cs]) when is_integer(C) -> name_char(C) andalso name_chars(Cs);
name_chars([]) -> true.

-compile({inline, [name_char/1]}).
name_char(C) ->
    C >= $0 andalso C =< $9 orelse
    C >= $a andalso C =< $z orelse
    C >= $A andalso C =< $Z orelse
    C >= $ß andalso C =< $ÿ andalso C =/= $÷ orelse
    C >= $À andalso C =< $Þ andalso C =/= $× orelse
    C =:= $_ orelse
    C =:= $@.
-endif.
