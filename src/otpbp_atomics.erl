-module(otpbp_atomics).

-ifndef(HAVE_atomics__info_1).
-export([info/1]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__new_2).
-export([new/2]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__get_2).
-export([get/2]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__put_3).
-export([put/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__add_3).
-export([add/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__sub_3).
-export([sub/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__add_get_3).
-export([add_get/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__sub_get_3).
-export([sub_get/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__exchange_3).
-export([exchange/3]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.
-ifndef(HAVE_atomics__compare_exchange_4).
-export([compare_exchange/4]).
-ifndef(NEED_atomics__load_nif_0).
-define(NEED_atomics__load_nif_0, true).
-endif.
-endif.

-compile(no_inline).

-ifdef(NEED_atomics__load_nif_0).
-on_load(load_nif/0).
-define(ATOMICS_NIF_VSN, 1).

-define(OPT_SIGNED, 2#00000001).
-define(OPT_DEFAULT, ?OPT_SIGNED).

-opaque atomics_ref() :: binary().
-export_type([atomics_ref/0]).

load_nif() ->
    P = case code:priv_dir(rar) of
            {error, bad_name} ->
                D1 = filename:join([".", "priv", "lib"]),
                case filelib:is_dir(D1) of
                    true -> D1;
                    _ ->
                        D2 = [$.|D1],
                        case filelib:is_dir(D2) of
                            true -> D2;
                            _ -> "."
                        end
                end;
            D -> D
        end,
    E = file:native_name_encoding(),
    L = filename:join(P, ?MODULE_STRING),
    erlang:load_nif(L, {?ATOMICS_NIF_VSN, unicode:characters_to_binary(L, E, E)}).
-endif.

-ifndef(HAVE_atomics__new_2).
-spec new(Arity::pos_integer(), Opts::[{signed, boolean()}]) -> atomics_ref().
new(Arity, Opts) -> new_2(Arity, encode_opts(Opts, ?OPT_DEFAULT)).

-spec new_2(Arity::pos_integer(), Opts::non_neg_integer()) -> atomics_ref().
new_2(_Arity, _Opts) -> erlang:nif_error(undef).

encode_opts([{signed, true}|T], Acc) -> encode_opts(T, Acc bor ?OPT_SIGNED);
encode_opts([{signed, false}|T], Acc) -> encode_opts(T, Acc band (bnot ?OPT_SIGNED));
encode_opts([], Acc) -> Acc;
encode_opts(_, _) -> error(badarg).
-endif.

-ifndef(HAVE_atomics__put_3).
-spec put(Ref::atomics_ref(), Ix::integer(), Value::integer()) -> ok.
put(_Ref, _Ix, _Value) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__get_2).
-spec get(Ref::atomics_ref(), Ix::integer()) -> integer().
get(_Ref, _Ix) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__add_3).
-spec add(Ref::atomics_ref(), Ix::integer(), Incr::integer()) -> ok.
add(_Ref, _Ix, _Incr) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__add_get_3).
-spec add_get(Ref::atomics_ref(), Ix::integer(), Incr::integer()) -> integer().
add_get(_Ref, _Ix, _Incr) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__sub_3).
-spec sub(Ref::atomics_ref(), Ix::integer(), Decr::integer()) -> ok.
sub(_Ref, _Ix, _Decr) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__sub_get_3).
-spec sub_get(Ref::atomics_ref(), Ix::integer(), Decr::integer()) -> integer().
sub_get(_Ref, _Ix, _Decr) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__exchange_3).
-spec exchange(Ref::atomics_ref(), Ix::integer(), Desired::integer()) -> integer().
exchange(_Ref, _Ix, _Desired) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__compare_exchange_4).
-spec compare_exchange(Ref::atomics_ref(), Ix::integer(), Expected::integer(), Desired::integer()) -> ok | integer().
compare_exchange(_Ref, _Ix, _Expected, _Desired) -> erlang:nif_error(undef).
-endif.

-ifndef(HAVE_atomics__info_1).
-ifdef(HAVE_maps__to_list_1).
-spec info(Ref::atomics_ref()) ->
          #{size => non_neg_integer(), max => integer(), min => integer(), memory => non_neg_integer()}.
info(_Ref) -> erlang:nif_error(undef).
-else.
-compile([{parse_transform, otpbp_pt}]).
-spec info(Ref::atomics_ref()) -> [{size|max|min|memory, integer()}].
info(Ref) -> maps:from_list(info_1(Ref)).

-spec info(Ref::atomics_ref()) -> [{size|max|min|memory, integer()}].
info_1(_Ref) -> erlang:nif_error(undef).
-endif.
-endif.
