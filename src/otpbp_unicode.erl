-module(otpbp_unicode).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_unicode__characters_to_nfc_binary_1).
% OTP 20.0
-export([characters_to_nfc_binary/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfc_list_1).
% OTP 20.0
-export([characters_to_nfc_list/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfd_binary_1).
% OTP 20.0
-export([characters_to_nfd_binary/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfd_list_1).
% OTP 20.0
-export([characters_to_nfd_list/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfkc_binary_1).
% OTP 20.0
-export([characters_to_nfkc_binary/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfkc_list_1).
% OTP 20.0
-export([characters_to_nfkc_list/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfkd_binary_1).
% OTP 20.0
-export([characters_to_nfkd_binary/1]).
-endif.
-ifndef(HAVE_unicode__characters_to_nfkd_list_1).
% OTP 20.0
-export([characters_to_nfkd_list/1]).
-endif.

-define(GC_N, 200). %% arbitrary number

-ifndef(HAVE_unicode__characters_to_nfc_binary_1).
characters_to_nfc_binary(CD) -> characters_to_nfc_binary(CD, ?GC_N, [], []).

characters_to_nfc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfc(CD) of
        [GC|Str] -> characters_to_nfc_binary(Str, N - 1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfc_binary(CD, _, Row, Acc) -> characters_to_nfc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

-ifndef(NEED_unicode__characters_to_binary_acc).
-define(NEED_unicode__characters_to_binary_acc, true).
-endif.
-endif.

-ifndef(HAVE_unicode__characters_to_nfc_list_1).
characters_to_nfc_list(CD) -> characters_to_nfc_list(CD, []).

characters_to_nfc_list(CD, Acc) ->
    case unicode_util:nfc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfc_list(Str, [CP|Acc]);
        [] -> lists:reverse(Acc);
        {error, Error} -> {error, lists:reverse(Acc), Error}
    end.
-endif.

-ifndef(HAVE_unicode__characters_to_nfd_binary_1).
characters_to_nfd_binary(CD) -> characters_to_nfd_binary(CD, ?GC_N, [], []).

characters_to_nfd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfd(CD) of
        [GC|Str] -> characters_to_nfd_binary(Str, N - 1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfd_binary(CD, _, Row, Acc) -> characters_to_nfd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

-ifndef(NEED_unicode__characters_to_binary_acc).
-define(NEED_unicode__characters_to_binary_acc, true).
-endif.
-endif.

-ifndef(HAVE_unicode__characters_to_nfd_list_1).
characters_to_nfd_list(CD) -> characters_to_nfd_list(CD, []).

characters_to_nfd_list(CD, Acc) ->
    case unicode_util:nfd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfd_list(Str, [CP|Acc]);
        [] -> lists:reverse(Acc);
        {error, Error} -> {error, lists:reverse(Acc), Error}
    end.
-endif.

-ifndef(HAVE_unicode__characters_to_nfkc_binary_1).
characters_to_nfkc_binary(CD) -> characters_to_nfkc_binary(CD, ?GC_N, [], []).

characters_to_nfkc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkc(CD) of
        [GC|Str] -> characters_to_nfkc_binary(Str, N - 1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkc_binary(CD, _, Row, Acc) -> characters_to_nfkc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

-ifndef(NEED_unicode__characters_to_binary_acc).
-define(NEED_unicode__characters_to_binary_acc, true).
-endif.
-endif.

-ifndef(HAVE_unicode__characters_to_nfkc_list_1).
characters_to_nfkc_list(CD) -> characters_to_nfkc_list(CD, []).

characters_to_nfkc_list(CD, Acc) ->
    case unicode_util:nfkc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkc_list(Str, [CP|Acc]);
        [] -> lists:reverse(Acc);
        {error, Error} -> {error, lists:reverse(Acc), Error}
    end.
-endif.

-ifndef(HAVE_unicode__characters_to_nfkd_binary_1).
characters_to_nfkd_binary(CD) -> characters_to_nfkd_binary(CD, ?GC_N, [], []).

characters_to_nfkd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkd(CD) of
        [GC|Str] -> characters_to_nfkd_binary(Str, N - 1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkd_binary(CD, _, Row, Acc) -> characters_to_nfkd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

-ifndef(NEED_unicode__characters_to_binary_acc).
-define(NEED_unicode__characters_to_binary_acc, true).
-endif.
-endif.

-ifndef(HAVE_unicode__characters_to_nfkd_list_1).
characters_to_nfkd_list(CD) -> characters_to_nfkd_list(CD, []).

characters_to_nfkd_list(CD, Acc) ->
    case unicode_util:nfkd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkd_list(Str, [CP|Acc]);
        [] -> lists:reverse(Acc);
        {error, Error} -> {error, lists:reverse(Acc), Error}
    end.
-endif.

-ifdef(NEED_unicode__characters_to_binary_acc).
acc_to_binary(Acc) -> list_to_binary(lists:reverse(Acc)).

prepend_row_to_acc(Row, Acc) -> [unicode:characters_to_binary(lists:reverse(Row))|Acc].
-endif.
