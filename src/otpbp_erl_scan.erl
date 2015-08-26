-module(otpbp_erl_scan).

-ifndef(HAVE_erl_scan__category_1).
-export([category/1]).
-endif.
-ifndef(HAVE_erl_scan__column_1).
-export([column/1]).
-endif.
-ifndef(HAVE_erl_scan__line_1).
-export([line/1]).
-endif.
-ifndef(HAVE_erl_scan__location_1).
-export([location/1]).
-endif.
-ifndef(HAVE_erl_scan__symbol_1).
-export([symbol/1]).
-endif.
-ifndef(HAVE_erl_scan__text_1).
-export([text/1]).
-endif.

-ifndef(HAVE_erl_scan__category_1).
category(Token) -> proplists:get_value(category, erl_scan:token_info(Token)).
-endif.

-ifndef(HAVE_erl_scan__column_1).
column(Token) -> proplists:get_value(column, erl_scan:token_info(Token)).
-endif.

-ifndef(HAVE_erl_scan__line_1).
line(Token) -> proplists:get_value(line, erl_scan:token_info(Token)).
-endif.

-ifndef(HAVE_erl_scan__location_1).
location(Token) -> proplists:get_value(location, erl_scan:token_info(Token)).
-endif.

-ifndef(HAVE_erl_scan__symbol_1).
symbol(Token) -> proplists:get_value(symbol, erl_scan:token_info(Token)).
-endif.

-ifndef(HAVE_erl_scan__text_1).
text(Token) -> proplists:get_value(text, erl_scan:token_info(Token)).
-endif.
