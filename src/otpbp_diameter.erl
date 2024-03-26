-module(otpbp_diameter).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_diameter__which_connections_0).
% OTP 27.0
-export([which_connections/0]).
-endif.
-ifndef(HAVE_diameter__which_connections_1).
% OTP 27.0
-export([which_connections/1]).
-endif.
-ifndef(HAVE_diameter__which_transports_0).
% OTP 27.0
-export([which_transports/0]).
-endif.
-ifndef(HAVE_diameter__which_transports_1).
% OTP 27.0
-export([which_transports/1]).
-endif.
-ifndef(HAVE_diameter__which_watchdogs_0).
% OTP 27.0
-export([which_watchdogs/0]).
-endif.
-ifndef(HAVE_diameter__which_watchdogs_1).
% OTP 27.0
-export([which_watchdogs/1]).
-endif.

-ifndef(HAVE_diameter__which_connections_0).
which_connections() -> diameter_service:which_connections().
-endif.

-ifndef(HAVE_diameter__which_connections_1).
which_connections(SvcName) -> diameter_service:which_connections(SvcName).
-endif.

-ifndef(HAVE_diameter__which_transports_0).
which_transports() -> diameter_config:which_transports().
-endif.

-ifndef(HAVE_diameter__which_transports_1).
which_transports(SvcName) -> diameter_config:which_transports(SvcName).
-endif.

-ifndef(HAVE_diameter__which_watchdogs_0).
which_watchdogs() -> diameter_service:which_watchdogs().
-endif.

-ifndef(HAVE_diameter__which_watchdogs_1).
which_watchdogs(SvcName) -> diameter_service:which_watchdogs(SvcName).
-endif.
