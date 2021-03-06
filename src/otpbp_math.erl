-module(otpbp_math).

-ifndef(HAVE_math__ceil_1).
% OTP 20.0
-export([ceil/1]).
-endif.
-ifndef(HAVE_math__floor_1).
% OTP 20.0
-export([floor/1]).
-endif.

-ifndef(HAVE_math__ceil_1).
-ifdef(HAVE_erlang__ceil_1).
-compile({no_auto_import, [ceil/1]}).
ceil(X) -> float(erlang:ceil(X)).
-else.
ceil(X) -> float(round(X + 0.5)).
-endif.
-endif.

-ifndef(HAVE_math__floor_1).
-ifdef(HAVE_erlang__floor_1).
-compile({no_auto_import, [floor/1]}).
floor(X) -> float(erlang:floor(X)).
-else.
floor(X) -> float(round(X - 0.5)).
-endif.
-endif.
