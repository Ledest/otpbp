-module(otpbp_math).

-ifndef(HAVE_math__log2_1).
-export([log2/1]).
-endif.

-ifndef(HAVE_math__ceil_1).
-export([ceil/1]).
-endif.
-ifndef(HAVE_math__floor_1).
-export([floor/1]).
-endif.

-ifndef(HAVE_math__log2_1).
log2(X) -> math:log(X) / math:log(2).
-endif.

-ifndef(HAVE_math__ceil_1).
-ifndef(HAVE_erlang__ceil_1).
ceil(X) -> float(erlang:ceil(X)).
-else.
ceil(X) -> float(round(X + 0.5)).
-endif.
-endif.

-ifndef(HAVE_math__floor_1).
-ifndef(HAVE_erlang__floor_1).
floor(X) -> float(erlang:floor(X)).
-else.
floor(X) -> float(round(X - 0.5)).
-endif.
-endif.
