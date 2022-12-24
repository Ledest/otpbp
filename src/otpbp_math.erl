-module(otpbp_math).

-ifndef(HAVE_math__log2_1).
% OTP 18.0
-export([log2/1]).
-endif.

-ifndef(HAVE_math__ceil_1).
% OTP 20.0
-export([ceil/1]).
-endif.
-ifndef(HAVE_math__floor_1).
% OTP 20.0
-export([floor/1]).
-endif.

-ifndef(HAVE_math__log2_1).
log2(X) -> math:log(X) / math:log(2).
-endif.

-ifndef(HAVE_math__ceil_1).
ceil(I) when is_integer(I) -> float(I);
ceil(0.0 = F) -> F;
ceil(F) when is_float(F), F < 0 -> float(trunc(F));
ceil(F) when is_float(F) ->
    case trunc(F) of
        I when I == F -> F;
        I -> float(I + 1)
    end;
ceil(A) -> error(badarg, [A]).
-endif.

-ifndef(HAVE_math__floor_1).
floor(I) when is_integer(I) -> float(I);
floor(0.0 = F) -> F;
floor(F) when is_float(F), F > 0 -> float(trunc(F));
floor(F) when is_float(F) ->
    case trunc(F) of
        I when I == F -> F;
        I -> float(I - 1)
    end;
floor(A) -> error(badarg, [A]).
-endif.
