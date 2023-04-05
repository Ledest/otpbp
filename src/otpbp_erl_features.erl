-module(otpbp_erl_features).

-ifndef(HAVE_erl_features__all_0).
% OTP 25.0
-export([all/0]).
-endif.
-ifndef(HAVE_erl_features__configurable_0).
% OTP 25.0
-export([configurable/0]).
-endif.
-ifndef(HAVE_erl_features__enabled_0).
% OTP 25.0
-export([enabled/0]).
-endif.
-ifndef(HAVE_erl_features__used_1).
% OTP 25.0
-export([used/1]).
-endif.
-ifndef(HAVE_erl_features__info_1).
% OTP 25.0
-export([info/1]).
-endif.
-ifndef(HAVE_erl_features__short_1).
% OTP 25.0
-export([short/1]).
-endif.
-ifndef(HAVE_erl_features__long_1).
% OTP 25.0
-export([long/1]).
-endif.

-ifndef(HAVE_erl_features__all_0).
all() -> [].
-endif.

-ifndef(HAVE_erl_features__configurable_0).
configurable() -> [].
-endif.

-ifndef(HAVE_erl_features__enabled_0).
enabled() -> [].
-endif.

-ifndef(HAVE_erl_features__used_1).
used(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        error -> not_found;
        {_Mod, _Bin, _Fname} -> []
    end;
used(FName) when is_list(FName) ->
    case beam_lib:chunks(FName, ["Meta"], [allow_missing_chunks]) of
        {ok, {_, [{_, _}]}} -> [];
        _ -> not_found
    end.
-endif.

-ifndef(HAVE_erl_features__info_1).
info(Feature) -> error(invalid_feature, [Feature]).
-endif.

-ifndef(HAVE_erl_features__short_1).
short(Feature) -> error(invalid_feature, [Feature]).
-endif.

-ifndef(HAVE_erl_features__long_1).
long(Feature) -> error(invalid_feature, [Feature]).
-endif.
