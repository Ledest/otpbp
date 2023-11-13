-module(otpbp_eldap).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_eldap__paged_result_control_1).
% OTP 24.3
-export([paged_result_control/1]).
-endif.
-ifndef(HAVE_eldap__paged_result_control_2).
% OTP 24.3
-export([paged_result_control/2]).
-endif.
-ifndef(HAVE_eldap__paged_result_cookie_1).
% OTP 24.3
-export([paged_result_cookie/1]).
-endif.

-ifndef(HAVE_eldap__paged_result_control_1).
-ifdef(HAVE_eldap__paged_result_control_2).
-import(eldap, [paged_result_control/2]).
-endif.
-endif.

-ifndef(HAVE_eldap__paged_result_control_1).
paged_result_control(PageSize) -> paged_result_control(PageSize, "").
-endif.

-record('RealSearchControlValue', {size, cookie}).

-ifndef(HAVE_eldap__paged_result_control_2).
paged_result_control(PageSize, Cookie) when is_integer(PageSize) ->
    {ok, ControlValue} = 'ELDAPv3':encode('RealSearchControlValue',
                                          #'RealSearchControlValue'{size = PageSize, cookie = Cookie}),
    {control, "1.2.840.113556.1.4.319", true, ControlValue}.
-endif.

-ifndef(HAVE_eldap__paged_result_cookie_1).
-record('Control', {controlType, criticality = asn1_DEFAULT, controlValue = asn1_NOVALUE}).

paged_result_cookie({eldap_search_result, _, _, Controls}) ->
    case lists:search(fun(#'Control'{controlType = "1.2.840.113556.1.4.319"}) -> true;
                         (_) -> false
                      end,
                      Controls) of
        {value, #'Control'{controlValue = ControlValue}} ->
            {ok, #'RealSearchControlValue'{cookie = Cookie}} = 'ELDAPv3':decode('RealSearchControlValue', ControlValue),
            {ok, Cookie};
        _false -> {error, no_cookie}
    end;
paged_result_cookie({eldap_search_result, _, _}) -> {error, no_cookie}.
-endif.
