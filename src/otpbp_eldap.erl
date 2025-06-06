-module(otpbp_eldap).

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

-ifndef(HAVE_eldap__paged_result_control_2).
paged_result_control(PageSize, Cookie) when is_integer(PageSize) ->
    {ok, ControlValue} = 'ELDAPv3':encode('RealSearchControlValue', {'RealSearchControlValue', PageSize, Cookie}),
    {control, "1.2.840.113556.1.4.319", true, ControlValue}.
-endif.

-ifndef(HAVE_eldap__paged_result_cookie_1).
paged_result_cookie({eldap_search_result, _, _, Controls}) ->
    case lists:search(fun({'Control',"1.2.840.113556.1.4.319", _, _}) -> true;
                         (_) -> false
                      end,
                      Controls) of
        {value, {'Control', _, _, ControlValue}} ->
            {ok, {'RealSearchControlValue', _, Cookie}} = 'ELDAPv3':decode('RealSearchControlValue', ControlValue),
            {ok, Cookie};
        _false -> {error, no_cookie}
    end;
paged_result_cookie({eldap_search_result, _, _}) -> {error, no_cookie}.
-endif.
