-module(otpbp_xmerl_xml_indent).

-ifndef('HAVE_xmerl_xml_indent__#root#_4').
% OTP 27.0
-export(['#root#'/4]).
-endif.
-ifndef('HAVE_xmerl_xml_indent__#element#_5').
% OTP 27.0
-export(['#element#'/5]).
-endif.

-ifndef('HAVE_xmerl_xml_indent__#root#_4').
-include_lib("xmerl/include/xmerl.hrl").

'#root#'(Data, [#xmlAttribute{name = prolog, value = V}], [], _E) -> [V, Data];
'#root#'(Data, _Attrs, [], _E) -> ["<?xml version=\"1.0\"?>\n", Data].
-endif.

-ifndef('HAVE_xmerl_xml_indent__#element#_5').
'#element#'(Tag, [], Attrs, _Parents, _E) -> xmerl_lib:empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, Parents, _E) ->
    xmerl_lib:markup(Tag, Attrs,
                     case is_char(Data) of
                         true ->
                             LengthParents = length(Parents),
                             IndentParents1 = indent(LengthParents + 1),
                             lists:foldr(fun(DataEntry, A) -> [IndentParents1 ++ DataEntry|A] end,
                                         indent(LengthParents), Data);
                         false -> Data
                     end).

is_char([[X|_]|_]) -> not is_integer(X);
is_char(Data) when is_list(Data) -> false.

indent(Level) -> [$\n|lists:duplicate(2 * Level, $\s)].
-endif.
