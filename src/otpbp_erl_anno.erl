-module(otpbp_erl_anno).

-ifndef(HAVE_erl_anno__new_1).
-export([new/1]).
-endif.

-ifndef(HAVE_erl_anno__new_1).
-type annotation() :: {file, file:filename_all()}
                    | {generated, boolean()}
                    | {location, location()}
                    | {record, boolean()}
                    | {text, string()}.

-type anno() :: location() | [annotation(),...].

-type column() :: pos_integer().
-type line() :: integer().
-type location() :: line() | {line(), column()}.

-define(LN(L), is_integer(L)).
-define(COL(C), (is_integer(C) andalso C >= 1)).

%% Location.
-define(LCOLUMN(C), ?COL(C)).
-define(LLINE(L), ?LN(L)).

-spec new(location()) -> anno().
new(Line) when ?LLINE(Line) -> new_location(Line);
new({Line, Column} = Loc) when ?LLINE(Line), ?LCOLUMN(Column) -> new_location(Loc);
new(Term) -> error(badarg, [Term]).

new_location(Location) -> Location.
-endif.