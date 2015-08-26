-module(otpbp_application).

-ifndef(HAVE_application__ensure_started_1).
-export([ensure_started/1]).
-endif.
-ifndef(HAVE_application__ensure_started_2).
-export([ensure_started/2]).
-endif.
-ifndef(HAVE_application__ensure_all_started_1).
-export([ensure_all_started/1]).
-endif.
-ifndef(HAVE_application__ensure_all_started_2).
-export([ensure_all_started/2]).
-endif.
-ifndef(HAVE_application__get_env_3).
-export([get_env/2]).
-endif.

-ifndef(HAVE_application__ensure_started_1).
-ifndef(HAVE_application__ensure_started_2).
ensure_started(Application) -> ensure_started(Application, temporary).
-else.
ensure_started(Application) -> application:ensure_started(Application, temporary).
-endif.
-endif.

-ifndef(HAVE_application__ensure_started_2).
ensure_started(Application, RestartType) ->
    case application:start(Application, RestartType) of
        ok -> ok;
        {error, {already_started, Application}} -> ok;
        Error -> Error
    end.
-endif.

-ifndef(HAVE_application__ensure_all_started_1).
-ifndef(HAVE_application__ensure_all_started_2).
ensure_all_started(Application) -> ensure_all_started(Application, temporary).
-else.
ensure_all_started(Application) -> application:ensure_all_started(Application, temporary).
-endif.
-endif.

-ifndef(HAVE_application__ensure_started_2).
ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
        {ok, Started} -> {ok, lists:reverse(Started)};
        {error, Reason, Started} ->
            lists:foreach(fun application:stop/1, Started),
            {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
        ok -> {ok, [Application|Started]};
        {error, {already_started, Application}} -> {ok, Started};
        {error, {not_started, Dependency}} ->
            case ensure_all_started(Dependency, Type, Started) of
                {ok, NewStarted} -> ensure_all_started(Application, Type, NewStarted);
                Error -> Error
            end;
        {error, Reason} -> {error, {Application, Reason}, Started}
    end.
-endif.

-ifndef(HAVE_application__get_env_3).
get_env(Application, Par, Def) ->
    case application:get_env(Application, Par) of
        {ok, Val} -> Val;
        undefined -> Def
    end.
-endif.
