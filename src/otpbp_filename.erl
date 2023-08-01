-module(otpbp_filename).

-ifndef(HAVE_filename__basedir_2).
% OTP 19.0
-export([basedir/2]).
-endif.
-ifndef(HAVE_filename__basedir_3).
% OTP 19.0
-export([basedir/3]).
-endif.
-ifndef(HAVE_filename__safe_relative_path_1).
% OTP 19.3
-export([safe_relative_path/1]).
-endif.

-ifndef(HAVE_filename__basedir_2).
-ifdef(HAVE_filename__basedir_3).
-import(filename, [basedir/3]).
-endif.
-endif.

-ifndef(HAVE_filename__safe_relative_path_1).
safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative -> safe_relative_path(filename:split(Path), []);
        _ -> unsafe
    end.

safe_relative_path([H|_], []) when H =:= ".."; H =:= <<"..">> -> unsafe;
safe_relative_path([H|T], Acc) when H =:= $.; H =:= <<$.>>; H =:= <<"..">>; H =:= ".." -> safe_relative_path(T, Acc);
safe_relative_path([H|T], Acc) -> safe_relative_path(T, [H|Acc]);
safe_relative_path([], []) -> [];
safe_relative_path([], Acc) -> filename:join(lists:reverse(Acc)).
-endif.

-ifndef(HAVE_filename__basedir_2).
basedir(Type, Application) when is_atom(Type), is_list(Application) orelse is_binary(Application) ->
    basedir(Type, Application, #{}).
-endif.

-ifndef(HAVE_filename__basedir_3).
basedir(Type, Application, Opts) when is_atom(Type), is_map(Opts), is_list(Application) orelse is_binary(Application) ->
    Os = basedir_os_from_opts(Opts),
    Name = basedir_name_from_opts(Os, Application, Opts),
    Base = basedir_from_os(Type, Os),
    case {Type, Os} of
        {user_log, linux} -> filename:join([Base, Name, "log"]);
        {user_log, windows} -> filename:join([Base, Name, "Logs"]);
        {user_cache, windows} -> filename:join([Base, Name, "Cache"]);
        {Type, _} when Type =:= site_config; Type =:= site_data -> [filename:join(B, Name) || B <- Base];
        _ -> filename:join(Base, Name)
    end.

basedir_os_from_opts(#{os := linux}) -> linux;
basedir_os_from_opts(#{os := windows}) -> windows;
basedir_os_from_opts(#{os := darwin}) -> darwin;
basedir_os_from_opts(#{}) -> basedir_os_type().

basedir_name_from_opts(windows, App, #{author := Author, version := Vsn}) -> filename:join([Author, App, Vsn]);
basedir_name_from_opts(windows, App, #{author := Author}) -> filename:join(Author, App);
basedir_name_from_opts(_, App, #{version := Vsn}) -> filename:join(App, Vsn);
basedir_name_from_opts(_, App, _) -> App.

basedir_from_os(Type, linux) -> basedir_linux(Type);
basedir_from_os(Type, darwin) -> basedir_darwin(Type);
basedir_from_os(Type, windows) -> basedir_windows(Type).

-define(basedir_linux_user_data, ".local/share").
-define(basedir_linux_user_config, ".config").
-define(basedir_linux_user_cache, ".cache").
-define(basedir_linux_user_log, ".cache"). %% .cache/App/log
-define(basedir_linux_site_data, "/usr/local/share/:/usr/share/").
-define(basedir_linux_site_config, "/etc/xdg").

basedir_linux(user_data) -> getenv("XDG_DATA_HOME", ?basedir_linux_user_data, true);
basedir_linux(user_config) -> getenv("XDG_CONFIG_HOME", ?basedir_linux_user_config, true);
basedir_linux(user_cache) -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_cache, true);
basedir_linux(user_log) -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_log, true);
basedir_linux(site_data) -> string:tokens(getenv("XDG_DATA_DIRS", ?basedir_linux_site_data, false), ":");
basedir_linux(site_config) -> string:tokens(getenv("XDG_CONFIG_DIRS", ?basedir_linux_site_config, false), ":").

-define(basedir_darwin_user_data, "Library/Application Support").
-define(basedir_darwin_user_config, "Library/Application Support").
-define(basedir_darwin_user_cache, "Library/Caches").
-define(basedir_darwin_user_log, "Library/Logs").
-define(basedir_darwin_site_data, "/Library/Application Support").
-define(basedir_darwin_site_config, "/Library/Application Support").

basedir_darwin(user_data) -> basedir_join_home(?basedir_darwin_user_data);
basedir_darwin(user_config) -> basedir_join_home(?basedir_darwin_user_config);
basedir_darwin(user_cache) -> basedir_join_home(?basedir_darwin_user_cache);
basedir_darwin(user_log) -> basedir_join_home(?basedir_darwin_user_log);
basedir_darwin(site_data) -> [?basedir_darwin_site_data];
basedir_darwin(site_config) -> [?basedir_darwin_site_config].

-define(basedir_windows_user_data, "Local").
-define(basedir_windows_user_config, "Roaming").
-define(basedir_windows_user_cache, "Local").    %% Cache is added later
-define(basedir_windows_user_log, "Local").    %% Logs is added later

basedir_windows(Type) -> basedir_windows(Type, basedir_windows_appdata()).

basedir_windows(user_data, noappdata) -> basedir_join_home(?basedir_windows_user_data);
basedir_windows(user_config, noappdata) -> basedir_join_home(?basedir_windows_user_config);
basedir_windows(user_config, {ok, AppData}) -> AppData;
basedir_windows(user_cache, noappdata) -> basedir_join_home(?basedir_windows_user_cache);
basedir_windows(user_log, noappdata) -> basedir_join_home(?basedir_windows_user_log);
basedir_windows(Type, noappdata) when Type =:= site_data; Type =:= site_config -> "";
basedir_windows(Type, {ok, AppData}) when Type =:= user_data; Type =:= user_cache; Type =:= user_log ->
    getenv("LOCALAPPDATA", AppData);
basedir_windows(Type, {ok, _}) when Type =:= site_data; Type =:= site_config -> "".

basedir_windows_appdata() ->
    case os:getenv("APPDATA", "") of
        "" -> noappdata;
        Val -> {ok, Val}
    end.

basedir_join_home(Dir) ->
    filename:join(case os:getenv("HOME") of
                      false ->
                          {ok, [[Home]]} = init:get_argument(home),
                          Home;
                      Home -> Home
                  end,
                  Dir).

basedir_os_type() ->
    case os:type() of
        {unix, darwin} -> darwin;
        {win32, _} -> windows;
        _ -> linux
    end.

getenv(K, Def, false) -> getenv(K, Def);
getenv(K, Def, true)  -> getenv(K, basedir_join_home(Def)).

getenv(K, Def) ->
    case os:getenv(K) of
        Val when Val =:= ""; Val =:= false -> Def;
        Val -> Val
    end.
-endif.
