-module(otpbp_disksup).

-ifndef(HAVE_disksup__parse_df_2).
% OTP 21.0
-export([parse_df/2]).
-endif.
-ifndef(HAVE_disksup__get_disk_info_0).
% OTP 26.0
-export([get_disk_info/0]).
-endif.
-ifndef(HAVE_disksup__get_disk_info_1).
% OTP 26.0
-export([get_disk_info/1]).
-endif.

-ifndef(HAVE_disksup__get_disk_info_0).
-ifdef(HAVE_disksup__get_disk_info_1).
-import(disksup, [get_disk_info/1]).
-endif.
-ifdef(HAVE_disksup__parse_df_2).
-import(disksup, [parse_df/2]).
-endif.
-endif.

-ifndef(HAVE_disksup__get_disk_info_0).
get_disk_info() -> get_disk_info("").
-endif.

-ifndef(HAVE_disksup__get_disk_info_1).
get_disk_info(Path) ->
    case get_disk_info(Path, os:type()) of
        [] -> [{Path, 0, 0, 0}];
        DiskInfo -> DiskInfo
    end.

get_disk_info("", {win32, _}) -> disk_info_win32(os_mon_sysinfo:get_disk_info());
get_disk_info(DriveRoot, {win32, _}) -> disk_info_win32(os_mon_sysinfo:get_disk_info(DriveRoot));
get_disk_info(Path, {unix, OS})
  when OS =:= linux; OS =:= freebsd; OS =:= openbsd; OS =:= netbsd;
       OS =:= solaris; OS =:= dragonfly; OS =:= sunos4; OS =:= posix ->
    disk_info_solaris(skip_to_eol(run_df(Path, OS)));
get_disk_info(Path, {unix, darwin}) -> disk_info_susv3(skip_to_eol(run_df(Path, darwin)));
get_disk_info(Path, {unix, irix}) -> disk_info_irix(skip_to_eol(run_df(Path, irix))).

run_df(Path, solaris) -> my_cmd("/usr/bin/df -lk " ++ Path);
run_df(Path, irix) -> my_cmd("/usr/sbin/df -lk " ++ Path);
run_df(Path, linux) -> my_cmd(find_cmd("df", "/bin") ++ " -lk -x squashfs " ++ Path);
run_df(Path, posix) -> my_cmd("df -k -P " ++ Path);
run_df(Path, dragonfly) -> my_cmd("/bin/df -k -t ufs,hammer " ++ Path);
run_df(Path, netbsd) -> my_cmd("/bin/df -k -t ffs " ++ Path);
run_df(Path, sunos4) -> my_cmd("df " ++ Path);
run_df(Path, darwin) -> my_cmd("/bin/df -i -k -t ufs,hfs,apfs " ++ Path);
run_df(Path, OS) when OS =:= freebsd; OS =:= openbsd -> my_cmd("/bin/df -k -l " ++ Path).

my_cmd(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, {env, [{"LANG0", "C"}]}]),
    receive
        {Port, {data, N}} -> N;
        {'EXIT', Port, Reason} -> exit({port_died, Reason})
    end.

find_cmd(Cmd) -> os:find_executable(Cmd).

find_cmd(Cmd, Path) ->
    %% try to find it at the specific location
    case os:find_executable(Cmd, Path) of
        false -> find_cmd(Cmd);
        Found -> Found
    end.

disk_info_win32([]) -> [];
disk_info_win32([H|T]) ->
    case io_lib:fread("~s~s~d~d~d", H) of
        {ok, [Drive, "DRIVE_FIXED", BAvail, BTot, _TotFree], _RestStr} ->
            [{Drive, BTot div 1024, BAvail div 1024,
              trunc(math:ceil(100 * ((BTot - BAvail) / BTot)))}|disk_info_win32(T)];
        {ok, _, _RestStr} -> disk_info_win32(T);
        _Other -> []
    end.

disk_info_solaris(Str) when Str =:= ""; Str =:= "\n" -> [];
disk_info_solaris(Str) ->
    case parse_df(Str, posix) of
        {ok, {KiBTotal, KiBAvailable, Capacity, MntOn}, RestStr} ->
            [{MntOn, KiBTotal, KiBAvailable, Capacity}|disk_info_solaris(RestStr)];
        _Other -> disk_info_solaris(skip_to_eol(Str))
    end.

disk_info_irix(Str) when Str =:= ""; Str =:= "\n" -> [];
disk_info_irix(Str) ->
    case io_lib:fread("~s~s~d~d~d~d~s", Str) of
        {ok, [_FS, _FSType, KiBAvailable, Capacity, _Avail, KiBTotal, MntOn], RestStr} ->
            [{MntOn, KiBTotal, KiBAvailable, Capacity}|disk_info_irix(RestStr)];
        _Other -> disk_info_irix(skip_to_eol(Str))
    end.

disk_info_susv3(Str) when Str =:= ""; Str =:= "\n" -> [];
disk_info_susv3(Str) ->
    case parse_df(Str, susv3) of
        {ok, {KiBTotal, KiBAvailable, Capacity, MntOn}, RestStr} ->
            [{MntOn, KiBTotal, KiBAvailable, Capacity}|disk_info_susv3(RestStr)];
        _Other -> disk_info_susv3(skip_to_eol(Str))
    end.

skip_to_eol([$\n|T]) -> T;
skip_to_eol([_|T]) -> skip_to_eol(T);
skip_to_eol([]) -> [].
-endif.

-ifndef(HAVE_disksup__parse_df_2).
parse_df(Input0, Flavor) ->
    {KiBTotalStr, Input2} = parse_df_take_word(parse_df_skip_word(Input0)),
    {KiBAvailableStr, Input4} = parse_df_take_word(parse_df_skip_word(Input2)),
    {CapacityStr, Input5} = parse_df_take_word_percent(Input4),
    try {list_to_integer(KiBTotalStr), list_to_integer(KiBAvailableStr), list_to_integer(CapacityStr)} of
        {KiBTotal, KiBAvailable, Capacity} ->
            {MountPath, Input7} = lists:splitwith(fun(C) -> C =/= $\r andalso C =/= $\n end,
                                                      if
                                                          Flavor =:= posix -> Input5;
                                                          Flavor =:= susv3 ->
                                                              Input5a = parse_df_skip_word(Input5),
                                                              Input5b = parse_df_skip_word(Input5a),
                                                              {_, Input5c} = parse_df_take_word_percent(Input5b),
                                                              Input5c
                                                      end),

            {ok, {KiBTotal, KiBAvailable, Capacity, MountPath}, lists:dropwhile(fun parse_df_is_eol/1, Input7)}
    catch
        error:badarg -> {error, parse_df}
    end.

parse_df_is_eol(C) -> C =:= $\r orelse C =:= $\n.

parse_df_is_not_space(C) -> C =/= $\s andalso C =/= $%.

parse_df_take_word_percent(Input) ->
    case lists:splitwith(fun parse_df_is_not_space/1, Input) of
        {Word, [$%|Remaining]} -> {Word, Remaining};
        Result -> Result
    end.

parse_df_skip_word(Input) ->
    lists:dropwhile(fun(C) -> C =:= $\s end, lists:dropwhile(fun parse_df_is_not_space/1, Input)).

parse_df_take_word(Input) ->
    {Word, Remaining} = lists:splitwith(fun parse_df_is_not_space/1, Input),
    {Word, lists:dropwhile(fun(C) -> C =:= $\s end, Remaining)}.
-endif.
