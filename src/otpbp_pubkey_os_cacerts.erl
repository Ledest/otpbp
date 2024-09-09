-module(otpbp_pubkey_os_cacerts).

-compile({parse_transform, otpbp_pt}).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-ifndef(HAVE_pubkey_os_cacerts__clear_0).
% OTP 25.0
-export([clear/0]).
-endif.
-ifndef(HAVE_pubkey_os_cacerts__format_error_2).
% OTP 27.0
-export([format_error/2]).
-endif.
-ifndef(HAVE_pubkey_os_cacerts__get_0).
% OTP 25.0
-export([get/0]).
-endif.
-ifndef(HAVE_pubkey_os_cacerts__load_0).
% OTP 25.0
-export([load/0]).
-endif.
-ifndef(HAVE_pubkey_os_cacerts__load_1).
% OTP 25.0
-export([load/1]).
-endif.

-ifndef(HAVE_pubkey_os_cacerts__get_0).
-ifdef(HAVE_pubkey_os_cacerts__load_0).
-import(pubkey_os_cacerts, [load/0]).
-endif.
-endif.

-ifndef(HAVE_erlang__get_cookie_1).
-ifndef(HAVE_erlang__set_cookie_1).
-ifndef(HAVE_inet__gen_udp_module_1).
-record(cert, {der :: public_key:der_encoded(), otp :: #'OTPCertificate'{}}).
-endif.
-endif.
-endif.

-ifndef(HAVE_pubkey_os_cacerts__clear_0).
-ifdef(HAVE_persistent_term__erase_1).
clear() -> persistent_term:erase(pubkey_os_cacerts).
-else.
clear() -> erase(pubkey_os_cacerts) =/= udefined.
-endif.
-endif.

-ifndef(HAVE_pubkey_os_cacerts__format_error_2).
format_error(Reason, [{_M, _F, _As, Info} | _]) ->
    #{general => io_lib:format("Failed to load cacerts: ~s",
                               [case proplists:get_value(error_info, Info) of
                                    #{cause := enoent} -> "operating system CA bundle could not be located";
                                    #{cause := {enotsup, OS}} ->
                                        io_lib:format("operating system ~p is not supported", [OS]);
                                    #{cause := {eopnotsupp, SubReason}} ->
                                        io_lib:format("operation failed because of ~p", [SubReason]);
                                    #{cause := {eopnotsupp, Status, _Acc}} ->
                                        io_lib:format("operation failed with status ~B", [Status])
                                end]),
      reason => io_lib:format("~p: ~p", [pubkey_os_cacerts, Reason])}.
-endif.

-ifndef(HAVE_pubkey_os_cacerts__get_0).
get() ->
    case get2() of
        undefined ->
            case load() of
                ok -> get1();
                {error, Reason} ->
                    erlang:error({failed_load_cacerts, conv_error_reason(Reason)},
                                 none,
                                 [{error_info, #{cause => Reason, module => pubkey_os_cacerts}}])
            end;
        CaCerts -> CaCerts
    end.

-compile({inline, get1/0}).
-ifdef(HAVE_persistent_term__get_1).
get1() -> persistent_term:get(pubkey_os_cacerts).
-else.
get1() ->
    Value = get(pubkey_os_cacerts),
    Value =:= undefined andalso error(badarg),
    Value.
-endif.

-compile({inline, get2/0}).
-ifdef(HAVE_persistent_term__get_2).
get2() -> persistent_term:get(pubkey_os_cacerts, undefined).
-else.
get2() -> get(pubkey_os_cacerts).
-endif.

-compile({inline, conv_error_reason/1}).
conv_error_reason(enoent) -> enoent;
conv_error_reason({enotsup, _OS}) -> enotsup;
conv_error_reason({eopnotsupp, _Reason}) -> eopnotsupp;
conv_error_reason({eopnotsupp, _Status, _Acc}) -> eopnotsupp.
-endif.

-ifndef(HAVE_pubkey_os_cacerts__load_1).
load(Paths) -> load(Paths, {error, enoent}).

-ifndef(NEED_load_2).
-define(NEED_load_2, true).
-endif.
-endif.

-ifndef(HAVE_pubkey_os_cacerts__load_0).
load() ->
    case os:type() of
        {unix, darwin} -> load_darwin();
        {unix, OS} ->
            load(if
                     OS =:= linux -> linux_paths();
                     OS =:= freebsd; OS =:= openbsd; OS =:= netbsd -> bsd_paths();
                     OS =:= sunos -> sunos_paths()
                 end,
                 undefined);
        OS -> {error, {enotsup, OS}}
    end.

-compile({inline, load_darwin/0}).
load_darwin() ->
    try run_cmd("/usr/bin/security",
                ["export",
                 "-t", "certs",
                 "-f", "pemseq",
                 "-k", "/System/Library/Keychains/SystemRootCertificates.keychain"]) of
        {ok, Bin} -> decode_result(Bin);
        Err -> Err
    catch
        error:Reason -> {error, {eopnotsupp, Reason}}
    end.

-compile({inline, linux_paths/0}).
linux_paths() ->
    ["/etc/ssl/certs/ca-certificates.crt",                %% Debian, Ubuntu, Gentoo
     "/etc/pki/tls/certs/ca-bundle.crt",                  %% Fedora, RHEL 6, Amazon Linux
     "/etc/ssl/ca-bundle.pem",                            %% OpenSUSE
     "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem", %% CentOS, RHEL 7
     "/etc/ssl/cert.pem"].                                %% Alpine Linux

-compile({inline, bsd_paths/0}).
bsd_paths() ->
    ["/etc/ssl/cert.pem",
     "/etc/openssl/certs/cacert.pem",   %% netbsd (if installed)
     "/etc/openssl/certs/ca-certificates.crt",
     "/usr/local/share/certs/ca-root-nss.crt"].

-compile({inline, sunos_paths/0}).
sunos_paths() ->
    ["/etc/certs/CA/",       %% Oracle Solaris, some older illumos distros
     "/etc/ssl/cacert.pem"]. %% OmniOS

-compile({inline, run_cmd/2}).
run_cmd(Cmd, Args) ->
    Port = open_port({spawn_executable, Cmd}, [{args, Args}, binary, exit_status, stderr_to_stdout]),
    unlink(Port),
    cmd_data(Port, <<>>).

cmd_data(Port, Acc) ->
    receive
        {Port, {data, Bin}} -> cmd_data(Port, <<Acc/binary, Bin/binary>>);
        {Port, {exit_status, 0}} -> {ok, Acc};
        {Port, {exit_status, Status}} -> {error, {eopnotsupp, Status, Acc}}
    end.

-ifndef(NEED_load_2).
-define(NEED_load_2, true).
-endif.
-endif.

-ifdef(NEED_load_2).
load([Path|Paths], Error) ->
    case dir_or_file(Path) of
        enoent -> load(Paths, Error);
        directory ->
            case load_from_files(Path) of
                ok -> ok;
                Err -> load(Paths, Err)
            end;
        file ->
            case load_from_file(Path) of
                ok -> ok;
                Err -> load(Paths, Err)
            end
    end;
load([], Error) -> Error.

-compile({inline, dir_or_file/1}).
dir_or_file(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} -> directory;
        {ok, #file_info{type = regular}} -> file;
        {ok, #file_info{}} ->  %% Link
            case filelib:is_dir(Path) of
                true -> directory;
                false -> file
            end;
        {error, _} -> enoent
    end.

-compile({inline, load_from_file/1}).
load_from_file(Path) when is_list(Path); is_binary(Path) ->
    try
        {ok, Binary} = file:read_file(Path),
        ok = decode_result(Binary)
    catch
        _:_Reason -> {error, enoent}
    end.

-compile({inline, load_from_files/1}).
load_from_files(Path) ->
    store(filelib:fold_files(Path, ".*\.pem", false,
                             fun(FileName, Acc) ->
                                 try
                                     {ok, Bin} = file:read_file(FileName),
                                     lists:foldr(fun({'Certificate', Der, not_encrypted}, A) ->
                                                     [#cert{der = Der, otp = public_key:pkix_decode_cert(Der, otp)}|A];
                                                    (_, A) -> A
                                                 end,
                                                 Acc, pubkey_pem:decode(Bin))
                                 catch
                                     _:_ -> Acc
                                 end
                             end,
                             [])).

-ifndef(NEED_decode_result_1).
-define(NEED_decode_result_1, true).
-endif.
-ifndef(NEED_store_1).
-define(NEED_store_1, true).
-endif.
-endif.

-ifdef(NEED_decode_result_1).
decode_result(Binary) ->
    MakeCert = fun({'Certificate', Der, not_encrypted}, Acc) ->
                   try public_key:pkix_decode_cert(Der, otp) of
                       Decoded -> [#cert{der = Der, otp = Decoded}|Acc]
                   catch 
                       _:_ -> Acc
                   end
               end,
    try
        store(lists:foldl(MakeCert, [], pubkey_pem:decode(Binary)))
    catch
        _:Reason -> {error, Reason}
    end.

-ifndef(NEED_store_1).
-define(NEED_store_1, true).
-endif.
-endif.

-ifdef(NEED_store_1).
store([]) -> {error, no_cacerts_found};
store(CaCerts) -> put(CaCerts).

-compile({inline, put/1}).
-ifdef(HAVE_persistent_term__put_2).
put(Value) -> persistent_term:put(pubkey_os_cacerts, Value).
-else.
put(Value) ->
    put(pubkey_os_cacerts, Value),
    ok.
-endif.
-endif.
