-module(otpbp_httpd).

-compile([{parse_transform, otpbp_pt}]).

-ifndef(HAVE_httpd__serve_1).
% OTP 27.0
-export([serve/1, start/1]).
-endif.

-ifndef(HAVE_httpd__serve_1).
start(Args) -> serve(Args).

serve(Args) -> argparse:run(Args, serve_cli(), #{progname => "erl -S httpd serve"}).

serve_cli() ->
    #{arguments => [#{name => directory,
                      type => string,
                      help => "Directory to serve data from.",
                      default => "."},
                    #{name => help,
                      type => boolean,
                      short => $h,
                      long => "-help",
                      help => "Show this description."},
                    #{name => port,
                      type => {integer, [{min, 0}, {max, 65535}]},
                      short => $p,
                      long => "-port",
                      default => 8000,
                      help => "Port to bind on. Use '0' for the OS to automatically "
                              "assign a port which can then be seen on server startup."},
                    #{name => address,
                      type => {custom, fun(Input) ->
                                           case inet:parse_address(Input) of
                                               {ok, Address} -> Address;
                                               {error, einval} -> error(badarg)
                                           end
                                       end},
                      short => $b,
                      long => "-bind",
                      default => {127, 0, 0, 1},
                      help => "IP address to listen on. Use 0.0.0.0 or :: for all interfaces."}],
      help => "Start a HTTP server serving files from DIRECTORY.",
      handler => fun do_serve/1}.

do_serve(#{help := true}) ->
    io:put_chars(argparse:help(serve_cli())),
    halt(0);
do_serve(#{address := Address, port := Port, directory := Path}) -> do_serve(Address, Port, Path).

-compile({inline, do_serve/3}).
do_serve({_, _, _, _} = Address, Port, Path) -> do_serve(Address, Port, Path, inet);
do_serve(Address, Port, Path) -> do_serve(Address, Port, Path, inet6).

do_serve(Address, Port, Path, IpFamily) ->
    AbsPath = string:trim(filename:absname(Path), trailing, "/."),
    inets:start(),
    {ok, Pid} = httpd:start_service([{bind_address, Address},
                                     {ipfamily, IpFamily},
                                     {document_root, AbsPath},
                                     {server_root, AbsPath},
                                     {directory_index, ["index.html"]},
                                     {port, Port},
                                     {mime_type, "application/octet-stream"},
                                     {mime_types, default_mime_types()},
                                     {modules, [mod_alias, mod_dir, mod_get]}]),
    % This is needed to support random port assignment (--port 0)
    [{port, ActualPort}] = httpd:info(Pid, [port]),
    io:fwrite("~nStarted HTTP server on http://~s:~w at ~s~n", [inet:ntoa(Address), ActualPort, AbsPath]),
    receive
        {From, shutdown} ->
            ok = httpd:stop_service(Pid),
            From ! done
    end.

-compile({inline, default_mime_types/0}).
default_mime_types() -> find_mime_types("/etc/mime.types").

-compile({inline, find_mime_types/1}).
find_mime_types(Path) ->
    case filelib:is_file(Path) of
        true -> Path;
        false ->
            [{"html", "text/html"}, {"htm", "text/html"}, {"js", "text/javascript"}, {"css","text/css"},
             {"gif", "image/gif"}, {"jpg", "image/jpeg"}, {"jpeg", "image/jpeg"}, {"png", "image/png"}]
    end.
-endif.
