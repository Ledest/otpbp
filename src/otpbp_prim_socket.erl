-module(otpbp_prim_socket).

-ifndef(HAVE_prim_socket__enc_sockaddr_1).
% OTP 24.0
-export([enc_sockaddr/1]).
-endif.

-ifndef(HAVE_prim_socket__enc_sockaddr_1).
-define(ESOCK_SOCKADDR_IN_DEFAULTS, (#{family => inet, port => 0, addr => any})).
-define(ESOCK_SOCKADDR_IN6_DEFAULTS, (#{family => inet6, port => 0, addr => any, flowinfo => 0, scope_id => 0})).
-define(ESOCK_SOCKADDR_LOCAL_DEFAULTS, (#{family => local, path => <<>>})).
-define(ESOCK_SOCKADDR_UNSPEC_DEFAULTS, (#{family => unspec, addr => <<>>})).
-define(ESOCK_SOCKADDR_NATIVE_DEFAULTS, (#{family => 0, addr => <<>>})).

enc_sockaddr(#{family := inet} = SockAddr) -> merge_sockaddr(?ESOCK_SOCKADDR_IN_DEFAULTS, SockAddr);
enc_sockaddr(#{family := inet6} = SockAddr) -> merge_sockaddr(?ESOCK_SOCKADDR_IN6_DEFAULTS, SockAddr);
enc_sockaddr(#{family := local, path := Path} = SockAddr) when length(Path) =< 255 ->
    enc_sockaddr(SockAddr#{path => enc_path(Path)});
enc_sockaddr(#{family := local, path := Path} = SockAddr) when byte_size(Path) =< 255 ->
    merge_sockaddr(?ESOCK_SOCKADDR_LOCAL_DEFAULTS, SockAddr);
enc_sockaddr(#{family := local} = SockAddr) ->
    %% Neater than a function clause
    throw({invalid, {sockaddr, path, SockAddr}});
enc_sockaddr(#{family := unspec} = SockAddr) -> merge_sockaddr(?ESOCK_SOCKADDR_UNSPEC_DEFAULTS, SockAddr);
enc_sockaddr(#{family := Native} = SockAddr) when is_integer(Native) ->
    merge_sockaddr(?ESOCK_SOCKADDR_NATIVE_DEFAULTS, SockAddr);
enc_sockaddr(#{family := _} = SockAddr) -> SockAddr;
enc_sockaddr(SockAddr) ->
    is_map(SockAddr) andalso throw({invalid, {sockaddr, family, SockAddr}}),
    %% Neater than a function clause
    error({invalid, {sockaddr, SockAddr}}).

merge_sockaddr(Default, SockAddr) ->
    case maps:fold(fun(Key, _, Acc) ->
                       case maps:is_key(Key, Default) of
                          true -> Acc;
                          _false -> [Key|Acc]
                       end
                   end,
                   [], SockAddr) of
        [] -> maps:merge(Default, SockAddr);
        InvalidKeys -> throw({invalid, {sockaddr, {keys, InvalidKeys}, SockAddr}})
    end.

enc_path(Path) ->
    case unicode:characters_to_binary(Path, file:native_name_encoding()) of
        {T, _Bin, _Rest} when T =:= error; T =:= incomplete -> throw({invalid, {path, Path}});
        BinPath when is_binary(BinPath) -> BinPath
    end.
-endif.
