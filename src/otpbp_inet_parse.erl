-module(otpbp_inet_parse).

-ifndef(HAVE_inet_parse__strict_address_1).
-export([strict_address/1]).
-endif.

-ifndef(HAVE_inet_parse__strict_address_1).
strict_address(Cs) when is_list(Cs) ->
    case inet_parse:ipv4strict_address(Cs) of
        {ok, IP} -> {ok, IP};
        _ -> inet_parse:ipv6strict_address(Cs)
    end;
strict_address(_) -> {error, einval}.
-endif.
