-module(otpbp_inet).

-ifndef(HAVE_inet__ipv4_mapped_ipv6_address_1).
% OTP 21.0
-export([ipv4_mapped_ipv6_address/1]).
-endif.

-ifndef(HAVE_inet__ipv4_mapped_ipv6_address_1).
ipv4_mapped_ipv6_address({D1, D2, D3, D4}) when (D1 bor D2 bor D3 bor D4) =< 16#FF ->
    {0, 0, 0, 0, 0, 16#FFFF, (D1 bsl 8) bor D2, (D3 bsl 8) bor D4};
ipv4_mapped_ipv6_address({D1, D2, D3, D4, D5, D6, D7, D8})
  when (D1 bor D2 bor D3 bor D4 bor D5 bor D6 bor D7 bor D8) =< 16#FFFF ->
    {D7 bsr 8, D7 band 16#FF, D8 bsr 8, D8 band 16#FF}.
-endif.
