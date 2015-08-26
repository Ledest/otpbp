-module(otpbp_inet).

-ifndef(HAVE_inet__ntoa_1).
-export([ntoa/1]).
-endif.

-ifndef(HAVE_inet__parse_address_1).
-export([parse_address/1]).
-endif.

-ifndef(HAVE_inet__parse_ipv4_address_1).
-export([parse_ipv4_address/1]).
-endif.

-ifndef(HAVE_inet__parse_ipv4strict_address_1).
-export([parse_ipv4strict_address/1]).
-endif.

-ifndef(HAVE_inet__parse_ipv6_address_1).
-export([parse_ipv6_address/1]).
-endif.

-ifndef(HAVE_inet__parse_ipv6strict_address_1).
-export([parse_ipv6strict_address/1]).
-endif.

-ifndef(HAVE_inet__parse_strict_address_1).
-export([parse_strict_address/1]).
-endif.

-ifndef(HAVE_inet__ntoa_1).
ntoa(Addr) -> inet_parse:ntoa(Addr).
-endif.

-ifndef(HAVE_inet__parse_address_1).
parse_address(Address) -> inet_parse:address(Address).
-endif.

-ifndef(HAVE_inet__parse_ipv4_address_1).
parse_ipv4_address(Address) -> inet_parse:ipv4_address(Address).
-endif.

-ifndef(HAVE_inet__parse_ipv4strict_address_1).
parse_ipv4strict_address(Address) -> inet_parse:ipv4strict_address(Address).
-endif.

-ifndef(HAVE_inet__parse_ipv6_address_1).
parse_ipv6_address(Address) -> inet_parse:ipv6_address(Address).
-endif.

-ifndef(HAVE_inet__parse_ipv6strict_address_1).
parse_ipv6strict_address(Address) -> inet_parse:ipv6strict_address(Address).
-endif.

-ifndef(HAVE_inet__parse_strict_address_1).
-ifndef(HAVE_inet_parse__strict_address_1).
parse_strict_address(Address) -> otpbp_inet_parse:strict_address(Address).
-else.
parse_strict_address(Address) -> inet_parse:strict_address(Address).
-endif.
-endif.
