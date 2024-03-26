-module(otpbp_diameter_config).

-ifndef(HAVE_diameter_config__which_transports_0).
% OTP 27.0
-export([which_transports/0]).
-endif.
-ifndef(HAVE_diameter_config__which_transports_1).
% OTP 27.0
-export([which_transports/1]).
-endif.

-ifndef(HAVE_diameter_config__which_transports_0).
-ifndef(NEED_record_transport).
-define(NEED_record_transport, true).
-endif.
-endif.
-ifndef(HAVE_diameter_config__which_transports_1).
-ifndef(NEED_record_transport).
-define(NEED_record_transport, true).
-endif.
-endif.

-ifdef(NEED_record_transport).
-type match(T) :: T | '_' | '$1' | '$2' | '$3' | '$4'.

-record(transport, {service, %% name
                    ref = make_ref() :: match(reference()),
                    type :: match(connect|listen),
                    options :: match(list())}).
-endif.

-ifndef(HAVE_diameter_config__which_transports_0).
which_transports() ->
    lists:map(fun({Ref, Type, Service}) -> #{ref => Ref, type => Type, service => Service} end,
              ets:select(diameter_config,
                         [{#transport{service = '$1', ref = '$2', type = '$3', _ = '_'}, [], [{{'$2', '$3', '$1'}}]}])).
-endif.

-ifndef(HAVE_diameter_config__which_transports_1).
which_transports(SvcName) ->
    lists:map(fun({Ref, Type, Service}) -> #{ref => Ref, type => Type, service => Service} end,
              ets:select(diameter_config,
                         [{#transport{service = '$1', ref = '$2', type = '$3', _ = '_'},
                           [{'=:=', '$1', {const, SvcName}}],
                           [{{'$2', '$3'}}]}])).
-endif.
