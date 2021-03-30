-module(otpbp_snmpa).

-ifndef(HAVE_snmpa__old_info_format_1).
% OTP < 24.0
-export([old_info_format/1]).
-endif.

-ifndef(HAVE_snmpa__old_info_format_1).
old_info_format(Info) when is_list(Info) ->
    {_, Vsns} = lists:keyfind(vsns, 1, Info),
    {_, {_, MibInfo}} = lists:keyfind(mib_server, 1, Info),
    [Vsns|lists:map(fun(K) ->
                        {_, V} = lists:keyfind(K, 1, MibInfo),
                        V
                    end, [subagents, loaded_mibs, tree_size_bytes, process_memory, db_memory])].
-endif.
