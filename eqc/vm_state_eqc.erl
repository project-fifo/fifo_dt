-module(vm_state_eqc).

-import(ft_test_helper, [model_set_metadata/3, model_delete_metadata/2,
                         metadata/1, r/3, id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).

-define(V, ft_vm).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

vm() ->
    ?SIZED(Size, vm(Size+1)).

ip() ->
    choose(16#00000000, 16#FFFFFFFF).

fw_rule() ->
    {oneof([allow, block]), oneof([inbound, outbound]), target(),
     frequency([{2, udp_tcp()}, {1, icmp()}])}.

target() ->
    frequency(
      [{1, all},
       {100, {ip, ip()}},
       {100, {subnet, ip(), choose(1, 32)}}]).

udp_tcp() ->
    {oneof([tcp, udp]), ports()}.

ports()  ->
    frequency([{20, not_empty(list(pos_int()))},
               {1, all}]).
pos_int() ->
    ?SUCHTHAT(I, int(), I > 0).

icmp() ->
    {icmp, not_empty(list(code_and_type()))}.

code_and_type() ->
    oneof([{icmp, pos_int()}, {icmp, pos_int(), pos_int()}]).


vm(Size) ->
    ?LAZY(oneof([{call, ?V, new, [id(Size)]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [vm(Size - 1)],
                        oneof([
                               {call, ?V, load, [id(Size), O]},
                               %%{call, ?V, merge, [O, O]},

                               {call, ?V, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?V, alias, [id(Size), non_blank_string(), O]},
                               {call, ?V, state, [id(Size), non_blank_string(), O]},
                               {call, ?V, deleting, [id(Size), bool(), O]},
                               {call, ?V, owner, [id(Size), non_blank_string(), O]},
                               {call, ?V, dataset, [id(Size), non_blank_string(), O]},
                               {call, ?V, package, [id(Size), non_blank_string(), O]},
                               {call, ?V, hypervisor, [id(Size), non_blank_string(), O]},

                               {call, ?V, created_at, [id(Size), pos_int(), O]},
                               {call, ?V, created_by, [id(Size), non_blank_string(), O]},

                               {call, ?V, set_config, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_config, [id(Size), maybe_oneof(calc_map(set_config, O)), delete, O]},

                               {call, ?V, set_info, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_info, [id(Size), maybe_oneof(calc_map(set_info, O)), delete, O]},

                               {call, ?V, set_backup, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_backup, [id(Size), maybe_oneof(calc_map(set_backup, O)), delete, O]},

                               {call, ?V, set_snapshot, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_snapshot, [id(Size), maybe_oneof(calc_map(set_snapshot, O)), delete, O]},

                               {call, ?V, set_network_map, [id(Size), ip(), non_blank_string(), O]},
                               {call, ?V, set_network_map, [id(Size), maybe_oneof(calc_map(set_network_map, O), ip()), delete, O]},

                               {call, ?V, set_hostname_map, [id(Size), ip(), non_blank_string(), O]},
                               {call, ?V, set_hostname_map, [id(Size), maybe_oneof(calc_map(set_hostname_map, O), ip()), delete, O]},

                               {call, ?V, set_iprange_map, [id(Size), ip(), non_blank_string(), O]},
                               {call, ?V, set_iprange_map, [id(Size), maybe_oneof(calc_map(set_iprange_map, O), ip()), delete, O]},

                               {call, ?V, add_grouping, [id(Size), non_blank_string(), O]},
                               {call, ?V, remove_grouping, [id(Size), maybe_oneof(calc_groupings(O)), O]},

                               {call, ?V, add_fw_rule, [id(Size), fw_rule(), O]},
                               {call, ?V, remove_fw_rule, [id(Size), maybe_oneof(calc_fw_rules(O), fw_rule()), O]},

                               {call, ?V, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]}

                              ]))
                     || Size > 1])).

type() ->
    oneof([none, cluster, stack]).

calc_map(M, {call, _, M, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_map(M, U)));
calc_map(M, {call, _, M, [_, I, _K, U]}) ->
    [I | calc_map(M, U)];
calc_map(M, {call, _, _, P}) ->
    calc_map(M, lists:last(P));
calc_map(_M, _) ->
    [].

calc_groupings({call, _, remove_grouping, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_groupings(U)));
calc_groupings({call, _, add_grouping, [_, E, U]}) ->
    [E | calc_groupings(U)];
calc_groupings({call, _, _, P}) ->
    calc_groupings(lists:last(P));
calc_groupings(_) ->
    [].

calc_fw_rules({call, _, remove_fw_rule, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_fw_rules(U)));
calc_fw_rules({call, _, add_fw_rule, [_, E, U]}) ->
    [E | calc_fw_rules(U)];
calc_fw_rules({call, _, _, P}) ->
    calc_fw_rules(lists:last(P));
calc_fw_rules(_) ->
    [].


model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_alias(N, R) ->
    r(<<"alias">>, N, R).

model_state(N, R) ->
    r(<<"state">>, N, R).

model_deleting(N, R) ->
    r(<<"deleting">>, N, R).

model_owner(N, R) ->
    r(<<"owner">>, N, R).

model_dataset(N, R) ->
    r(<<"dataset">>, N, R).

model_package(N, R) ->
    r(<<"package">>, N, R).

model_hypervisor(N, R) ->
    r(<<"hypervisor">>, N, R).

model_created_at(N, R) ->
    r(<<"created_at">>, N, R).

model_created_by(N, R) ->
    r(<<"created_by">>, N, R).


model_set_config(K, V, U) ->
    r(<<"config">>, r(K, V, config(U)), U).

model_delete_config(K, U) ->
    r(<<"config">>, maps:remove(K, config(U)), U).

model_set_info(K, V, U) ->
    r(<<"info">>, r(K, V, info(U)), U).

model_delete_info(K, U) ->
    r(<<"info">>, maps:remove(K, info(U)), U).

model_set_backup(K, V, U) ->
    r(<<"backups">>, r(K, V, backups(U)), U).

model_delete_backup(K, U) ->
    r(<<"backups">>, maps:remove(K, backups(U)), U).

model_set_snapshot(K, V, U) ->
    r(<<"snapshots">>, r(K, V, snapshots(U)), U).

model_delete_snapshot(K, U) ->
    r(<<"snapshots">>, maps:remove(K, snapshots(U)), U).

model_set_network_map(K, V, U) ->
    r(<<"network_mappings">>, r(ft_iprange:to_bin(K), V, network_map(U)), U).

model_delete_network_map(K, U) ->
    r(<<"network_mappings">>, maps:remove(ft_iprange:to_bin(K), network_map(U)), U).

model_set_hostname_map(K, V, U) ->
    r(<<"hostname_mappings">>, r(ft_iprange:to_bin(K), V, hostname_map(U)), U).

model_delete_hostname_map(K, U) ->
    r(<<"hostname_mappings">>, maps:remove(ft_iprange:to_bin(K), hostname_map(U)), U).

model_set_iprange_map(K, V, U) ->
    r(<<"iprange_mappings">>, r(ft_iprange:to_bin(K), V, iprange_map(U)), U).

model_delete_iprange_map(K, U) ->
    r(<<"iprange_mappings">>, maps:remove(ft_iprange:to_bin(K), iprange_map(U)), U).

model_add_grouping(E, U) ->
    r(<<"groupings">>, lists:usort([E | get_groupings(U)]), U).

model_remove_grouping(E, U) ->
    r(<<"groupings">>, lists:delete(E, get_groupings(U)), U).

model_add_fw_rule(E, U) ->
    r(<<"fw_rules">>, lists:usort([ft_vm:fw_rule_to_json(E) | get_fw_rules(U)]), U).

model_remove_fw_rule(E, U) ->
    r(<<"fw_rules">>, lists:delete(ft_vm:fw_rule_to_json(E), get_fw_rules(U)), U).

model(R) ->
    ?V:to_json(R).

config(#{<<"config">> := M}) ->
    M.

info(#{<<"info">> := M}) ->
    M.

backups(#{<<"backups">> := M}) ->
    M.

snapshots(#{<<"snapshots">> := M}) ->
    M.

network_map(#{<<"network_mappings">> := M}) ->
    M.

hostname_map(#{<<"hostname_mappings">> := M}) ->
    M.

iprange_map(#{<<"iprange_mappings">> := M}) ->
    M.

get_groupings(#{<<"groupings">> := M}) ->
    M.

get_fw_rules(#{<<"fw_rules">> := M}) ->
    M.

prop_merge() ->
    ?FORALL(R,
            vm(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?V:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            vm(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?V:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?V:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_alias() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:alias(id(?BIG_TIME), N, Hv)) ==
                              model_alias(N, model(Hv)))
            end).

prop_state() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:state(id(?BIG_TIME), N, Hv)) ==
                              model_state(N, model(Hv)))
            end).

prop_deleting() ->
    ?FORALL({N, R},
            {bool(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:deleting(id(?BIG_TIME), N, Hv)) ==
                              model_deleting(N, model(Hv)))
            end).

prop_owner() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:owner(id(?BIG_TIME), N, Hv)) ==
                              model_owner(N, model(Hv)))
            end).

prop_dataset() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:dataset(id(?BIG_TIME), N, Hv)) ==
                              model_dataset(N, model(Hv)))
            end).

prop_package() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:package(id(?BIG_TIME), N, Hv)) ==
                              model_package(N, model(Hv)))
            end).

prop_hypervisor() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:hypervisor(id(?BIG_TIME), N, Hv)) ==
                              model_hypervisor(N, model(Hv)))
            end).

prop_created_by() ->
    ?FORALL({N, R},
            {non_blank_string(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:created_by(id(?BIG_TIME), N, Hv)) ==
                              model_created_by(N, model(Hv)))
            end).

prop_created_at() ->
    ?FORALL({N, R}, {pos_int(), vm()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?V:created_at(id(?BIG_TIME), N, Hv)) ==
                              model_created_at(N, model(Hv)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_config() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_config(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_config(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_config_list() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_config(id(?BIG_TIME), [{K, V}], Hv),
                M1 = model_set_config(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).
prop_set_config_map() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_config(id(?BIG_TIME), maps:from_list([{K, V}]), Hv),
                M1 = model_set_config(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_config() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_config, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_config(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_config(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_info() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_info(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_info(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_info() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_info, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_info(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_info(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_snapshot() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_snapshot(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_snapshot(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_snapshot() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_snapshot, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_snapshot(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_snapshot(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_backup() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_backup(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_backup(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_backup() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_backup, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_backup(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_backup(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_network_map() ->
    ?FORALL({IP, V, O}, {ip(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_network_map(id(?BIG_TIME), IP, V, Hv),
                M1 = model_set_network_map(IP, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_network_map() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_network_map, O), ip())}),
            begin
                Hv = eval(O),
                O1 = ?V:set_network_map(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_network_map(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_hostname_map() ->
    ?FORALL({IP, V, O}, {ip(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_hostname_map(id(?BIG_TIME), IP, V, Hv),
                M1 = model_set_hostname_map(IP, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_hostname_map() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_hostname_map, O), ip())}),
            begin
                Hv = eval(O),
                O1 = ?V:set_hostname_map(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_hostname_map(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_iprange_map() ->
    ?FORALL({IP, V, O}, {ip(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_iprange_map(id(?BIG_TIME), IP, V, Hv),
                M1 = model_set_iprange_map(IP, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_iprange_map() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_iprange_map, O), ip())}),
            begin
                Hv = eval(O),
                O1 = ?V:set_iprange_map(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_iprange_map(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_grouping() ->
    ?FORALL({E, O}, {non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:add_grouping(id(?BIG_TIME), E, Hv),
                M1 = model_add_grouping(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_grouping() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_groupings(O))}),
            begin
                Hv = eval(O),
                O1 = ?V:remove_grouping(id(?BIG_TIME), K, Hv),
                M1 = model_remove_grouping(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_fw_rule() ->
    ?FORALL({E, O}, {fw_rule(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:add_fw_rule(id(?BIG_TIME), E, Hv),
                M1 = model_add_fw_rule(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_fw_rule() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_fw_rules(O), fw_rule())}),
            begin
                Hv = eval(O),
                O1 = ?V:remove_fw_rule(id(?BIG_TIME), K, Hv),
                M1 = model_remove_fw_rule(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, vm(),
            jsone:encode(?V:to_json(eval(E))) /= #{}).

prop_fw_json() ->
    ?FORALL(R, fw_rule(),
            begin
                RJSON = ft_vm:fw_rule_to_json(R),
                R2 = ft_vm:json_to_fw_rule(RJSON),
                ?WHENFAIL(io:format(user, "~p -> ~p -> ~p~n",
                                    [R, RJSON, R2]),
                          R =:= R2)
            end).
