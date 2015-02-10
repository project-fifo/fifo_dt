-module(vm_state_eqc).

-ifdef(TEST).
-ifdef(EQC).

-import(ft_test_helper, [id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc_fsm.hrl").
-include_lib("fqc/include/fqc.hrl").

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
    {oneof([allow, block]), oneof([inbound, outbound]), all,
     {oneof([tcp, udp]), not_empty(pos_int())}}.

pos_int() ->
    ?SUCHTHAT(I, int(), I > 0).

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
                               {call, ?V, owner, [id(Size), non_blank_string(), O]},
                               {call, ?V, dataset, [id(Size), non_blank_string(), O]},
                               {call, ?V, package, [id(Size), non_blank_string(), O]},
                               {call, ?V, hypervisor, [id(Size), non_blank_string(), O]},

                               {call, ?V, set_config, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_config, [id(Size), maybe_oneof(calc_map(set_config, O)), delete, O]},

                               {call, ?V, set_info, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_info, [id(Size), maybe_oneof(calc_map(set_info, O)), delete, O]},

                               {call, ?V, set_backup, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_backup, [id(Size), maybe_oneof(calc_map(set_backup, O)), delete, O]},

                               {call, ?V, set_snapshot, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_snapshot, [id(Size), maybe_oneof(calc_map(set_snapshot, O)), delete, O]},

                               {call, ?V, set_service, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?V, set_service, [id(Size), maybe_oneof(calc_map(set_service, O)), delete, O]},

                               {call, ?V, set_network_map, [id(Size), ip(), non_blank_string(), O]},
                               {call, ?V, set_network_map, [id(Size), maybe_oneof(calc_map(set_network_map, O), ip()), delete, O]},

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

r(K, V, U) ->
    lists:keystore(K, 1, U, {K, V}).

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_alias(N, R) ->
    r(<<"alias">>, N, R).

model_state(N, R) ->
    r(<<"state">>, N, R).

model_owner(N, R) ->
    r(<<"owner">>, N, R).

model_dataset(N, R) ->
    r(<<"dataset">>, N, R).

model_package(N, R) ->
    r(<<"package">>, N, R).

model_hypervisor(N, R) ->
    r(<<"hypervisor">>, N, R).

model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).

model_set_config(K, V, U) ->
    r(<<"config">>, lists:usort(r(K, V, config(U))), U).

model_delete_config(K, U) ->
    r(<<"config">>, lists:keydelete(K, 1, config(U)), U).

model_set_info(K, V, U) ->
    r(<<"info">>, lists:usort(r(K, V, info(U))), U).

model_delete_info(K, U) ->
    r(<<"info">>, lists:keydelete(K, 1, info(U)), U).

model_set_backup(K, V, U) ->
    r(<<"backups">>, lists:usort(r(K, V, backups(U))), U).

model_delete_backup(K, U) ->
    r(<<"backups">>, lists:keydelete(K, 1, backups(U)), U).

model_set_snapshot(K, V, U) ->
    r(<<"snapshots">>, lists:usort(r(K, V, snapshots(U))), U).

model_delete_snapshot(K, U) ->
    r(<<"snapshots">>, lists:keydelete(K, 1, snapshots(U)), U).

model_set_service(K, V, U) ->
    r(<<"services">>, lists:usort(r(K, V, services(U))), U).

model_delete_service(K, U) ->
    r(<<"services">>, lists:keydelete(K, 1, services(U)), U).

model_set_network_map(K, V, U) ->
    r(<<"network_mappings">>, lists:usort(r(ft_iprange:to_bin(K), V, network_map(U))), U).

model_delete_network_map(K, U) ->
    r(<<"network_mappings">>, lists:keydelete(ft_iprange:to_bin(K), 1, network_map(U)), U).

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

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

config(U) ->
    {<<"config">>, M} = lists:keyfind(<<"config">>, 1, U),
    M.

info(U) ->
    {<<"info">>, M} = lists:keyfind(<<"info">>, 1, U),
    M.

backups(U) ->
    {<<"backups">>, M} = lists:keyfind(<<"backups">>, 1, U),
    M.

snapshots(U) ->
    {<<"snapshots">>, M} = lists:keyfind(<<"snapshots">>, 1, U),
    M.

services(U) ->
    {<<"services">>, M} = lists:keyfind(<<"services">>, 1, U),
    M.

network_map(U) ->
    {<<"network_mappings">>, M} = lists:keyfind(<<"network_mappings">>, 1, U),
    M.

get_groupings(U) ->
    {<<"groupings">>, M} = lists:keyfind(<<"groupings">>, 1, U),
    M.

get_fw_rules(U) ->
    {<<"fw_rules">>, M} = lists:keyfind(<<"fw_rules">>, 1, U),
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


prop_set_service() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), vm()},
            begin
                Hv = eval(O),
                O1 = ?V:set_service(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_service(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_service() ->
    ?FORALL({O, K}, ?LET(O, vm(), {O, maybe_oneof(calc_map(set_service, O))}),
            begin
                Hv = eval(O),
                O1 = ?V:set_service(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_service(K, model(Hv)),
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
            jsx:encode(?V:to_json(eval(E))) /= []).

-endif.
-endif.
