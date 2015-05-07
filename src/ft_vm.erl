%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_vm).

-include("ft_vm.hrl").
-define(OBJ, ?VM).
-include("ft_helper.hrl").

-define(LOGLEN, 100).

-export([fw_rules_to_json/1, fw_rule_to_json/1, json_to_fw_rule/1]).

-export([
         is_a/1, load/2, new/1, getter/2, merge/2, to_json/1, set/4
        ]).

-export([
         logs/1, log/4,
         uuid/1, uuid/3,
         state/1, state/3,
         deleting/1, deleting/3,
         creating/1, creating/3,
         alias/1, alias/3,
         owner/1, owner/3,
         dataset/1, dataset/3,
         package/1, package/3,
         hypervisor/1, hypervisor/3,
         config/1, set_config/3, set_config/4,
         info/1, set_info/3, set_info/4,
         backups/1, set_backup/3, set_backup/4,
         snapshots/1, set_snapshot/3, set_snapshot/4,
         services/1, set_service/3, set_service/4,
         metadata/1, set_metadata/3, set_metadata/4,
         network_map/1, set_network_map/4,
         groupings/1, add_grouping/3, remove_grouping/3,
         fw_rules/1, add_fw_rule/3, remove_fw_rule/3
        ]).

-ignore_xref([
              logs/1, log/4,
              uuid/1, uuid/3,
              state/1, state/3,
              deleting/1, deleting/3,
              creating/1, creating/3,
              alias/1, alias/3,
              owner/1, owner/3,
              dataset/1, dataset/3,
              package/1, package/3,
              hypervisor/1, hypervisor/3,
              config/1, set_config/4,
              info/1, set_info/4,
              network_map/1, set_network_map/4,

              config/1, set_config/3, set_config/4,
              info/1, set_info/3, set_info/4,
              backups/1, set_backup/3, set_backup/4,
              snapshots/1, set_snapshot/3, set_snapshot/4,
              services/1, set_service/3, set_service/4,
              metadata/1, set_metadata/3, set_metadata/4,
              groupings/1, add_grouping/3, remove_grouping/3
             ]).

-ignore_xref([load/2, set/4, getter/2, uuid/1, merge/2]).

-type vm() :: #?OBJ{}.
-export_type([vm/0]).

?IS_A.

new(_) ->
    {ok, False} = ?NEW_LWW(false, 1),
    #?OBJ{deleting = False,
          creating = False}.

?G(<<"uuid">>, uuid);
?G(<<"alias">>, alias);
?G(<<"state">>, state);
?G(<<"deleting">>, deleting);
?G(<<"creating">>, creating);
?G(<<"owner">>, owner);
?G(<<"dataset">>, dataset);
?G(<<"package">>, package);
?G(<<"hypervisor">>, hypervisor);
?G(<<"config">>, config);
?G(<<"services">>, services);
?G(<<"metadata">>, hypervisor);
?G_JSX.

?S(<<"uuid">>, uuid);
?S(<<"alias">>, alias);
?S(<<"state">>, state);
?S(<<"deleting">>, deleting);
?S(<<"creating">>, creating);
?S(<<"owner">>, owner);
?S(<<"dataset">>, dataset);
?S(<<"package">>, package);
?S(<<"hypervisor">>, hypervisor);

set(ID, K = <<"config.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, <<"config">>, V, H) ->
    set(ID, [<<"config">>], V, H);

set(ID, [<<"config">> | R], Vs, H) when is_list(Vs) ->
    lists:foldl(fun({K, V}, AccH) ->
                        set_config(ID, R ++ [K], V, AccH)
                end, H, Vs);

set(ID, K = <<"info.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, <<"info">>, V, H) ->
    set(ID, [<<"info">>], V, H);

set(ID, [<<"info">> | R], Vs, H) when is_list(Vs) ->
    lists:foldl(fun({K, V}, AccH) ->
                        set_info(ID, R ++ [K], V, AccH)
                end, H, Vs);

set(ID, K = <<"services.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, <<"services">>, V, H) ->
    set(ID, [<<"services">>], V, H);

set(ID, [<<"services">> | R], Vs, H) when is_list(Vs) ->
    lists:foldl(fun({K, V}, AccH) ->
                        set_service(ID, R ++ [K], V, AccH)
                end, H, Vs);

set(ID, [<<"services">> | R], V, H) ->
    set_service(ID, R, V, H);

set(ID, <<"network_mappings">>, V, H) ->
    set(ID, [<<"network_mappings">>], V, H);

set(ID, [<<"network_mappings">> | R], Vs, H) when is_list(Vs) ->
    lists:foldl(fun({K, V}, AccH) ->
                        set_network_map(ID, R ++ [K], V, AccH)
                end, H, Vs);

set(ID, [<<"network_mappings">> | R], V, H) ->
    set_service(ID, R, V, H);

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, <<"metadata">>, V, H) ->
    set(ID, [<<"metadata">>], V, H);

set(ID, [<<"metadata">> | R], Vs, H) when is_list(Vs) ->
    lists:foldl(fun({K, V}, AccH) ->
                        set_metadata(ID, R ++ [K], V, AccH)
                end, H, Vs);

set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H).


?G(uuid).
?S(uuid).
?G(alias).
?S(alias).
?G(state).
?S(state).
?G(deleting).
?S(deleting).
?G(creating).
?S(creating).
?G(owner).
?S(owner).
?G(dataset).
?S(dataset).
?G(package).
?S(package).
?G(hypervisor).
?S(hypervisor).

config(H) ->
    fifo_map:value(H#?VM.config).


set_config(ID, [{K, V} | R] , Vm) ->
    set_config(ID, R, set_config(ID, K, V, Vm));

set_config(_ID, _, Vm) ->
    Vm.

set_config({_T, _ID}, _P, [{}], Vm) ->
    Vm;

set_config({T, ID}, P, Value, Vm) when is_binary(P) ->
    set_config({T, ID}, fifo_map:split_path(P), Value, Vm);

set_config({_T, ID}, Attribute, delete, Vm) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Vm#?VM.config),
    Vm#?VM{config = M1};

set_config({T, ID}, Attribute, Value, Vm) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Vm#?VM.config),
    Vm#?VM{config = M1}.

info(H) ->
    fifo_map:value(H#?VM.info).

set_info(ID, [{K, V} | R] , Vm) ->
    set_info(ID, R, set_info(ID, K, V, Vm));

set_info(_ID, _, Vm) ->
    Vm.

set_info({_T, _ID}, _P, [{}], Vm) ->
    Vm;

set_info({T, ID}, P, Value, Vm) when is_binary(P) ->
    set_info({T, ID}, fifo_map:split_path(P), Value, Vm);

set_info({_T, ID}, Attribute, delete, Vm) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Vm#?VM.info),
    Vm#?VM{info = M1};

set_info({T, ID}, Attribute, Value, Vm) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Vm#?VM.info),
    Vm#?VM{info = M1}.

services(H) ->
    fifo_map:value(H#?VM.services).

set_service(ID, [{K, V} | R] , Vm) ->
    set_service(ID, R, set_service(ID, K, V, Vm));

set_service(_ID, _, Vm) ->
    Vm.

set_service({_T, _ID}, _P, [{}], Vm) ->
    Vm;

set_service({T, ID}, P, Value, Vm) when is_binary(P) ->
    set_service({T, ID}, fifo_map:split_path(P), Value, Vm);

set_service({_T, ID}, Attribute, delete, Vm) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Vm#?VM.services),
    Vm#?VM{services = M1};

set_service({T, ID}, Attribute, Value, Vm) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Vm#?VM.services),
    Vm#?VM{services = M1}.

groupings(H) ->
    riak_dt_orswot:value(H#?VM.groupings).

add_grouping({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?VM.groupings),
    H#?VM{groupings = O1}.

remove_grouping({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?VM.groupings) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?VM{groupings = O1}
    end.

fw_rules(H) ->
    riak_dt_orswot:value(H#?VM.fw_rules).

add_fw_rule({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?VM.fw_rules),
    H#?VM{fw_rules = O1}.

remove_fw_rule({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?VM.fw_rules) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?VM{fw_rules = O1}
    end.

metadata(H) ->
    fifo_map:value(H#?VM.metadata).

set_metadata(ID, [{K, V} | R] , Vm) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Vm));

set_metadata(_ID, _, Vm) ->
    Vm.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?VM.metadata),
    G#?VM{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?VM.metadata),
    G#?VM{metadata = M1}.

network_map(H) ->
    fifo_map:value(H#?VM.network_map).

set_network_map({T, ID}, IP, Value, User) when is_integer(IP) ->
    set_network_map({T, ID}, [IP], Value, User);

set_network_map({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?VM.network_map),
    G#?VM{network_map = M1};

set_network_map({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?VM.network_map),
    G#?VM{network_map = M1}.

backups(H) ->
    fifo_map:value(H#?VM.backups).

set_backup(ID, [{K, V} | R] , Vm) ->
    set_backup(ID, R, set_backup(ID, K, V, Vm));

set_backup(_ID, _, Vm) ->
    Vm.

set_backup({T, ID}, P, Value, User) when is_binary(P) ->
    set_backup({T, ID}, fifo_map:split_path(P), Value, User);

set_backup({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?VM.backups),
    G#?VM{backups = M1};

set_backup({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?VM.backups),
    G#?VM{backups = M1}.

snapshots(H) ->
    fifo_map:value(H#?VM.snapshots).

set_snapshot(ID, [{K, V} | R] , Vm) ->
    set_snapshot(ID, R, set_snapshot(ID, K, V, Vm));

set_snapshot(_ID, _, Vm) ->
    Vm.

set_snapshot({T, ID}, P, Value, User) when is_binary(P) ->
    set_snapshot({T, ID}, fifo_map:split_path(P), Value, User);

set_snapshot({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?VM.snapshots),
    G#?VM{snapshots = M1};

set_snapshot({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?VM.snapshots),
    G#?VM{snapshots = M1}.

load(_, #?VM{} = V) ->
    V;

load(TID, #vm_1{
             uuid           = UUID,
             alias          = Alias,
             owner          = Owner,

             dataset        = Dataset,
             package        = Package,
             hypervisor     = HV,
             network_map    = NetMap,

             config         = Config,
             info           = Info,
             services       = Services,
             backups        = Backups,
             snapshots      = Snaps,

             logs           = Logs,
             groupings      = Groupings,
             state          = State,
             deleting       = Deleting,
             fw_rules       = FWRules,

             metadata       = Metadata
            }) ->
    {ok, Creating} = ?NEW_LWW(false, 1),
    V = #vm_002{
           uuid           = UUID,
           alias          = Alias,
           owner          = Owner,

           dataset        = Dataset,
           package        = Package,
           hypervisor     = HV,
           network_map    = NetMap,

           config         = Config,
           info           = Info,
           services       = Services,
           backups        = Backups,
           snapshots      = Snaps,

           logs           = Logs,
           groupings      = Groupings,
           fw_rules       = FWRules,
           state          = State,
           deleting       = Deleting,
           creating       = Creating,

           metadata       = Metadata
          },
        load(TID, V);

load(TID, #vm_0{
             uuid           = UUID,
             alias          = Alias,
             owner          = Owner,

             dataset        = Dataset,
             package        = Package,
             hypervisor     = HV,
             network_map    = NetMap,

             config         = Config,
             info           = Info,
             services       = Services,
             backups        = Backups,
             snapshots      = Snaps,

             logs           = Logs,
             groupings      = Groupings,
             state          = State,
             fw_rules       = FWRules,

             metadata       = Metadata
            }) ->
    {ok, Deleting} = ?NEW_LWW(false, 1),
    V = #vm_1{
           uuid           = UUID,
           alias          = Alias,
           owner          = Owner,

           dataset        = Dataset,
           package        = Package,
           hypervisor     = HV,
           network_map    = NetMap,

           config         = Config,
           info           = Info,
           services       = Services,
           backups        = Backups,
           snapshots      = Snaps,

           logs           = Logs,
           groupings      = Groupings,
           fw_rules       = FWRules,
           state          = State,
           deleting       = Deleting,

           metadata       = Metadata
          },
        load(TID, V);

load(TID, #vm_0_1_1{
             uuid           = UUID,
             alias          = Alias,
             owner          = Owner,

             dataset        = Dataset,
             package        = Package,
             hypervisor     = HV,
             network_map    = NetMap,

             config         = Config,
             info           = Info,
             services       = Services,
             backups        = Backups,
             snapshots      = Snaps,

             logs           = Logs,
             groupings      = Groupings,
             state          = State,
             fw_rules       = FWRules,

             metadata       = Metadata
            }) ->
    V = #vm_0{
           uuid           = UUID,
           alias          = Alias,
           owner          = Owner,

           dataset        = Dataset,
           package        = Package,
           hypervisor     = HV,
           network_map    = fifo_dt:update_map(NetMap),

           config         = fifo_dt:update_map(Config),
           info           = fifo_dt:update_map(Info),
           services       = fifo_dt:update_map(Services),
           backups        = fifo_dt:update_map(Backups),
           snapshots      = fifo_dt:update_map(Snaps),

           logs           = fifo_dt:update_set(Logs),
           groupings      = fifo_dt:update_set(Groupings),
           fw_rules       = fifo_dt:update_set(FWRules),
           state          = State,

           metadata       = fifo_dt:update_map(Metadata)
          },
        load(TID, V);

load(TID, #vm_0_1_0{
             uuid           = UUID,
             alias          = Alias,
             owner          = Owner,

             dataset        = Dataset,
             package        = Package,
             hypervisor     = HV,
             network_map    = NetMap,

             config         = Config,
             info           = Info,
             services       = Services,
             backups        = Backups,
             snapshots      = Snaps,

             logs           = Logs,
             groupings      = Groupings,
             state          = State,

             metadata       = Metadata
            }) ->
    V = #vm_0_1_1{
           uuid           = UUID,
           alias          = Alias,
           owner          = Owner,

           dataset        = Dataset,
           package        = Package,
           hypervisor     = HV,
           network_map    = NetMap,

           config         = Config,
           info           = Info,
           services       = Services,
           backups        = Backups,
           snapshots      = Snaps,

           logs           = Logs,
           groupings      = Groupings,
           state          = State,

           metadata       = Metadata
          },
    load(TID, V).

-spec to_json(vm()) -> jsxd:object().

to_json(V) ->
    M = lists:sort(
          [{ft_iprange:to_bin(IP), Range} ||
              {IP, Range} <- network_map(V)]),
    L = lists:sort(
          [[{<<"date">>, T},
            {<<"log">>, L}] ||
              {T, L} <- logs(V)]),
    Creating = case creating(V) of
                   false ->
                       false;
                   _ ->
                       true
               end,
    [
     {<<"alias">>, alias(V)},
     {<<"backups">>, backups(V)},
     {<<"config">>, config(V)},
     {<<"dataset">>, dataset(V)},
     {<<"groupings">>, groupings(V)},
     {<<"hypervisor">>, hypervisor(V)},
     {<<"info">>, info(V)},
     {<<"log">>, L},
     {<<"metadata">>, metadata(V)},
     {<<"network_mappings">>, M},
     {<<"owner">>, owner(V)},
     {<<"package">>, package(V)},
     {<<"services">>, services(V)},
     {<<"snapshots">>, snapshots(V)},
     {<<"state">>, state(V)},
     {<<"deleting">>, deleting(V)},
     {<<"creating">>, Creating},
     {<<"fw_rules">>, fw_rules_to_json(fw_rules(V))},
     {<<"uuid">>, uuid(V)}
    ].

merge(#?VM{
          uuid = UUID1,
          alias = Alias1,
          owner = Owner1,
          dataset = Dataset1,
          package = Package1,
          hypervisor = Hypervisor1,
          state = State1,
          deleting = Deleting1,
          creating = Creating1,

          logs = Logs1,
          groupings = Groupings1,

          network_map = NetworkMap1,
          config = Config1,
          info = Info1,
          backups = Backups1,
          snapshots = Snapshots1,
          services = Services1,
          fw_rules = FWRules1,

          metadata = Metadata1
         },
      #?VM{
          uuid = UUID2,
          alias = Alias2,
          owner = Owner2,
          dataset = Dataset2,
          package = Package2,
          hypervisor = Hypervisor2,
          state = State2,
          deleting = Deleting2,
          creating = Creating2,

          logs = Logs2,
          groupings = Groupings2,

          network_map = NetworkMap2,
          config = Config2,
          info = Info2,
          backups = Backups2,
          snapshots = Snapshots2,
          services = Services2,
          fw_rules = FWRules2,
          metadata = Metadata2
         }) ->
    #?VM{
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        alias = riak_dt_lwwreg:merge(Alias1, Alias2),
        owner = riak_dt_lwwreg:merge(Owner1, Owner2),
        dataset = riak_dt_lwwreg:merge(Dataset1, Dataset2),
        package = riak_dt_lwwreg:merge(Package1, Package2),
        hypervisor = riak_dt_lwwreg:merge(Hypervisor1, Hypervisor2),
        state = riak_dt_lwwreg:merge(State1, State2),
        deleting = riak_dt_lwwreg:merge(Deleting1, Deleting2),
        creating = riak_dt_lwwreg:merge(Creating1, Creating2),

        logs = riak_dt_orswot:merge(Logs1, Logs2),
        groupings = riak_dt_orswot:merge(Groupings1, Groupings2),
        fw_rules = riak_dt_orswot:merge(FWRules1, FWRules2),

        network_map = fifo_map:merge(NetworkMap1, NetworkMap2),
        config = fifo_map:merge(Config1, Config2),
        info = fifo_map:merge(Info1, Info2),
        backups = fifo_map:merge(Backups1, Backups2),
        snapshots = fifo_map:merge(Snapshots1, Snapshots2),
        services = fifo_map:merge(Services1, Services2),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.


logs(VM) ->
    riak_dt_orswot:value(VM#?VM.logs).

log(ID, Time, Log, Vm) ->
    {ok, L1} = riak_dt_orswot:update({add, {Time, Log}}, ID, Vm#?VM.logs),
    V = riak_dt_orswot:value(L1),
    L2 = case length(V) of
             Len when Len >= ?LOGLEN ->
                 [V0 | _] = V,
                 {ok, Lx} = riak_dt_orswot:update({remove, V0}, ID, L1),
                 Lx;
             _ ->
                 L1
         end,
    Vm#?VM{ logs = L2 }.

fw_rules_to_json(Rs) ->
    lists:sort([fw_rule_to_json(R) || R <- Rs]).

fw_rule_to_json({Action, Direction, Target, {Proto, Filters}}) ->
    [
     {<<"action">>, a2b(Action)},
     {<<"direction">>, a2b(Direction)},
     {<<"filters">>, filters_to_json(Filters)},
     {<<"protocol">>, a2b(Proto)},
     {<<"target">>, target_to_json(Target)}
    ].

json_to_fw_rule([
                 {<<"action">>, Action},
                 {<<"direction">>, Direction},
                 {<<"filters">>, Filters},
                 {<<"protocol">>, Proto},
                 {<<"target">>, Target}
                ]) ->
    {json_to_action(Action),
     json_to_direction(Direction),
     json_to_target(Target),
     {json_to_proto(Proto),
      json_to_filters(Filters)}}.

json_to_action(<<"allow">>) ->
    allow;
json_to_action(<<"block">>) ->
    block.

json_to_direction(<<"inbound">>) ->
    inbound;
json_to_direction(<<"outbound">>) ->
    outbound.

json_to_target(<<"all">>) ->
    all;

json_to_target([{<<"ip">>, IP}]) ->
    {ip, ft_iprange:parse_bin(IP)};

json_to_target([{<<"mask">>, Mask},
                {<<"subnet">>, Subnet}]) when is_integer(Mask)->
    {subnet, ft_iprange:parse_bin(Subnet), Mask}.

target_to_json(all) ->
    <<"all">>;
target_to_json({ip, IP}) ->
    [{<<"ip">>, ft_iprange:to_bin(IP)}];
target_to_json({subnet, Subnet, Mask}) ->
    [{<<"mask">>, Mask},
     {<<"subnet">>, ft_iprange:to_bin(Subnet)}].

json_to_proto(<<"icmp">>) ->
    icmp;

json_to_proto(<<"tcp">>) ->
    tcp;

json_to_proto(<<"udp">>) ->
    udp.

a2b(A) ->
    atom_to_binary(A, utf8).

filters_to_json(L) when is_list(L) ->
    [filter_to_json(P) || P <- L];
filters_to_json(all) ->
    <<"all">>.


json_to_filters(L) when is_list(L), length(L) > 0 ->
    [json_to_filter(P) || P <- L];

json_to_filters(<<"all">>) ->
    all.


filter_to_json({icmp, Type}) ->
    [{<<"type">>, Type}];
filter_to_json({icmp, Type, Code}) ->
    [{<<"code">>, Code}, {<<"type">>, Type}];
filter_to_json(I) when is_integer(I) ->
    I.

json_to_filter(I) when is_integer(I) ->
    I;
json_to_filter([{<<"code">>, Code}, {<<"type">>, Type}]) ->
    {icmp, Type, Code};
json_to_filter([{<<"type">>, Type}]) ->
    {icmp, Type}.
