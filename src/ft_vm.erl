%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_vm).
-behaviour(fifo_dt).

-include("ft_vm.hrl").
-include("ft_helper.hrl").

-define(LOGLEN, 100).

-export([fw_rules_to_json/1, fw_rule_to_json/1, json_to_fw_rule/1]).

-export([
         is_a/1, load/2, new/1, getter/2, merge/2, to_json/1
        ]).

-export([
         logs/1, log/4,
         uuid/1, uuid/3,
         created_at/1, created_at/3,
         vm_type/1, vm_type/3,
         state/1, state/3,
         deleting/1, deleting/3,
         creating/1, creating/3,
         alias/1, alias/3,
         owner/1, owner/3,
         dataset/1, dataset/3,
         package/1, package/3,
         hypervisor/1, hypervisor/3,
         docker/1, set_docker/3, set_docker/4,
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
              created_at/1, created_at/3,
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

-type vm() :: #{
          type           => vm,
          version        => pos_integer(),
          uuid           => riak_dt_lwwreg:lwwreg(),
          alias          => riak_dt_lwwreg:lwwreg(),
          owner          => riak_dt_lwwreg:lwwreg(),
          created_at     => riak_dt_lwwreg:lwwreg(),
          vm_type        => riak_dt_lwwreg:lwwreg(),

          dataset        => riak_dt_lwwreg:lwwreg(),
          package        => riak_dt_lwwreg:lwwreg(),
          hypervisor     => riak_dt_lwwreg:lwwreg(),
          network_map    => riak_dt_map:riak_dt_map(),

          config         => riak_dt_map:riak_dt_map(),
          info           => riak_dt_map:riak_dt_map(),
          services       => riak_dt_map:riak_dt_map(),
          backups        => riak_dt_map:riak_dt_map(),
          snapshots      => riak_dt_map:riak_dt_map(),

          logs           => riak_dt_orswot:orswot(),
          groupings      => riak_dt_orswot:orswot(),
          state          => riak_dt_lwwreg:lwwreg(),
          deleting       => riak_dt_lwwreg:lwwreg(),
          creating       => riak_dt_lwwreg:lwwreg(),

          fw_rules       => riak_dt_orswot:orswot(),
          metadata       => riak_dt_map:riak_dt_map()
         }.
-export_type([vm/0]).

?IS_A.

new(_) ->
    {ok, False} = ?NEW_LWW(false, 1),
    {ok, CT}    = ?NEW_LWW(0, 1),
    {ok, Type}  = ?NEW_LWW(zone, 1),
    #{
       type           => ?TYPE,
       version        => ?VERSION,

       uuid           => riak_dt_lwwreg:new(),
       alias          => riak_dt_lwwreg:new(),
       owner          => riak_dt_lwwreg:new(),
       created_at     => CT,
       vm_type        => Type,

       dataset        => riak_dt_lwwreg:new(),
       package        => riak_dt_lwwreg:new(),
       hypervisor     => riak_dt_lwwreg:new(),
       network_map    => riak_dt_map:new(),

       config         => riak_dt_map:new(),
       info           => riak_dt_map:new(),
       services       => riak_dt_map:new(),
       backups        => riak_dt_map:new(),
       snapshots      => riak_dt_map:new(),
       docker         => riak_dt_map:new(),

       logs           => riak_dt_orswot:new(),
       groupings      => riak_dt_orswot:new(),
       state          => riak_dt_lwwreg:new(),
       deleting       => False,
       creating       => False,

       fw_rules       => riak_dt_orswot:new(),
       metadata       => riak_dt_map:new()
     }.

load(_, #{version := ?VERSION, type := ?TYPE} = V) ->
    V;
load(TID, #vm_002{
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
            }) ->
    {ok, CT}    = ?NEW_LWW(0, 1),
    {ok, Type}  = ?NEW_LWW(zone, 1),

    V = #{
      type           => ?TYPE,
      version        => 1,
      uuid           => UUID,
      alias          => Alias,
      owner          => Owner,
      created_at     => CT,
      vm_type        => Type,
      docker         => riak_dt_map:new(),

      dataset        => Dataset,
      package        => Package,
      hypervisor     => HV,
      network_map    => NetMap,

      config         => Config,
      info           => Info,
      services       => Services,
      backups        => Backups,
      snapshots      => Snaps,

      logs           => Logs,
      groupings      => Groupings,
      fw_rules       => FWRules,
      state          => State,
      deleting       => Deleting,
      creating       => Creating,

      metadata       => Metadata
     },
    load(TID, V);
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

%% Dear dialyzer please kindly go fuck yourself.
-dialyzer({nowarn_function, to_json/1}).
-spec to_json(vm()) -> [{binary(), term()}].

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
     {<<"created_at">>, created_at(V)},
     {<<"creating">>, Creating},
     {<<"dataset">>, dataset(V)},
     {<<"deleting">>, deleting(V)},
     {<<"docker">>, docker(V)},
     {<<"fw_rules">>, fw_rules_to_json(fw_rules(V))},
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
     {<<"uuid">>, uuid(V)},
     {<<"vm_type">>, vm_type(V)}
    ].

merge(O = #{
        type := ?TYPE,
        uuid := UUID1,
        alias := Alias1,
        owner := Owner1,
        dataset := Dataset1,
        package := Package1,
        hypervisor := Hypervisor1,
        state := State1,
        deleting := Deleting1,
        creating := Creating1,
        created_at := CreatedAt1,
        vm_type := Type1,

        logs := Logs1,
        groupings := Groupings1,

        network_map := NetworkMap1,
        config := Config1,
        info := Info1,
        backups := Backups1,
        snapshots := Snapshots1,
        services := Services1,
        docker := Docker1,
        fw_rules := FWRules1,

        metadata := Metadata1
       },
      #{
         uuid := UUID2,
         alias := Alias2,
         owner := Owner2,
         dataset := Dataset2,
         package := Package2,
         hypervisor := Hypervisor2,
         state := State2,
         deleting := Deleting2,
         creating := Creating2,
         created_at := CreatedAt2,
         vm_type := Type2,


         logs := Logs2,
         groupings := Groupings2,

         network_map := NetworkMap2,
         config := Config2,
         info := Info2,
         backups := Backups2,
         snapshots := Snapshots2,
         services := Services2,
         fw_rules := FWRules2,
         docker := Docker2,
         metadata := Metadata2
       }) ->
    O#{
      uuid => riak_dt_lwwreg:merge(UUID1, UUID2),
      alias => riak_dt_lwwreg:merge(Alias1, Alias2),
      owner => riak_dt_lwwreg:merge(Owner1, Owner2),
      dataset => riak_dt_lwwreg:merge(Dataset1, Dataset2),
      package => riak_dt_lwwreg:merge(Package1, Package2),
      hypervisor => riak_dt_lwwreg:merge(Hypervisor1, Hypervisor2),
      created_at => riak_dt_lwwreg:merge(CreatedAt1, CreatedAt2),
      vm_type => riak_dt_lwwreg:merge(Type1, Type2),

      state => riak_dt_lwwreg:merge(State1, State2),
      deleting => riak_dt_lwwreg:merge(Deleting1, Deleting2),
      creating => riak_dt_lwwreg:merge(Creating1, Creating2),

      logs => riak_dt_orswot:merge(Logs1, Logs2),
      groupings => riak_dt_orswot:merge(Groupings1, Groupings2),
      fw_rules => riak_dt_orswot:merge(FWRules1, FWRules2),

      network_map => fifo_map:merge(NetworkMap1, NetworkMap2),
      config => fifo_map:merge(Config1, Config2),
      info => fifo_map:merge(Info1, Info2),
      backups => fifo_map:merge(Backups1, Backups2),
      snapshots => fifo_map:merge(Snapshots1, Snapshots2),
      services => fifo_map:merge(Services1, Services2),
      docker => fifo_map:merge(Docker1, Docker2),
      metadata => fifo_map:merge(Metadata1, Metadata2)
     }.


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
?G(<<"created_at">>, created_at);
?G(<<"vm_type">>, vm_type);
?G_JSX.


?REG_GET(created_at).
?REG_SET(created_at).

?REG_GET(vm_type).
?REG_SET(vm_type).

?REG_GET(uuid).
?REG_SET(uuid).
?REG_GET(alias).
?REG_SET(alias).
?REG_GET(state).
?REG_SET(state).
?REG_GET(deleting).
?REG_SET(deleting).
?REG_GET(creating).
?REG_SET(creating).
?REG_GET(owner).
?REG_SET(owner).
?REG_GET(dataset).
?REG_SET(dataset).
?REG_GET(package).
?REG_SET(package).
?REG_GET(hypervisor).
?REG_SET(hypervisor).

?MAP_GET(docker).
?MAP_SET_3(set_docker).
?MAP_SET_4(set_docker, docker).


?MAP_GET(config).
?MAP_SET_3(set_config).
?MAP_SET_4 (set_config, config).

?MAP_GET(info).
?MAP_SET_3(set_info).
?MAP_SET_4 (set_info, info).

?MAP_GET(services).
?MAP_SET_3(set_service).
?MAP_SET_4 (set_service, services).

?SET_GET(groupings).
?SET_ADD(add_grouping, groupings).
?SET_REM(remove_grouping, groupings).

?SET_GET(fw_rules).
?SET_ADD(add_fw_rule, fw_rules).
?SET_REM(remove_fw_rule, fw_rules).

?MAP_GET(network_map).
%% Dear dialyzer please kindly go fuck yourself.
-dialyzer({nowarn_function, set_network_map/4}).
?MAP_SET_4(set_network_map, network_map).

?MAP_GET(backups).
?MAP_SET_3(set_backup).
?MAP_SET_4 (set_backup, backups).

?MAP_GET(snapshots).
?MAP_SET_3(set_snapshot).
?MAP_SET_4 (set_snapshot, snapshots).

?SET_GET(logs).

log(ID, Time, Log, #{type := ?TYPE, logs := Logs} = Vm) ->
    {ok, L1} = riak_dt_orswot:update({add, {Time, Log}}, ID, Logs),
    V = riak_dt_orswot:value(L1),
    L2 = case length(V) of
             Len when Len >= ?LOGLEN ->
                 [V0 | _] = V,
                 {ok, Lx} = riak_dt_orswot:update({remove, V0}, ID, L1),
                 Lx;
             _ ->
                 L1
         end,
    Vm#{ logs => L2 }.

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

?META.
?SET_META_3.
?SET_META_4.
