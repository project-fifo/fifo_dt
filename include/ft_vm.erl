%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_vm).

-include("sniffle.hrl").
-include("ft.hrl").

-define(G(N, F),
        getter(#sniffle_obj{val=S0}, N) ->
               F(S0)).

-define(G(E),
        E(H) -> riak_dt_lwwreg:value(H#?VM.E)).

-define(S(E),
        E({T, _ID}, V, H) ->
               {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?VM.E),
               H#?VM{E = V1}).

-define(S(N, F),
        set(TID, N, Value, D) ->
               F(TID, Value, D)).

-define(LOGLEN, 100).

-export([
         load/2, new/1, getter/2, merge/2, to_json/1, set/4
        ]).

-export([
         logs/1, log/4,
         uuid/1, uuid/3,
         state/1, state/3,
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
         groupings/1, add_grouping/3, remove_grouping/3
        ]).

-ignore_xref([
              logs/1, log/4,
              uuid/1, uuid/3,
              state/1, state/3,
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

new(_) ->
    #?VM{}.

?G(<<"uuid">>, uuid);
?G(<<"alias">>, alias);
?G(<<"state">>, state);
?G(<<"owner">>, owner);
?G(<<"dataset">>, dataset);
?G(<<"package">>, package);
?G(<<"hypervisor">>, hypervisor);
?G(<<"config">>, config);
?G(<<"services">>, services);
?G(<<"metadata">>, hypervisor).

?S(<<"uuid">>, uuid);
?S(<<"alias">>, alias);
?S(<<"state">>, state);
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

load({T, ID}, Sb) ->
    V = statebox:value(Sb),
    {ok, UUID} = jsxd:get([<<"uuid">>], V),
    {ok, Alias} = jsxd:get([<<"alias">>], V),
    {ok, Owner} = jsxd:get([<<"owner">>], V),
    {ok, Dataset} = jsxd:get([<<"dataset">>], V),
    {ok, Package} = jsxd:get([<<"package">>], V),
    {ok, Hypervisor} = jsxd:get([<<"hypervisor">>], V),
    {ok, State} = jsxd:get([<<"state">>], V),

    NetworkMap = jsxd:get([<<"network_mappings">>], [], V),
    Config = jsxd:get([<<"config">>], [], V),
    Info = jsxd:get([<<"info">>], [], V),
    Backups = jsxd:get([<<"backups">>], [], V),
    Snapshots = jsxd:get([<<"snapshots">>], [], V),
    Services = jsxd:get([<<"services">>], [], V),
    Metadata = jsxd:get([<<"metadata">>], [], V),
    Logs = jsxd:get([<<"log">>], [], V),

    UUID1 = ?NEW_LWW(UUID, T),
    State1 = ?NEW_LWW(State, T),
    Alias1 = ?NEW_LWW(Alias, T),
    Owner1 = ?NEW_LWW(Owner, T),
    Dataset1 = ?NEW_LWW(Dataset, T),
    Package1 = ?NEW_LWW(Package, T),
    Hypervisor1 = ?NEW_LWW(Hypervisor, T),

    NetworkMap1 = fifo_map:from_orddict(NetworkMap, ID, T),
    Config1 = fifo_map:from_orddict(Config, ID, T),
    Info1 = fifo_map:from_orddict(Info, ID, T),
    Backups1 = fifo_map:from_orddict(Backups, ID, T),
    Snapshots1 = fifo_map:from_orddict(Snapshots, ID, T),
    Services1 = fifo_map:from_orddict(Services, ID, T),
    Metadata1 = fifo_map:from_orddict(Metadata, ID, T),

    Logs1 = riak_dt_orswot:update(
              {add_all, Logs}, ID,
              riak_dt_orswot:new()),

    #?VM{
        uuid = UUID1,
        alias = Alias1,
        owner = Owner1,
        dataset = Dataset1,
        package = Package1,
        hypervisor = Hypervisor1,
        state = State1,

        logs = Logs1,

        network_map = NetworkMap1,
        config = Config1,
        info = Info1,
        backups = Backups1,
        snapshots = Snapshots1,
        services = Services1,
        metadata = Metadata1
       }.

to_json(V) ->
    M = lists:sort(
          [{ft_iprange:to_bin(IP), Range} ||
              {IP, Range} <- network_map(V)]),
    L = lists:sort(
          [[{<<"date">>, T},
            {<<"log">>, L}] ||
              {T, L} <- logs(V)]),
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

          logs = Logs1,
          groupings = Groupings1,

          network_map = NetworkMap1,
          config = Config1,
          info = Info1,
          backups = Backups1,
          snapshots = Snapshots1,
          services = Services1,
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

          logs = Logs2,
          groupings = Groupings2,

          network_map = NetworkMap2,
          config = Config2,
          info = Info2,
          backups = Backups2,
          snapshots = Snapshots2,
          services = Services2,
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

        logs = riak_dt_orswot:merge(Logs1, Logs2),
        groupings = riak_dt_orswot:merge(Groupings1, Groupings2),

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
