%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_hypervisor).
-behaviour(fifo_dt).

-include("ft_hypervisor.hrl").
-include("ft_helper.hrl").

-export([
         is_a/1,
         load/2,
         new/1,
         getter/2,
         to_json/1,
         merge/2
        ]).

-export([
         last_seen/3,
         alias/3,
         etherstubs/3,
         host/3,
         resources/1, set_resource/3, set_resource/4,
         services/1, set_service/3, set_service/4,
         characteristics/1, set_characteristic/3, set_characteristic/4,
         metadata/1, set_metadata/3, set_metadata/4,
         pools/1, set_pool/3, set_pool/4,
         networks/3,
         path/3,
         port/3,
         endpoint/1,
         sysinfo/3,
         uuid/3,
         version/3,
         virtualisation/3,
         architecture/3
        ]).

-ignore_xref([
              merge/2,
              alias/3,
              etherstubs/3,
              host/3,
              set_pool/4,
              set_characteristic/4,
              set_metadata/4,
              networks/3,
              path/3,
              port/3,
              set_resource/4,
              set_service/4,
              sysinfo/3,
              uuid/3,
              version/3,
              virtualisation/3
             ]).

-export([
         alias/1,
         last_seen/1,
         etherstubs/1,
         host/1,
         networks/1,
         path/1,
         port/1,
         sysinfo/1,
         uuid/1,
         version/1,
         virtualisation/1,
         architecture/1
        ]).

-ignore_xref([
              characteristics/1,
              alias/1,
              etherstubs/1,
              host/1,
              metadata/1,
              networks/1,
              path/1,
              pools/1,
              port/1,
              resources/1,
              services/1,
              sysinfo/1,
              uuid/1,
              version/1,
              virtualisation/1
             ]).

-ignore_xref([to_json/1, load/2, set/4, getter/2, uuid/1]).


-type architecture() :: arm64 | x86.

-type hypervisor() :: #{
                  type            => ?TYPE,
                  version         => non_neg_integer(),
                  architecture    => riak_dt_lwwreg:lwwreg(),
                  last_seen       => riak_dt_lwwreg:lwwreg(),
                  characteristics => riak_dt_map:riak_dt_map(),
                  etherstubs      => riak_dt_lwwreg:lwwreg(),
                  host            => riak_dt_lwwreg:lwwreg(),
                  metadata        => riak_dt_map:riak_dt_map(),
                  alias           => riak_dt_lwwreg:lwwreg(),
                  networks        => riak_dt_lwwreg:lwwreg(),
                  path            => riak_dt_lwwreg:lwwreg(),
                  pools           => riak_dt_map:riak_dt_map(),
                  port            => riak_dt_lwwreg:lwwreg(),
                  resources       => riak_dt_map:riak_dt_map(),
                  services        => riak_dt_map:riak_dt_map(),
                  sysinfo         => riak_dt_lwwreg:lwwreg(),
                  uuid            => riak_dt_lwwreg:lwwreg(),
                  chunter_version => riak_dt_lwwreg:lwwreg(),
                  virtualisation  => riak_dt_lwwreg:lwwreg()
                 }.
-export_type([hypervisor/0, architecture/0]).


-spec is_a(any()) -> true | false.
?IS_A.

-spec new(fifo_dt:tid()) -> hypervisor().
new(_) ->
    {ok, Empty} = ?NEW_LWW([], 1),
    {ok, Zero} = ?NEW_LWW(0, 1),
    {ok, Arch} = ?NEW_LWW(undefined, 1),
    #{
       type            => ?TYPE,
       version         => ?VERSION,
       architecture    => Arch,
       last_seen       => Zero,
       characteristics => riak_dt_map:new(),
       etherstubs      => Empty,
       host            => riak_dt_lwwreg:new(),
       metadata        => riak_dt_map:new(),
       alias           => riak_dt_lwwreg:new(),
       networks        => Empty,
       path            => Empty,
       pools           => riak_dt_map:new(),
       port            => riak_dt_lwwreg:new(),
       resources       => riak_dt_map:new(),
       services        => riak_dt_map:new(),
       sysinfo         => Empty,
       uuid            => riak_dt_lwwreg:new(),
       chunter_version => riak_dt_lwwreg:new(),
       virtualisation  => Empty
     }.

load(_, #{type := ?TYPE, version := ?VERSION} = H) ->
    H;
load({T, _ID} = TID, #{type := ?TYPE, version := 0} = H) ->
    {ok, Arch} = ?NEW_LWW(x86, T),
    H1 = H#{architecture => Arch, version => 1},
    load(TID, H1);
load(TID, #hypervisor_0{
             characteristics = Characteristics,
             etherstubs      = Etherstubs,
             host            = Host,
             metadata        = Metadata,
             alias           = Alias,
             networks        = Networks,
             path            = Path,
             pools           = Pools,
             port            = Port,
             resources       = Rsources,
             services        = Services,
             sysinfo         = Sysinfo,
             uuid            = UUID,
             version         = Version,
             virtualisation  = Virt
            }) ->
    {ok, Zero} = ?NEW_LWW(0, 1),
    H1 = #{
      type            => ?TYPE,
      version         => 0,
      last_seen       => Zero,
      characteristics => Characteristics,
      etherstubs      => Etherstubs,
      host            => Host,
      metadata        => Metadata,
      alias           => Alias,
      networks        => Networks,
      path            => Path,
      pools           => Pools,
      port            => Port,
      resources       => Rsources,
      services        => Services,
      sysinfo         => Sysinfo,
      uuid            => UUID,
      chunter_version => Version,
      virtualisation  => Virt
     },
    load(TID, H1);
load(TID, #hypervisor_0_1_0{
             characteristics = Characteristics,
             etherstubs      = Etherstubs,
             host            = Host,
             metadata        = Metadata,
             alias           = Alias,
             networks        = Networks,
             path            = Path,
             pools           = Pools,
             port            = Port,
             resources       = Rsources,
             services        = Services,
             sysinfo         = Sysinfo,
             uuid            = UUID,
             version         = Version,
             virtualisation  = Virt
            }) ->
    H1 = #hypervisor_0{
            characteristics = fifo_dt:update_map(Characteristics),
            etherstubs      = Etherstubs,
            host            = Host,
            metadata        = fifo_dt:update_map(Metadata),
            alias           = Alias,
            networks        = Networks,
            path            = Path,
            pools           = fifo_dt:update_map(Pools),
            port            = Port,
            resources       = fifo_dt:update_map(Rsources),
            services        = fifo_dt:update_map(Services),
            sysinfo         = Sysinfo,
            uuid            = UUID,
            version         = Version,
            virtualisation  = Virt
           },
    load(TID, H1).

to_json(H) ->
    Arch = case architecture(H) of
               undefined ->
                   null;
               O ->
                   atom_to_binary(O, utf8)
           end,
    #{
       <<"alias">> => alias(H),
       <<"architecture">> => Arch,
       <<"characteristics">> => characteristics(H),
       <<"etherstubs">> => etherstubs(H),
       <<"host">> => host(H),
       <<"last_seen">> => last_seen(H),
       <<"metadata">> => metadata(H),
       <<"networks">> => networks(H),
       <<"path">> => path_to_json(path(H)),
       <<"pools">> => pools(H),
       <<"port">> => port(H),
       <<"resources">> => resources(H),
       <<"services">> => services(H),
       <<"sysinfo">> => sysinfo(H),
       <<"uuid">> => uuid(H),
       <<"version">> => version(H),
       <<"virtualisation">> => virtualisation(H)
     }.

path_to_json(P) ->
    [#{<<"cost">> => C, <<"name">> => N} || {N, C} <- P].

merge(H=#{
        type := ?TYPE,
        architecture := Arch1,
        last_seen := LastSeen1,
        characteristics := Characteristics1,
        alias := Alias1,
        etherstubs := Etherstubs1,
        host := Host1,
        metadata := Metadata1,
        networks := Networks1,
        path := Path1,
        pools := Pools1,
        port := Port1,
        resources := Resources1,
        services := Services1,
        sysinfo := Sysinfo1,
        uuid := UUID1,
        chunter_version := Version1,
        virtualisation := Virtualisation1
       },
      #{
         type := ?TYPE,
         last_seen := LastSeen2,
         architecture := Arch2,
         characteristics := Characteristics2,
         alias := Alias2,
         etherstubs := Etherstubs2,
         host := Host2,
         metadata := Metadata2,
         networks := Networks2,
         path := Path2,
         pools := Pools2,
         port := Port2,
         resources := Resources2,
         services := Services2,
         sysinfo := Sysinfo2,
         uuid := UUID2,
         chunter_version := Version2,
         virtualisation := Virtualisation2
       }) ->
    H#{
      architecture => riak_dt_lwwreg:merge(Arch1, Arch2),
      characteristics => fifo_map:merge(Characteristics1, Characteristics2),
      alias => riak_dt_lwwreg:merge(Alias1, Alias2),
      last_seen => riak_dt_lwwreg:merge(LastSeen1, LastSeen2),
      etherstubs => riak_dt_lwwreg:merge(Etherstubs1, Etherstubs2),
      host => riak_dt_lwwreg:merge(Host1, Host2),
      metadata => fifo_map:merge(Metadata1, Metadata2),
      networks => riak_dt_lwwreg:merge(Networks1, Networks2),
      path => riak_dt_lwwreg:merge(Path1, Path2),
      pools => fifo_map:merge(Pools1, Pools2),
      port => riak_dt_lwwreg:merge(Port1, Port2),
      resources => fifo_map:merge(Resources1, Resources2),
      services => fifo_map:merge(Services1, Services2),
      sysinfo => riak_dt_lwwreg:merge(Sysinfo1, Sysinfo2),
      uuid => riak_dt_lwwreg:merge(UUID1, UUID2),
      chunter_version => riak_dt_lwwreg:merge(Version1, Version2),
      virtualisation => riak_dt_lwwreg:merge(Virtualisation1, Virtualisation2)
     }.


-spec uuid(hypervisor()) -> binary().
?REG_GET(uuid).
-spec uuid(fifo_dt:tid(), binary(), hypervisor()) -> hypervisor().
?REG_SET_BIN(uuid).

-spec alias(hypervisor()) -> binary().
?REG_GET(alias).
-spec alias(fifo_dt:tid(), binary(), hypervisor()) -> hypervisor().
?REG_SET_BIN(alias).

-spec version(hypervisor()) -> binary().
version(#{type := ?TYPE, chunter_version := V}) ->
    riak_dt_lwwreg:value(V).
-spec version(fifo_dt:tid(), binary(), hypervisor()) -> hypervisor().
version({T, _ID}, V, O = #{type := ?TYPE, chunter_version := Reg0})
  when is_binary(V) ->
    ?REG_SET_BODY(chunter_version).

-spec last_seen(hypervisor()) -> pos_integer().
?REG_GET(last_seen).
-spec last_seen(fifo_dt:tid(), pos_integer(), hypervisor()) -> hypervisor().
?REG_SET_PI(last_seen).

-spec host(hypervisor()) -> binary().
?REG_GET(host).
-spec host(fifo_dt:tid(), binary(), hypervisor()) -> hypervisor().
?REG_SET_BIN(host).

-spec port(hypervisor()) -> pos_integer().
?REG_GET(port).
-spec port(fifo_dt:tid(), pos_integer(), hypervisor()) -> hypervisor().
?REG_SET_PI(port).

-spec virtualisation(hypervisor()) -> [binary()].
?REG_GET(virtualisation).
-spec virtualisation(fifo_dt:tid(), [binary()], hypervisor()) -> hypervisor().
?REG_SET(virtualisation).

-spec etherstubs(hypervisor()) -> [binary()].
?REG_GET(etherstubs).
-spec etherstubs(fifo_dt:tid(), [binary()], hypervisor()) -> hypervisor().
?REG_SET(etherstubs).

-spec networks(hypervisor()) -> [binary()].
?REG_GET(networks).
-spec networks(fifo_dt:tid(), [{non_neg_integer(), binary()}], hypervisor()) ->
                      hypervisor().
?REG_SET(networks).
-spec architecture(hypervisor()) -> undefined | architecture().
?REG_GET(architecture).
-spec architecture(fifo_dt:tid(), architecture(), hypervisor()) ->
                          hypervisor().
architecture({T, _ID}, V, O = #{type := ?TYPE, architecture := Reg0})
  when V =:= x86; V =:= arm64 ->
    {ok, Reg1} = riak_dt_lwwreg:update({assign, V, T}, none, Reg0),
    O#{architecture => Reg1}.


-spec path(hypervisor()) -> [{binary(), non_neg_integer()}].
%% We need to ensure we get an list
path(#{type := ?TYPE, path := V}) ->
    case riak_dt_lwwreg:value(V) of
        <<>> ->
            [];
        R ->
            R
    end.

-spec path(fifo_dt:tid(), [{binary(), non_neg_integer()}], hypervisor()) ->
                  hypervisor().
?REG_SET(path).

-spec sysinfo(hypervisor()) -> term().
?REG_GET(sysinfo).
-spec sysinfo(fifo_dt:tid(), term(), hypervisor()) -> hypervisor().
?REG_SET(sysinfo).


-spec endpoint(hypervisor()) -> {string(), pos_integer()}.
endpoint(H) ->
    {binary_to_list(host(H)), port(H)}.


?MAP_GET(pools).
?MAP_SET_3(set_pool).
?MAP_SET_4(set_pool, pools).

?MAP_GET(characteristics).
?MAP_SET_3(set_characteristic).
?MAP_SET_4(set_characteristic, characteristics).

?MAP_GET(resources).
?MAP_SET_3(set_resource).
?MAP_SET_4(set_resource, resources).

?MAP_GET(services).
?MAP_SET_3(set_service).
?MAP_SET_4(set_service, services).


?META.
?SET_META_3.
?SET_META_4.

?G(<<"path">>, path);
?G(<<"uuid">>, uuid);
?G(<<"virtualisation">>, virtualisation);
?G(<<"etherstubs">>, etherstubs);
?G(<<"alias">>, alias);
?G(<<"resources">>, resources);
getter(O, <<"architecture">>) ->
    V = ft_obj:val(O),
    atom_to_binary(architecture(V), utf8);
getter(O, <<"resources.", K/binary>>) ->
    V = ft_obj:val(O),
    Rs = resources(V),
    jsxd:get(K, <<>>, Rs);
getter(O, [<<"resources">> | K]) ->
    V = ft_obj:val(O),
    Rs = resources(V),
    jsxd:get(K, <<>>, Rs);
?G_JSX.
