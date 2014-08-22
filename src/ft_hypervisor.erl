%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_hypervisor).

-include("ft.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(OBJ, ?HYPERVISOR).
-include("ft_helper.hrl").

-export([
         is_a/1,
         load/2,
         new/1,
         set/4,
         getter/2,
         to_json/1,
         merge/2
        ]).

-export([
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
         virtualisation/3
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
         etherstubs/1,
         host/1,
         networks/1,
         path/1,
         port/1,
         sysinfo/1,
         uuid/1,
         version/1,
         virtualisation/1
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

?IS_A.

new(_) ->
    {ok, Es} = ?NEW_LWW([], 0),
    {ok, Ns} = ?NEW_LWW([], 0),
    {ok, Ps} = ?NEW_LWW([], 0),
    {ok, Si} = ?NEW_LWW([], 0),
    {ok, Vi} = ?NEW_LWW([], 0),
    #?HYPERVISOR{
          etherstubs      = Es,
          networks        = Ns,
          path            = Ps,
          sysinfo         = Si,
          virtualisation  = Vi
        }
    #?HYPERVISOR{}.

alias(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.alias).

alias({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.alias),
    H#?HYPERVISOR{alias = V1}.

etherstubs(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.etherstubs).

etherstubs({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.etherstubs),
    H#?HYPERVISOR{etherstubs = V1}.

endpoint(H) ->
    {binary_to_list(host(H)), port(H)}.

host(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.host).

host({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.host),
    H#?HYPERVISOR{host = V1}.

networks(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.networks).

networks({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.networks),
    H#?HYPERVISOR{networks = V1}.

path(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.path).

path({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.path),
    H#?HYPERVISOR{path = V1}.

port(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.port).

port({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.port),
    H#?HYPERVISOR{port = V1}.

sysinfo(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.sysinfo).

sysinfo({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.sysinfo),
    H#?HYPERVISOR{sysinfo = V1}.

uuid(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.uuid).

uuid({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.uuid),
    H#?HYPERVISOR{uuid = V1}.

version(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.version).

version({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.version),
    H#?HYPERVISOR{version = V1}.

virtualisation(H) ->
    riak_dt_lwwreg:value(H#?HYPERVISOR.virtualisation).

virtualisation({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?HYPERVISOR.virtualisation),
    H#?HYPERVISOR{virtualisation = V1}.

pools(H) ->
    fifo_map:value(H#?HYPERVISOR.pools).

set_pool(ID, [{K, V} | R] , Vm) ->
    set_pool(ID, R, set_pool(ID, K, V, Vm));

set_pool(_ID, _, Vm) ->
    Vm.

set_pool({T, ID}, P, Value, H) when is_binary(P) ->
    set_pool({T, ID}, fifo_map:split_path(P), Value, H);

set_pool({_T, ID}, Attribute, delete, H) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, H#?HYPERVISOR.pools),
    H#?HYPERVISOR{pools = M1};

set_pool({T, ID}, Attribute, Value, H) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, H#?HYPERVISOR.pools),
    H#?HYPERVISOR{pools = M1}.

characteristics(H) ->
    fifo_map:value(H#?HYPERVISOR.characteristics).

set_characteristic(ID, [{K, V} | R] , Vm) ->
    set_characteristic(ID, R, set_characteristic(ID, K, V, Vm));

set_characteristic(_ID, _, Vm) ->
    Vm.

set_characteristic({T, ID}, P, Value, H) when is_binary(P) ->
    set_characteristic({T, ID}, fifo_map:split_path(P), Value, H);

set_characteristic({_T, ID}, Attribute, delete, H) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, H#?HYPERVISOR.characteristics),
    H#?HYPERVISOR{characteristics = M1};

set_characteristic({T, ID}, Attribute, Value, H) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, H#?HYPERVISOR.characteristics),
    H#?HYPERVISOR{characteristics = M1}.

metadata(H) ->
    fifo_map:value(H#?HYPERVISOR.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, H) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, H);

set_metadata({_T, ID}, Attribute, delete, H) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, H#?HYPERVISOR.metadata),
    H#?HYPERVISOR{metadata = M1};

set_metadata({T, ID}, Attribute, Value, H) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, H#?HYPERVISOR.metadata),
    H#?HYPERVISOR{metadata = M1}.

resources(H) ->
    fifo_map:value(H#?HYPERVISOR.resources).

set_resource(ID, [{K, V} | R] , Vm) ->
    set_resource(ID, R, set_resource(ID, K, V, Vm));

set_resource(_ID, _, Vm) ->
    Vm.

set_resource({_T, ID}, Attribute, delete, H) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, H#?HYPERVISOR.resources),
    H#?HYPERVISOR{resources = M1};

set_resource({T, ID}, Attribute, Value, H) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, H#?HYPERVISOR.resources),
    H#?HYPERVISOR{resources = M1}.

services(H) ->
    fifo_map:value(H#?HYPERVISOR.services).

set_service(ID, [{K, V} | R] , Vm) ->
    set_service(ID, R, set_service(ID, K, V, Vm));

set_service(_ID, _, Vm) ->
    Vm.

set_service({_T, ID}, Attribute, delete, H) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, H#?HYPERVISOR.services),
    H#?HYPERVISOR{services = M1};

set_service({T, ID}, Attribute, Value, H) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, H#?HYPERVISOR.services),
    H#?HYPERVISOR{services = M1}.

?G(<<"path">>, path);
?G(<<"uuid">>, uuid);
?G(<<"virtualisation">>, virtualisation);
?G(<<"etherstubs">>, etherstubs);
?G(<<"alias">>, alias);
?G(<<"resources">>, resources);
getter(O, <<"resources.", K/binary>>) ->
    V = ft_obj:val(O),
    Rs = resources(V),
    jsxd:get(K, <<>>, Rs);
getter(O, [<<"resources">> | K]) ->
    V = ft_obj:val(O),
    Rs = resources(V),
    jsxd:get(K, <<>>, Rs);
?G_JSX.

load(_, #?HYPERVISOR{} = H) ->
    H;

load({T, ID}, Sb) ->
    H = statebox:value(Sb),
    Characteristics = case jsxd:get([<<"characteristics">>],  H) of
                          {ok, [{}]} ->
                              [];
                          {ok, V} ->
                              V;
                          _ ->
                              []
                      end,
    {ok, Etherstubs} = jsxd:get([<<"etherstubs">>], H),
    {ok, Host} = jsxd:get([<<"host">>], H),
    Metadata = jsxd:get([<<"metadata">>], [], H),
    {ok, Alias} = jsxd:get([<<"alias">>], H),
    {ok, Networks} = jsxd:get([<<"networks">>], H),
    {ok, Pools} = jsxd:get([<<"pools">>], H),
    {ok, Port} = jsxd:get([<<"port">>], H),
    {ok, Resources} = jsxd:get([<<"resources">>], H),
    Services = jsxd:get([<<"services">>], [], H),
    {ok, Sysinfo} = jsxd:get([<<"sysinfo">>], H),
    {ok, UUID} = jsxd:get([<<"uuid">>], H),
    {ok, Version} = jsxd:get([<<"version">>], H),
    {ok, Virtualisation} = jsxd:get([<<"virtualisation">>], H),

    Characteristics1 = fifo_map:from_orddict(Characteristics, ID, T),
    {ok, Alias1} = ?NEW_LWW(Alias, T),
    {ok, Etherstubs1} = ?NEW_LWW(Etherstubs, T),
    {ok, Host1} = ?NEW_LWW(Host, T),
    Metadata1 = fifo_map:from_orddict(Metadata, ID, T),
    {ok, Networks1} = ?NEW_LWW(Networks, T),
    Pools1 = fifo_map:from_orddict(Pools, ID, T),
    {ok, Path} = ?NEW_LWW([{UUID, 1}], T),
    {ok, Port1} = ?NEW_LWW(Port, T),
    Resources1 = fifo_map:from_orddict(Resources, ID, T),
    Services1 = fifo_map:from_orddict(Services, ID, T),
    {ok, Sysinfo1} = ?NEW_LWW(Sysinfo, T),
    {ok, UUID1} = ?NEW_LWW(UUID, T),
    {ok, Version1} = ?NEW_LWW(Version, T),
    {ok, Virtualisation1} = ?NEW_LWW(Virtualisation, T),
    H1 = #hypervisor_0_1_0{
            characteristics = Characteristics1,
            alias = Alias1,
            etherstubs = Etherstubs1,
            host = Host1,
            metadata = Metadata1,
            networks = Networks1,
            path = Path,
            pools = Pools1,
            port = Port1,
            resources = Resources1,
            services = Services1,
            sysinfo = Sysinfo1,
            uuid = UUID1,
            version = Version1,
            virtualisation = Virtualisation1
           },
    load({T, ID}, H1).

to_json(H) ->
    [
     {<<"alias">>, alias(H)},
     {<<"characteristics">>, characteristics(H)},
     {<<"etherstubs">>, etherstubs(H)},
     {<<"host">>, host(H)},
     {<<"metadata">>, metadata(H)},
     {<<"networks">>, networks(H)},
     {<<"path">>, path_to_json(path(H))},
     {<<"pools">>, pools(H)},
     {<<"port">>, port(H)},
     {<<"resources">>, resources(H)},
     {<<"services">>, services(H)},
     {<<"sysinfo">>, sysinfo(H)},
     {<<"uuid">>, uuid(H)},
     {<<"version">>, version(H)},
     {<<"virtualisation">>, virtualisation(H)}
    ].

path_to_json(<<>>) ->
    [];
path_to_json(P) ->
    [[{<<"cost">>, C}, {<<"name">>, N}] || {N, C} <- P].

merge(#?HYPERVISOR{
          characteristics = Characteristics1,
          alias = Alias1,
          etherstubs = Etherstubs1,
          host = Host1,
          metadata = Metadata1,
          networks = Networks1,
          path = Path1,
          pools = Pools1,
          port = Port1,
          resources = Resources1,
          services = Services1,
          sysinfo = Sysinfo1,
          uuid = UUID1,
          version = Version1,
          virtualisation = Virtualisation1
         },
      #?HYPERVISOR{
          characteristics = Characteristics2,
          alias = Alias2,
          etherstubs = Etherstubs2,
          host = Host2,
          metadata = Metadata2,
          networks = Networks2,
          path = Path2,
          pools = Pools2,
          port = Port2,
          resources = Resources2,
          services = Services2,
          sysinfo = Sysinfo2,
          uuid = UUID2,
          version = Version2,
          virtualisation = Virtualisation2
         }
     ) ->
    #?HYPERVISOR{
        characteristics = fifo_map:merge(Characteristics1, Characteristics2),
        alias = riak_dt_lwwreg:merge(Alias1, Alias2),
        etherstubs = riak_dt_lwwreg:merge(Etherstubs1, Etherstubs2),
        host = riak_dt_lwwreg:merge(Host1, Host2),
        metadata = fifo_map:merge(Metadata1, Metadata2),
        networks = riak_dt_lwwreg:merge(Networks1, Networks2),
        path = riak_dt_lwwreg:merge(Path1, Path2),
        pools = fifo_map:merge(Pools1, Pools2),
        port = riak_dt_lwwreg:merge(Port1, Port2),
        resources = fifo_map:merge(Resources1, Resources2),
        services = fifo_map:merge(Services1, Services2),
        sysinfo = riak_dt_lwwreg:merge(Sysinfo1, Sysinfo2),
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        version = riak_dt_lwwreg:merge(Version1, Version2),
        virtualisation = riak_dt_lwwreg:merge(Virtualisation1, Virtualisation2)
       }.

set(ID, [K], V, H) ->
    set(ID, K, V, H);

set({T, ID}, <<"characteristics">>, V, H) ->
    H#?HYPERVISOR{characteristics = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"characteristics.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"characteristics">> | R], V, H) ->
    set_characteristic(ID, R, V, H);

set({T, ID}, <<"metadata">>, V, H) ->
    H#?HYPERVISOR{metadata = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H);

set({T, ID}, <<"services">>, V, H) ->
    H#?HYPERVISOR{services = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"services.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"services">> | R], V, H) ->
    set_service(ID, R, V, H);

set({T, ID}, <<"resources">>, V, H) ->
    H#?HYPERVISOR{resources = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"resources.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"resources">> | R], V, H) ->
    set_resource(ID, R, V, H);

set({T, ID}, <<"pools">>, V, H) ->
    H#?HYPERVISOR{pools = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"pools.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"pools">> | R], V, H) ->
    set_pool(ID, R, V, H);

set(ID, <<"alias">>, Value, Hypervisor) ->
    alias(ID, Value, Hypervisor);

set(ID, <<"etherstubs">>, Value, Hypervisor) ->
    etherstubs(ID, Value, Hypervisor);

set(ID, <<"host">>, Value, Hypervisor) ->
    host(ID, Value, Hypervisor);

set(ID, <<"networks">>, Value, Hypervisor) ->
    networks(ID, Value, Hypervisor);

set(ID, <<"path">>, Value, Hypervisor) ->
    path(ID, Value, Hypervisor);

set(ID, <<"port">>, Value, Hypervisor) ->
    port(ID, Value, Hypervisor);

set(ID, <<"sysinfo">>, Value, Hypervisor) ->
    sysinfo(ID, Value, Hypervisor);

set(ID, <<"uuid">>, Value, Hypervisor) ->
    uuid(ID, Value, Hypervisor);

set(ID, <<"version">>, Value, Hypervisor) ->
    version(ID, Value, Hypervisor);

set(ID, <<"virtualisation">>, Value, Hypervisor) ->
    virtualisation(ID, Value, Hypervisor).

-ifdef(TEST).
mkid() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    {(MegaSecs*1000000 + Secs)*1000000 + MicroSecs, test}.

nested_test() ->
    H = new(mkid()),
    H1 = set(mkid(), <<"pools">>, [{<<"a">>, 1}], H),
    JSON1 = to_json(H1),
    H2 = set(mkid(), [<<"pools">>, <<"a">>], 1, H),
    JSON2 = to_json(H2),
    H3 = set(mkid(), <<"pools.a">>, 1, H),
    JSON3 = to_json(H3),
    ?assertEqual({ok, [{<<"a">>, 1}]}, jsxd:get(<<"pools">>, JSON1)),
    ?assertEqual({ok, [{<<"a">>, 1}]}, jsxd:get(<<"pools">>, JSON2)),
    ?assertEqual({ok, [{<<"a">>, 1}]}, jsxd:get(<<"pools">>, JSON3)).

-endif.
