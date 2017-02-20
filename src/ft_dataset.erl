%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_dataset).
-behaviour(fifo_dt).

-include("ft_dataset.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         getter/2,
         to_json/1,
         merge/2,
         is_a/1
        ]).

-ignore_xref([
              load/2, load/1, getter/2, merge/2
             ]).

-export([
         uuid/1, uuid/3,
         type/1, type/3,
         zone_type/1, zone_type/3,
         description/1, description/3,
         disk_driver/1, disk_driver/3,
         homepage/1, homepage/3,
         image_size/1, image_size/3,
         name/1, name/3,
         networks/1, add_network/3, remove_network/3,
         nic_driver/1, nic_driver/3,
         os/1, os/3,
         sha1/1, sha1/3,
         users/1, users/3,
         version/1, version/3,
         status/1, status/3,
         imported/1, imported/3,
         kernel_version/1, kernel_version/3,
         metadata/1, set_metadata/3, set_metadata/4,
         requirements/1, add_requirement/3, remove_requirement/3

        ]).

-ignore_xref([
              type/1, type/3,
              zone_type/1, zone_type/3,
              uuid/1, uuid/3,
              imported/1, imported/3,
              status/1, status/3,
              description/1, description/3,
              disk_driver/1, disk_driver/3,
              homepage/1, homepage/3,
              image_size/1, image_size/3,
              name/1, name/3,
              networks/1, add_network/3, remove_network/3,
              nic_driver/1, nic_driver/3,
              os/1, os/3,
              users/1, users/3,
              version/1, version/3,
              status/1, status/3,
              imported/1, imported/3,
              requirements/1, add_requirement/3, remove_requirement/3,
              metadata/1, set_metadata/3, set_metadata/4

             ]).

-type dataset() :: #{
               type            => ?TYPE,
               version         => non_neg_integer(),
               uuid            => riak_dt_lwwreg:lwwreg(),
               dataset_type    => riak_dt_lwwreg:lwwreg(),
               zone_type       => riak_dt_lwwreg:lwwreg(),

               status          => riak_dt_lwwreg:lwwreg(),
               imported        => riak_dt_lwwreg:lwwreg(),
               requirements    => riak_dt_orswot:orswot(),
               networks        => riak_dt_orswot:orswot(),
               metadata        => riak_dt_map:riak_dt_map(),

               description     => riak_dt_lwwreg:lwwreg(),
               disk_driver     => riak_dt_lwwreg:lwwreg(),
               homepage        => riak_dt_lwwreg:lwwreg(),
               image_size      => riak_dt_lwwreg:lwwreg(),
               name            => riak_dt_lwwreg:lwwreg(),
               nic_driver      => riak_dt_lwwreg:lwwreg(),
               os              => riak_dt_lwwreg:lwwreg(),
               users           => riak_dt_lwwreg:lwwreg(),
               sha1            => riak_dt_lwwreg:lwwreg(),
               dataset_version => riak_dt_lwwreg:lwwreg(),
               kernel_version  => riak_dt_lwwreg:lwwreg()
              }.
-export_type([dataset/0]).

?IS_A.

new({_T, _ID}) ->
    {ok, Imported} = ?NEW_LWW(0, 1),
    {ok, Status} = ?NEW_LWW(<<"new">>, 1),
    #{
       type            => ?TYPE,
       version         => ?VERSION,
       uuid            => riak_dt_lwwreg:new(),
       dataset_type    => riak_dt_lwwreg:new(),
       zone_type       => riak_dt_lwwreg:new(),

       status          => Status,
       imported        => Imported,
       requirements    => riak_dt_orswot:new(),
       networks        => riak_dt_orswot:new(),
       metadata        => riak_dt_map:new(),

       description     => riak_dt_lwwreg:new(),
       disk_driver     => riak_dt_lwwreg:new(),
       homepage        => riak_dt_lwwreg:new(),
       image_size      => riak_dt_lwwreg:new(),
       name            => riak_dt_lwwreg:new(),
       nic_driver      => riak_dt_lwwreg:new(),
       os              => riak_dt_lwwreg:new(),
       users           => riak_dt_lwwreg:new(),
       sha1            => riak_dt_lwwreg:new(),
       dataset_version => riak_dt_lwwreg:new(),
       kernel_version  => riak_dt_lwwreg:new()
     }.

?G(<<"uuid">>, uuid);
?G(<<"kernel_version">>, kernel_version);
?G(<<"type">>, type);
?G(<<"zone_type">>, zone_type);
?G(<<"status">>, status);
?G(<<"imported">>, imported);

?G(<<"description">>, description);
?G(<<"disk_driver">>, disk_driver);
?G(<<"homepage">>, homepage);
?G(<<"image_size">>, image_size);
?G(<<"name">>, name);
?G(<<"networks">>, networks);
?G(<<"nic_driver">>, nic_driver);
?G(<<"os">>, os);
?G(<<"users">>, users);
?G(<<"version">>, version);
?G_JSX.

?REG_GET(uuid).
?REG_SET(uuid).

-spec type(dataset()) -> kvm | zone | <<>>.
type(#{type := ?TYPE, dataset_type := T}) ->
    riak_dt_lwwreg:value(T).
type(ID, <<"kvm">>, H) ->
    type(ID, kvm, H);
type(ID, <<"zone">>, H) ->
    type(ID, zone, H);
type({T, _ID}, V, H = #{type := ?TYPE, dataset_type := Old})
  when V =:= kvm;
       V =:= zone ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, Old),
    H#{dataset_type => V1}.

-spec version(dataset()) -> binary().
version(#{version := ?VERSION, dataset_version := V}) ->
    riak_dt_lwwreg:value(V).
-spec version(fifo:tid(), binary(), dataset()) -> dataset().
version({T, _ID}, V, H = #{version := ?VERSION, dataset_version := Old}) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, Old),
    H#{dataset_version => V1}.

?REG_GET(zone_type).
zone_type(ID, <<"docker">>, H) ->
    zone_type(ID, docker, H);
zone_type(ID, <<"lx">>, H) ->
    zone_type(ID, lx, H);
zone_type(ID, <<"ipkg">>, H) ->
    zone_type(ID, ipkg, H);
zone_type(ID, <<"lipkg">>, H) ->
    zone_type(ID, lipkg, H);
zone_type({T, _ID}, V, H = #{type := ?TYPE, zone_type := Old})
  when V =:= lx;
       V =:= docker;
       V =:= ipkg;
       V =:= lipkg ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, Old),
    H#{zone_type => V1}.

?REG_GET(status).
?REG_SET(status).
?REG_GET(imported).
?REG_SET(imported).

?REG_GET(description).
?REG_SET(description).
?REG_GET(disk_driver).
?REG_SET(disk_driver).
?REG_GET(homepage).
?REG_SET(homepage).
?REG_GET(image_size).
?REG_SET(image_size).
?REG_GET(name).
?REG_SET(name).
?REG_GET(nic_driver).
?REG_SET(nic_driver).
?REG_GET(os).
?REG_SET(os).
?REG_GET(sha1).
?REG_SET(sha1).
?REG_GET(users).
?REG_SET(users).
?REG_GET(kernel_version).
?REG_SET(kernel_version).

-type requirement() :: term().

-spec requirements(dataset()) -> [requirement()].
?SET_GET(requirements).
-spec add_requirement(fifo_dt:tid(), term(), dataset()) -> dataset().
?SET_ADD(add_requirement, requirements).
-spec remove_requirement(fifo_dt:tid(), term(), dataset()) -> dataset().
?SET_REM(remove_requirement, requirements).

-type network() :: {binary(), binary()}.
-spec networks(dataset()) -> [network()].
?SET_GET(networks).
-spec add_network(fifo_dt:tid(), term(), dataset()) -> dataset().
?SET_ADD(add_network, networks).
-spec remove_network(fifo_dt:tid(), term(), dataset()) -> dataset().
?SET_REM(remove_network, networks).

?META.
?SET_META_3.
?SET_META_4.


-define(V(R), riak_dt_lwwreg:value(R)).

to_json(D) ->
    Type = case type(D) of
               <<>> -> <<>>;
               kvm -> <<"kvm">>;
               zone -> <<"zone">>
           end,
    Vs = [
          {<<"description">>, fun description/1},
          {<<"disk_driver">>, fun disk_driver/1},
          {<<"homepage">>, fun homepage/1},
          {<<"image_size">>, fun image_size/1},
          {<<"imported">>, fun imported/1},
          {<<"metadata">>, fun metadata/1},
          {<<"name">>, fun name/1},
          {<<"networks">>,
           fun(D1) ->
                   net_to_json(networks(D1))
           end},
          {<<"nic_driver">>, fun nic_driver/1},
          {<<"os">>, fun os/1},
          {<<"requirements">>,
           fun (D1) ->
                   [fifo_dt:req2js(R) || R <- requirements(D1)]
           end},
          {<<"sha1">>, fun sha1/1},
          {<<"status">>, fun status/1},
          {<<"users">>, fun users/1},
          {<<"uuid">>, fun uuid/1},
          {<<"kernel_version">>, fun kernel_version/1},
          {<<"zone_type">>, fun zone_type/1},
          {<<"version">>, fun version/1}
         ],
    add(Vs, D, #{<<"type">> => Type}).

add([], _, D) ->
    D;
add([{<<"zone_type">> = N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        ipkg ->
            add(R, In, jsxd:set(N, <<"ipkg">>, D));
        lipkg ->
            add(R, In, jsxd:set(N, <<"lipkg">>, D));
        lx ->
            add(R, In, jsxd:set(N, <<"lx">>, D));
        docker ->
            add(R, In, jsxd:set(N, <<"docker">>, D))
    end;
add([{N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        undefined ->
            add(R, In, D);
        V ->
            add(R, In, jsxd:set(N, V, D))
    end.

-spec load(fifo_dt:tid(), term()) -> dataset().

load(_, #{type := ?TYPE, version := ?VERSION} = D) ->
    D;

load(TID, #dataset_2{
             description    = Desc,
             disk_driver    = DiskD,
             homepage       = Homepage,
             image_size     = ImageSize,
             imported       = Imported,
             metadata       = Metadata,
             name           = Name,
             networks       = Networks,
             nic_driver     = NicD,
             os             = OS,
             requirements   = Reqs,
             status         = Status,
             type           = Type,
             users          = Users,
             uuid           = UUID,
             version        = Version,
             zone_type      = ZType,
             sha1           = SHA1,
             kernel_version = KVersion
            }) ->
    D1 = #{
      type            => ?TYPE,
      version         => 1,
      description     => Desc,
      disk_driver     => DiskD,
      homepage        => Homepage,
      image_size      => ImageSize,
      imported        => Imported,
      metadata        => Metadata,
      name            => Name,
      networks        => Networks,
      nic_driver      => NicD,
      os              => OS,
      requirements    => Reqs,
      status          => Status,
      dataset_type    => Type,
      users           => Users,
      uuid            => UUID,
      dataset_version => Version,
      zone_type       => ZType,
      sha1            => SHA1,
      kernel_version  => KVersion
     },
    load(TID, D1);

load({T, ID}, #dataset_1{
                 description    = Desc,
                 disk_driver    = DiskD,
                 homepage       = Homepage,
                 image_size     = ImageSize,
                 imported       = Imported,
                 metadata       = Metadata,
                 name           = Name,
                 networks       = Networks,
                 nic_driver     = NicD,
                 os             = OS,
                 requirements   = Reqs,
                 status         = Status,
                 type           = Type,
                 users          = Users,
                 uuid           = UUID,
                 version        = Version,
                 zone_type      = ZType,
                 sha1           = SHA1,
                 kernel_version = KVersion
                }) ->

    D =  #dataset_2{
            description    = Desc,
            disk_driver    = DiskD,
            homepage       = Homepage,
            image_size     = ImageSize,
            imported       = Imported,
            metadata       = fifo_dt:update_map(Metadata),
            name           = Name,
            networks       = fifo_dt:update_set(Networks),
            nic_driver     = NicD,
            os             = OS,
            requirements   = fifo_dt:update_set(Reqs),
            status         = Status,
            type           = Type,
            users          = Users,
            uuid           = UUID,
            version        = Version,
            zone_type      = ZType,
            sha1           = SHA1,
            kernel_version = KVersion
           },
    load({T, ID}, D);


load({T, ID}, #dataset_0{
                 description    = Desc,
                 disk_driver    = DiskD,
                 homepage       = Homepage,
                 image_size     = ImageSize,
                 imported       = Imported,
                 metadata       = Metadata,
                 name           = Name,
                 networks       = Networks,
                 nic_driver     = NicD,
                 os             = OS,
                 requirements   = Reqs,
                 status         = Status,
                 type           = Type,
                 users          = Users,
                 uuid           = UUID,
                 version        = Version
                }) ->
    Networks1 = [{NetName, NetDesc} ||
                    [{<<"description">>, NetDesc},
                     {<<"name">>, NetName}] <- riak_dt_lwwreg:value(Networks)],
    {ok, Networks2} = old_set:update(
                        {add_all, Networks1}, ID,
                        old_set:new()),
    D =  #dataset_1{
            description    = Desc,
            disk_driver    = DiskD,
            homepage       = Homepage,
            image_size     = ImageSize,
            imported       = Imported,
            metadata       = Metadata,
            name           = Name,
            networks       = Networks2,
            nic_driver     = NicD,
            os             = OS,
            requirements   = Reqs,
            status         = Status,
            type           = Type,
            users          = Users,
            uuid           = UUID,
            version        = Version
           },
    load({T, ID}, D);

load(TID, #dataset_0_1_1{
             description    = Desc,
             disk_driver    = DiskD,
             homepage       = Homepage,
             image_size     = ImageSize,
             imported       = Imported,
             metadata       = Metadata,
             name           = Name,
             networks       = Networks,
             nic_driver     = NicD,
             os             = OS,
             requirements   = Reqs,
             status         = Status,
             type           = Type,
             users          = Users,
             uuid           = UUID,
             version        = Version
            }) ->
    D1 = #dataset_0{
            description    = Desc,
            disk_driver    = DiskD,
            homepage       = Homepage,
            image_size     = ImageSize,
            imported       = Imported,
            metadata       = Metadata,
            name           = Name,
            networks       = Networks,
            nic_driver     = NicD,
            os             = OS,
            requirements   = Reqs,
            status         = Status,
            type           = Type,
            users          = Users,
            uuid           = UUID,
            version        = Version
           },
    load(TID, D1);

load(TID, #dataset_0_1_0{
             description    = Desc,
             disk_driver    = DiskD,
             homepage       = Homepage,
             image_size     = ImageSize,
             imported       = Imported,
             metadata       = Metadata,
             name           = Name,
             networks       = Networks,
             nic_driver     = NicD,
             os             = OS,
             requirements   = Reqs,
             status         = Status,
             type           = Type,
             users          = Users,
             uuid           = UUID,
             version        = Version
            }) ->
    D1 = #dataset_0_1_1{
            description    = Desc,
            disk_driver    = DiskD,
            homepage       = Homepage,
            image_size     = ImageSize,
            imported       = Imported,
            metadata       = Metadata,
            name           = Name,
            networks       = Networks,
            nic_driver     = NicD,
            os             = OS,
            requirements   = Reqs,
            status         = Status,
            type           = Type,
            users          = Users,
            uuid           = UUID,
            version        = Version
           },
    load(TID, D1).

merge(#{
         type            := ?TYPE,
         version         := ?VERSION,
         description     := Desc1,
         disk_driver     := DiskD1,
         homepage        := Homepage1,
         image_size      := ImageSize1,
         imported        := Imported1,
         kernel_version  := KernelVersion1,
         metadata        := Metadata1,
         name            := Name1,
         networks        := Networks1,
         nic_driver      := NicD1,
         os              := OS1,
         requirements    := Reqs1,
         sha1            := SHA11,
         status          := Status1,
         dataset_type    := Type1,
         users           := Users1,
         uuid            := UUID1,
         dataset_version := Version1,
         zone_type       := ZoneType1
       } = D1,
      #{
         type            := ?TYPE,
         description     := Desc2,
         disk_driver     := DiskD2,
         homepage        := Homepage2,
         image_size      := ImageSize2,
         imported        := Imported2,
         kernel_version  := KernelVersion2,
         metadata        := Metadata2,
         name            := Name2,
         networks        := Networks2,
         nic_driver      := NicD2,
         os              := OS2,
         requirements    := Reqs2,
         sha1            := SHA12,
         status          := Status2,
         dataset_type    := Type2,
         users           := Users2,
         uuid            := UUID2,
         dataset_version := Version2,
         zone_type       := ZoneType2
       }) ->
    D1#{
      description     => riak_dt_lwwreg:merge(Desc1, Desc2),
      disk_driver     => riak_dt_lwwreg:merge(DiskD1, DiskD2),
      homepage        => riak_dt_lwwreg:merge(Homepage1, Homepage2),
      image_size      => riak_dt_lwwreg:merge(ImageSize1, ImageSize2),
      imported        => riak_dt_lwwreg:merge(Imported1, Imported2),
      kernel_version  => riak_dt_lwwreg:merge(KernelVersion1, KernelVersion2),
      metadata        => fifo_map:merge(Metadata1, Metadata2),
      name            => riak_dt_lwwreg:merge(Name1, Name2),
      networks        => riak_dt_orswot:merge(Networks1, Networks2),
      nic_driver      => riak_dt_lwwreg:merge(NicD1, NicD2),
      os              => riak_dt_lwwreg:merge(OS1, OS2),
      requirements    => riak_dt_orswot:merge(Reqs1, Reqs2),
      sha1            => riak_dt_lwwreg:merge(SHA11, SHA12),
      status          => riak_dt_lwwreg:merge(Status1, Status2),
      dataset_type    => riak_dt_lwwreg:merge(Type1, Type2),
      users           => riak_dt_lwwreg:merge(Users1, Users2),
      uuid            => riak_dt_lwwreg:merge(UUID1, UUID2),
      dataset_version => riak_dt_lwwreg:merge(Version1, Version2),
      zone_type       => riak_dt_lwwreg:merge(ZoneType1, ZoneType2)
     }.

-spec net_to_json([network()]) ->
                         [#{}].
net_to_json(Nets) ->
    lists:sort([ #{<<"description">> => Desc, <<"name">> => Name}
                 || {Name, Desc} <- Nets]).
