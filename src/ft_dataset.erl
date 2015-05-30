%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_dataset).

-include("ft_dataset.hrl").
-define(OBJ, ?DATASET).
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

-type dataset() :: #?DATASET{}.
-export_type([dataset/0]).

?IS_A.

new({_T, _ID}) ->
    {ok, Imported} = ?NEW_LWW(0, 1),
    {ok, Status} = ?NEW_LWW(<<"new">>, 1),
    #?DATASET{
        imported = Imported,
        status = Status
       }.

?G(<<"uuid">>, uuid);
?G(<<"kernel_version">>, uuid);
?G(<<"type">>, type);
?G(<<"zone_type">>, type);
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

?G(uuid).
?S(uuid).

?G(type).
type(ID, <<"kvm">>, H) ->
    type(ID, kvm, H);
type(ID, <<"zone">>, H) ->
    type(ID, zone, H);
type({T, _ID}, V, H) when V =:= kvm;
                          V =:= zone ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DATASET.type),
    H#?DATASET{type = V1}.

?G(zone_type).
zone_type(ID, <<"lx">>, H) ->
    zone_type(ID, lx, H);
zone_type(ID, <<"ipkg">>, H) ->
    zone_type(ID, ipkg, H);
zone_type(ID, <<"lipkg">>, H) ->
    zone_type(ID, lipkg, H);
zone_type({T, _ID}, V, H) when V =:= lx ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DATASET.zone_type),
    H#?DATASET{zone_type = V1}.

?G(status).
?S(status).
?G(imported).
?S(imported).

?G(description).
?S(description).
?G(disk_driver).
?S(disk_driver).
?G(homepage).
?S(homepage).
?G(image_size).
?S(image_size).
?G(name).
?S(name).
?G(nic_driver).
?S(nic_driver).
?G(os).
?S(os).
?G(sha1).
?S(sha1).
?G(users).
?S(users).
?G(version).
?S(version).
?G(kernel_version).
?S(kernel_version).


requirements(H) ->
    riak_dt_orswot:value(H#?DATASET.requirements).

add_requirement({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?DATASET.requirements),
    H#?DATASET{requirements = O1}.

remove_requirement({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?DATASET.requirements) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?DATASET{requirements = O1}
    end.

networks(H) ->
    riak_dt_orswot:value(H#?DATASET.networks).

add_network({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?DATASET.networks),
    H#?DATASET{networks = O1}.

remove_network({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?DATASET.networks) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?DATASET{networks = O1}
    end.

metadata(H) ->
    fifo_map:value(H#?DATASET.metadata).

set_metadata(ID, [{K, V} | R] , Vm) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Vm));

set_metadata(_ID, _, Vm) ->
    Vm.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?DATASET.metadata),
    G#?DATASET{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?DATASET.metadata),
    G#?DATASET{metadata = M1}.

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
    add(Vs, D, [{<<"type">>, Type}]).

add([], _, D) ->
    D;
add([{<<"zone_type">> = N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        lx ->
            add(R, In, jsxd:set(N, <<"lx">>, D))
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

load(_, #?DATASET{} = H) ->
    H;

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
                    [{<<"description">>,NetDesc},
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

merge(#?DATASET{
          description    = Desc1,
          disk_driver    = DiskD1,
          homepage       = Homepage1,
          image_size     = ImageSize1,
          imported       = Imported1,
          kernel_version = KernelVersion1,
          metadata       = Metadata1,
          name           = Name1,
          networks       = Networks1,
          nic_driver     = NicD1,
          os             = OS1,
          requirements   = Reqs1,
          sha1           = SHA11,
          status         = Status1,
          type           = Type1,
          users          = Users1,
          uuid           = UUID1,
          version        = Version1,
          zone_type      = ZoneType1
         },
      #?DATASET{
          description    = Desc2,
          disk_driver    = DiskD2,
          homepage       = Homepage2,
          image_size     = ImageSize2,
          imported       = Imported2,
          kernel_version = KernelVersion2,
          metadata       = Metadata2,
          name           = Name2,
          networks       = Networks2,
          nic_driver     = NicD2,
          os             = OS2,
          requirements   = Reqs2,
          sha1           = SHA12,
          status         = Status2,
          type           = Type2,
          users          = Users2,
          uuid           = UUID2,
          version        = Version2,
          zone_type      = ZoneType2
         }) ->
    #?DATASET{
        description    = riak_dt_lwwreg:merge(Desc1, Desc2),
        disk_driver    = riak_dt_lwwreg:merge(DiskD1, DiskD2),
        homepage       = riak_dt_lwwreg:merge(Homepage1, Homepage2),
        image_size     = riak_dt_lwwreg:merge(ImageSize1, ImageSize2),
        imported       = riak_dt_lwwreg:merge(Imported1, Imported2),
        kernel_version = riak_dt_lwwreg:merge(KernelVersion1, KernelVersion2),
        metadata       = fifo_map:merge(Metadata1, Metadata2),
        name           = riak_dt_lwwreg:merge(Name1, Name2),
        networks       = riak_dt_orswot:merge(Networks1, Networks2),
        nic_driver     = riak_dt_lwwreg:merge(NicD1, NicD2),
        os             = riak_dt_lwwreg:merge(OS1, OS2),
        requirements   = riak_dt_orswot:merge(Reqs1, Reqs2),
        sha1           = riak_dt_lwwreg:merge(SHA11, SHA12),
        status         = riak_dt_lwwreg:merge(Status1, Status2),
        type           = riak_dt_lwwreg:merge(Type1, Type2),
        users          = riak_dt_lwwreg:merge(Users1, Users2),
        uuid           = riak_dt_lwwreg:merge(UUID1, UUID2),
        version        = riak_dt_lwwreg:merge(Version1, Version2),
        zone_type      = riak_dt_lwwreg:merge(ZoneType1, ZoneType2)
       }.

net_to_json(Nets) ->
    lists:sort([ [{<<"description">>, Desc}, {<<"name">>, Name}]
                 || {Name, Desc} <- Nets]).
