%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_package).

-include("ft_package.hrl").
-include("ft_helper.hrl").

-export([
         to_json/1,
         load/2,
         new/1,
         merge/2,
         getter/2,
         blocksize/1, blocksize/3,
         compression/1, compression/3,
         cpu_cap/1, cpu_cap/3,
         cpu_shares/1, cpu_shares/3,
         max_swap/1, max_swap/3,
         name/1, name/3,
         quota/1, quota/3,
         ram/1, ram/3,
         uuid/1, uuid/3,
         requirements/1, add_requirement/3, remove_requirement/3,
         metadata/1, set_metadata/3, set_metadata/4,
         zfs_io_priority/1, zfs_io_priority/3,
         org_resources/1, org_resource/2, org_resource_inc/4, org_resource_dec/4,
         org_resource_remove/3,
         hv_resources/1, hv_resource/2, hv_resource_inc/4, hv_resource_dec/4,
         hv_resource_remove/3,

         is_a/1
        ]).

-ignore_xref([
              metadata/1, set_metadata/3, set_metadata/4,
              blocksize/1, blocksize/3,
              compression/1, compression/3,
              cpu_cap/1, cpu_cap/3,
              cpu_shares/1, cpu_shares/3,
              max_swap/1, max_swap/3,
              name/1, name/3,
              quota/1, quota/3,
              ram/1, ram/3,
              uuid/1, uuid/3,
              requiremnets/3, add_requirement/3, remove_requirement/3,
              zfs_io_priority/1, zfs_io_priority/3
             ]).

-ignore_xref([merge/2, load/2, name/1, getter/2, uuid/1]).

-type package() ::
        #{
           type        => ?TYPE,
           version     => non_neg_integer(),
           uuid        => riak_dt_lwwreg:lwwreg(),
           name        => riak_dt_lwwreg:lwwreg(),

           blocksize       => riak_dt_lwwreg:lwwreg(),
           compression     => riak_dt_lwwreg:lwwreg(),
           cpu_cap         => riak_dt_lwwreg:lwwreg(),
           cpu_shares      => riak_dt_lwwreg:lwwreg(),
           max_swap        => riak_dt_lwwreg:lwwreg(),
           quota           => riak_dt_lwwreg:lwwreg(),
           ram             => riak_dt_lwwreg:lwwreg(),
           zfs_io_priority => riak_dt_lwwreg:lwwreg(),
           requirements    => riak_dt_orswot:orswot(),
           org_resources   => riak_dt_map:orswot(),
           hv_resources    => riak_dt_map:orswot(),
           metadata        => riak_dt_map:riak_dt_map()
         }.
-export_type([package/0]).

-spec is_a(any()) -> boolean().

?IS_A.

-spec new(fifo_dt:tid()) -> package().
new({_T, _ID}) ->
    {ok, Undefined} = ?NEW_LWW(undefined, 1),
    {ok, Off} = ?NEW_LWW(<<"off">>, 1),
    #{
       type            => ?TYPE,
       version         => ?VERSION,

       uuid            => riak_dt_lwwreg:new(),
       name            => riak_dt_lwwreg:new(),
       blocksize       => Undefined,
       cpu_shares      => Undefined,
       cpu_cap         => Undefined,
       compression     => Off,
       max_swap        => Undefined,
       zfs_io_priority => Undefined,

       quota           => riak_dt_lwwreg:new(),
       ram             => riak_dt_lwwreg:new(),
       org_resources   => ft_cmap:new(),
       hv_resources    => ft_cmap:new(),
       requirements    => riak_dt_orswot:new(),

       metadata        => riak_dt_map:new()
     }.

-spec to_json(Package :: package()) -> jsxd:object().

to_json(P) ->
    Vs = [
          {<<"metadata">>, fun metadata/1},
          {<<"name">>, fun name/1},
          {<<"quota">>, fun quota/1},
          {<<"ram">>, fun ram/1},
          {<<"requirements">>,
           fun (D1) ->
                   [fifo_dt:req2js(R) || R <- requirements(D1)]
           end},
          {<<"org_resources">>, fun org_resources/1},
          {<<"hv_resources">>, fun hv_resources/1},

          {<<"uuid">>, fun uuid/1},

          {<<"blocksize">>, fun blocksize/1},
          {<<"compression">>, fun compression/1},
          {<<"max_swap">>, fun max_swap/1},
          {<<"cpu_cap">>, fun cpu_cap/1},
          {<<"cpu_shares">>, fun cpu_shares/1},
          {<<"zfs_io_priority">>, fun zfs_io_priority/1}
         ],
    Vs1 = lists:sort(Vs),
    add(Vs1, P, []).

add([], _, D) ->
    D;

add([{N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        undefined ->
            add(R, In, D);
        V ->
            add(R, In, jsxd:set(N, V, D))
    end.

-spec getter(ft_obj:obj(), jsxd:key()) -> term().

?G(<<"uuid">>, uuid);
?G(<<"name">>, name);
?G(<<"blocksize">>, blocksize);
?G(<<"compression">>, compression);
?G(<<"cpu_cap">>, cpu_cap);
?G(<<"cpu_shares">>, cpu_shares);
?G(<<"max_swap">>, max_swap);
?G(<<"quota">>, quota);
?G(<<"ram">>, ram);
?G(<<"zfs_io_priority">>, zfs_io_priority);
?G_JSX.

-spec uuid(ft_type:tid(), binary(), package()) -> package().
?REG_SET_BIN(uuid).

-spec uuid(package()) -> binary().
?REG_GET(uuid).

-spec name(ft_type:tid(), binary(), package()) -> package().
?REG_SET_BIN(name).
-spec name(package()) -> binary().
?REG_GET(name).

-spec blocksize(ft_type:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(blocksize).
-spec blocksize(package()) -> pos_integer() | undefined.
?REG_GET(blocksize).

-spec compression(ft_type:tid(), binary(), package()) -> package().
compression({T, _ID}, V, O = #{type := ?TYPE, compression := Reg0}) when
      V == <<"on">>; V == <<"off">>;
      V == <<"lz4">>; V == <<"lzjb">>; V == <<"zle">>;
      V == <<"gzip">>; V == <<"gzip-1">>; V == <<"gzip-2">>; V == <<"gzip-3">>;
      V == <<"gzip-4">>; V == <<"gzip-5">>; V == <<"gzip-6">>;
      V == <<"gzip-7">>; V == <<"gzip-8">>; V == <<"gzip-9">> ->
    ?REG_SET_BODY(compression).
-spec compression(package()) -> binary() | undefined.
?REG_GET(compression).

-spec cpu_cap(ft_type:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(cpu_cap).
-spec cpu_cap(package()) -> pos_integer() | undefined.
?REG_GET(cpu_cap).

-spec cpu_shares(fifo_dt:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(cpu_shares).
-spec cpu_shares(package()) -> pos_integer() | undefined.
?REG_GET(cpu_shares).

-spec max_swap(fifo_dt:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(max_swap).
-spec max_swap(package()) -> pos_integer() | undefined.
?REG_GET(max_swap).

-spec quota(fifo_dt:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(quota).
-spec quota(package()) -> pos_integer().
?REG_GET(quota).

-spec ram(fifo_dt:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(ram).
-spec ram(package()) -> pos_integer().
?REG_GET(ram).

-spec zfs_io_priority(fifo_dt:tid(), pos_integer(), package()) -> package().
?REG_SET_PI(zfs_io_priority).
-spec zfs_io_priority(package()) -> pos_integer() | undefined.
?REG_GET(zfs_io_priority).



org_resources(#{type := ?TYPE, org_resources := Rs}) ->
    ft_cmap:value(Rs).

org_resource(#{type := ?TYPE, org_resources := Rs}, K) ->
    ft_cmap:get(K, Rs).

org_resource_inc({_T, ID}, K, V, O = #{type := ?TYPE, org_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:inc(ID, K, V, Rs),
    O#{org_resources := Rs1}.

org_resource_remove({_T, ID}, K, O = #{type := ?TYPE, org_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:remove(ID, K, Rs),
    O#{org_resources := Rs1}.

org_resource_dec({_T, ID}, K, V, O = #{type := ?TYPE, org_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:dec(ID, K, V, Rs),
    O#{org_resources := Rs1}.

hv_resources(#{type := ?TYPE, hv_resources := Rs}) ->
    ft_cmap:value(Rs).

hv_resource(#{type := ?TYPE, hv_resources := Rs}, K) ->
    ft_cmap:get(K, Rs).

hv_resource_inc({_T, ID}, K, V, O = #{type := ?TYPE, hv_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:inc(ID, K, V, Rs),
    O#{hv_resources := Rs1}.

hv_resource_remove({_T, ID}, K, O = #{type := ?TYPE, hv_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:remove(ID, K, Rs),
    O#{hv_resources := Rs1}.

hv_resource_dec({_T, ID}, K, V, O = #{type := ?TYPE, hv_resources := Rs}) ->
    {ok, Rs1} = ft_cmap:dec(ID, K, V, Rs),
    O#{hv_resources := Rs1}.


?SET_GET(requirements).
?SET_ADD(add_requirement, requirements).
?SET_REM(remove_requirement, requirements).

fix_reqs(ID, Requirements) ->
    Rs = old_set:value(Requirements),
    Rs1 = [fifo_dt:js2req(R) || R <- Rs],
    {ok, Rs2} = old_set:update(
                  {add_all, Rs1}, ID,
                  old_set:new()),
    Rs2.

-spec load(fifo_dt:tid(), term()) -> package().

load(_, #{type := ?TYPE, version := ?VERSION} = Package) ->
    Package;
load(TID, #package_1{
             uuid            = UUID,
             name            = Name,
             metadata        = Metadata,

             blocksize       = BlockSize,
             compression     = Compression,
             cpu_cap         = CpuCap,
             cpu_shares      = CpuShares,
             max_swap        = MaxSwap,
             quota           = Quota,
             ram             = RAM,
             requirements    = Requirements,

             zfs_io_priority = ZFSIOPriority
            }) ->
    load(TID, #{
           type            => ?TYPE,
           version         => 0,
           uuid            => UUID,
           name            => Name,
           metadata        => Metadata,

           blocksize       => BlockSize,
           compression     => Compression,
           cpu_cap         => CpuCap,
           cpu_shares      => CpuShares,
           max_swap        => MaxSwap,
           quota           => Quota,
           ram             => RAM,
           requirements    => Requirements,

           org_resources   => ft_cmap:new(),
           hv_resources     => ft_cmap:new(),

           zfs_io_priority => ZFSIOPriority
          });

load(TID, #package_0{
             uuid            = UUID,
             name            = Name,
             metadata        = Metadata,

             blocksize       = BlockSize,
             compression     = Compression,
             cpu_cap         = CpuCap,
             cpu_shares      = CpuShares,
             max_swap        = MaxSwap,
             quota           = Quota,
             ram             = RAM,
             requirements    = Requirements,

             zfs_io_priority = ZFSIOPriority1
            }) ->
    P = #package_1{
           uuid            = UUID,
           name            = Name,
           metadata        = fifo_dt:update_map(Metadata),

           blocksize       = BlockSize,
           compression     = Compression,
           cpu_cap         = CpuCap,
           cpu_shares      = CpuShares,
           max_swap        = MaxSwap,
           quota           = Quota,
           ram             = RAM,
           requirements    = fifo_dt:update_set(Requirements),

           zfs_io_priority = ZFSIOPriority1
          },
    load(TID, P);

load({T, ID}, #package_0_1_0{
                 uuid            = UUID1,
                 name            = Name1,
                 metadata        = Metadata1,

                 blocksize       = BlockSize1,
                 compression     = Compression1,
                 cpu_cap         = CpuCap1,
                 cpu_shares      = CpuShares1,
                 max_swap        = MaxSwap1,
                 quota           = Quota1,
                 ram             = RAM1,
                 requirements    = Requirements1,

                 zfs_io_priority = ZFSIOPriority1
                }) ->
    D1 = #package_0{
            uuid            = UUID1,
            name            = Name1,
            metadata        = Metadata1,

            blocksize       = BlockSize1,
            compression     = Compression1,
            cpu_cap         = CpuCap1,
            cpu_shares      = CpuShares1,
            max_swap        = MaxSwap1,
            quota           = Quota1,
            ram             = RAM1,
            requirements    = fix_reqs(ID, Requirements1),

            zfs_io_priority = ZFSIOPriority1
           },
    load({T, ID}, D1).


merge(O = #{
        type            := ?TYPE,
        version         := ?VERSION,
        blocksize       := BlockSize1,
        compression     := Compression1,
        cpu_cap         := CpuCap1,
        cpu_shares      := CpuShares1,
        max_swap        := MaxSwap1,
        metadata        := Metadata1,
        name            := Name1,
        quota           := Quota1,
        ram             := RAM1,
        requirements    := Req1,
        org_resources   := OrgResources1,
        hv_resources    := HVResources1,

        uuid            := UUID1,
        zfs_io_priority := ZFSIOPriority1
       },
      #{
         type            := ?TYPE,
         version         := ?VERSION,
         blocksize       := BlockSize2,
         compression     := Compression2,
         cpu_cap         := CpuCap2,
         cpu_shares      := CpuShares2,
         max_swap        := MaxSwap2,
         metadata        := Metadata2,
         name            := Name2,
         quota           := Quota2,
         ram             := RAM2,
         requirements    := Req2,
         org_resources   := OrgResources2,
         hv_resources    := HVResources2,
         uuid            := UUID2,
         zfs_io_priority := ZFSIOPriority2
       }) ->
    O#{
      blocksize       => riak_dt_lwwreg:merge(BlockSize1, BlockSize2),
      compression     => riak_dt_lwwreg:merge(Compression1, Compression2),
      cpu_cap         => riak_dt_lwwreg:merge(CpuCap1, CpuCap2),
      cpu_shares      => riak_dt_lwwreg:merge(CpuShares1, CpuShares2),
      max_swap        => riak_dt_lwwreg:merge(MaxSwap1, MaxSwap2),
      metadata        => fifo_map:merge(Metadata1, Metadata2),
      name            => riak_dt_lwwreg:merge(Name1, Name2),
      quota           => riak_dt_lwwreg:merge(Quota1, Quota2),
      ram             => riak_dt_lwwreg:merge(RAM1, RAM2),
      requirements    => riak_dt_orswot:merge(Req1, Req2),
      org_resources   => ft_cmap:merge(OrgResources1, OrgResources2),
      hv_resources    => ft_cmap:merge(HVResources1, HVResources2),
      uuid            => riak_dt_lwwreg:merge(UUID1, UUID2),
      zfs_io_priority => riak_dt_lwwreg:merge(ZFSIOPriority1, ZFSIOPriority2)
     }.

?META.
?SET_META_3.
?SET_META_4.
