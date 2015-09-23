%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_package).

-include("ft_package.hrl").
-define(OBJ, ?PACKAGE).
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

-type package() :: #?PACKAGE{}.
-export_type([package/0]).

-spec is_a(any()) -> boolean().

?IS_A.

-spec new({integer(), atom()}) -> package().
new({_T, _ID}) ->
    {ok, Undefined} = ?NEW_LWW(undefined, 1),
    {ok, Off} = ?NEW_LWW(<<"off">>, 1),
    #?PACKAGE{
        blocksize = Undefined,
        cpu_shares = Undefined,
        cpu_cap = Undefined,
        compression = Off,
        max_swap = Undefined,
        zfs_io_priority = Undefined
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
          {<<"uuid">>, fun uuid/1},
          {<<"blocksize">>, fun blocksize/1},
          {<<"compression">>, fun compression/1},
          {<<"max_swap">>, fun max_swap/1},
          {<<"cpu_cap">>, fun cpu_cap/1},
          {<<"cpu_shares">>, fun cpu_shares/1},
          {<<"zfs_io_priority">>, fun zfs_io_priority/1}
         ],
    add(Vs, P, []).

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

-spec uuid(package()) -> binary().
?G(uuid).

-spec name(package()) -> binary().
?G(name).

-spec blocksize(package()) -> pos_integer() | undefined.
?G(blocksize).

-spec compression(package()) -> binary() | undefined.
?G(compression).

-spec cpu_cap(package()) -> pos_integer() | undefined.
?G(cpu_cap).

-spec cpu_shares(package()) -> pos_integer() | undefined.
?G(cpu_shares).

-spec max_swap(package()) -> pos_integer() | undefined.
?G(max_swap).

-spec quota(package()) -> pos_integer().
?G(quota).

-spec ram(package()) -> pos_integer().
?G(ram).

-spec zfs_io_priority(package()) -> pos_integer() | undefined.
?G(zfs_io_priority).

-spec uuid({integer(), atom()}, binary(), package()) -> package().
?S_BIN(uuid).

-spec name({integer(), atom()}, binary(), package()) -> package().
?S_BIN(name).

-spec blocksize({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(blocksize).

-spec compression({integer(), atom()}, binary(), package()) -> package().
compression({T, _ID}, V, O) when
      V == <<"on">>; V == <<"off">>;
      V == <<"lz4">>; V == <<"lzjb">>; V == <<"zle">>;
      V == <<"gzip">>; V == <<"gzip-1">>; V == <<"gzip-2">>; V == <<"gzip-3">>;
      V == <<"gzip-4">>; V == <<"gzip-5">>; V == <<"gzip-6">>;
      V == <<"gzip-7">>; V == <<"gzip-8">>; V == <<"gzip-9">> ->
    ?S_BODY(compression).

-spec cpu_cap({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(cpu_cap).

-spec cpu_shares({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(cpu_shares).

-spec max_swap({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(max_swap).

-spec quota({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(quota).

-spec ram({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(ram).

-spec zfs_io_priority({integer(), atom()}, pos_integer(), package()) -> package().
?S_PI(zfs_io_priority).

requirements(H) ->
    riak_dt_orswot:value(H#?PACKAGE.requirements).

add_requirement({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?PACKAGE.requirements),
    H#?PACKAGE{requirements = O1}.

remove_requirement({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?PACKAGE.requirements) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?PACKAGE{requirements = O1}
    end.

-spec metadata(package()) -> jsxd:object().

metadata(H) ->
    fifo_map:value(H#?PACKAGE.metadata).


fix_reqs(ID, Requirements) ->
    Rs = old_set:value(Requirements),
    Rs1 = [fifo_dt:js2req(R) || R <- Rs],
    {ok, Rs2} = old_set:update(
                  {add_all, Rs1}, ID,
                  old_set:new()),
    Rs2.

-spec load({integer(), atom()}, term()) -> package().

load(_, #?PACKAGE{} = P) ->
    P;
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

-spec set_metadata({integer(), atom()}, [{jsxd:key(), jsxd:value()}], package()) -> package().
set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?PACKAGE.metadata),
    G#?PACKAGE{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?PACKAGE.metadata),
    G#?PACKAGE{metadata = M1}.

merge(#?PACKAGE{
          blocksize       = BlockSize1,
          compression     = Compression1,
          cpu_cap         = CpuCap1,
          cpu_shares      = CpuShares1,
          max_swap        = MaxSwap1,
          metadata        = Metadata1,
          name            = Name1,
          quota           = Quota1,
          ram             = RAM1,
          requirements    = Req1,
          uuid            = UUID1,
          zfs_io_priority = ZFSIOPriority1
         },
      #?PACKAGE{
          blocksize       = BlockSize2,
          compression     = Compression2,
          cpu_cap         = CpuCap2,
          cpu_shares      = CpuShares2,
          max_swap        = MaxSwap2,
          metadata        = Metadata2,
          name            = Name2,
          quota           = Quota2,
          ram             = RAM2,
          requirements    = Req2,
          uuid            = UUID2,
          zfs_io_priority = ZFSIOPriority2
         }) ->
    #?PACKAGE{
        blocksize       = riak_dt_lwwreg:merge(BlockSize1, BlockSize2),
        compression     = riak_dt_lwwreg:merge(Compression1, Compression2),
        cpu_cap         = riak_dt_lwwreg:merge(CpuCap1, CpuCap2),
        cpu_shares      = riak_dt_lwwreg:merge(CpuShares1, CpuShares2),
        max_swap        = riak_dt_lwwreg:merge(MaxSwap1, MaxSwap2),
        metadata        = fifo_map:merge(Metadata1, Metadata2),
        name            = riak_dt_lwwreg:merge(Name1, Name2),
        quota           = riak_dt_lwwreg:merge(Quota1, Quota2),
        ram             = riak_dt_lwwreg:merge(RAM1, RAM2),
        requirements    = riak_dt_orswot:merge(Req1, Req2),
        uuid            = riak_dt_lwwreg:merge(UUID1, UUID2),
        zfs_io_priority = riak_dt_lwwreg:merge(ZFSIOPriority1, ZFSIOPriority2)
       }.
