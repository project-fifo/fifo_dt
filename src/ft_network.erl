%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_network).
-behaviour(fifo_dt).

-include("ft_network.hrl").
-include("ft_helper.hrl").

-export([
         is_a/1,
         new/1,
         load/2,
         to_json/1,
         name/1, name/3,
         uuid/1, uuid/3,
         add_resolver/3, remove_resolver/3, resolvers/1,
         add_iprange/3, remove_iprange/3, ipranges/1,
         metadata/1, set_metadata/3, set_metadata/4,
         getter/2,
         merge/2
        ]).

-ignore_xref([load/2, to_json/1, getter/2, merge/2]).

-ignore_xref([
              add_resolver/3, remove_resolver/3, resolvers/1,
              add_iprange/3, remove_iprange/3, ipranges/1,
              name/1, name/3,
              uuid/1, uuid/3,
              metadata/1, set_metadata/3, set_metadata/4
             ]).

-type network() :: #{
               type           => ?TYPE,
               version        => non_neg_integer(),
               uuid           => riak_dt_lwwreg:lwwreg(),
               name           => riak_dt_lwwreg:lwwreg(),
               ipranges       => riak_dt_orswot:orswot(),
               resolvers      => riak_dt_orswot:orswot(),
               metadata       => riak_dt_map:riak_dt_map()
              }.

-export_type([network/0]).

?IS_A.

-spec new(fifo:tid()) -> network().

new(_) ->
    #{
     type        => ?TYPE,
     version     => ?VERSION,
     uuid        => riak_dt_lwwreg:new(),
     name        => riak_dt_lwwreg:new(),
     ipranges    => riak_dt_orswot:new(),
     resolvers   => riak_dt_orswot:new(),
     metadata    => riak_dt_map:new()
    }.


-spec load(fifo:tid(), any()) -> network().
load(_, #{type := ?TYPE, version := ?VERSION} = N) ->
    N;

load(_TID, #network_0{
             uuid           = UUID,
             name           = Name,
             ipranges       = IPRanges,
             metadata       = Metadata
        }) ->
    #{
      type      => ?TYPE,
      version   => ?VERSION,
      uuid      => UUID,
      name      => Name,
      ipranges  => IPRanges,
      metadata  => Metadata,
      resolvers   => riak_dt_orswot:new()
     };

load(TID, #network_0_1_0{
             uuid           = UUID,
             name           = Name,
             ipranges       = IPRanges,
             metadata       = Metadata
        }) ->
    N = #network_0{
           uuid           = UUID,
           name           = Name,
           ipranges       = fifo_dt:update_set(IPRanges),
           metadata       = fifo_dt:update_map(Metadata)
          },
    load(TID, N).

to_json(N) ->
    #{
       <<"resolvers">> => resolvers(N),
       <<"ipranges">>  => ipranges(N),
       <<"metadata">>  => metadata(N),
       <<"name">>      => name(N),
       <<"uuid">>      => uuid(N)
     }.
-spec merge(network(), network()) -> network().
merge(R = #{
        type      := ?TYPE,
        ipranges  := IPRanges1,
        metadata  := Metadata1,
        resolvers := Resolvers1,
        name      := Name1,
        uuid      := UUID1
       },
      #{
         type      := ?TYPE,
         ipranges  := IPRanges2,
         metadata  := Metadata2,
         resolvers := Resolvers2,
         name      := Name2,
         uuid      := UUID2
       }) ->
    R#{
      resolvers => riak_dt_orswot:merge(Resolvers1, Resolvers2),
      ipranges  => riak_dt_orswot:merge(IPRanges1, IPRanges2),
      metadata  => fifo_map:merge(Metadata1, Metadata2),
      name      => riak_dt_lwwreg:merge(Name1, Name2),
      uuid     => riak_dt_lwwreg:merge(UUID1, UUID2)
     } .


?G(<<"name">>, name);
?G(<<"uuid">>, uuid);
?G_JSX.

-spec name(network()) -> binary().
?REG_GET(name).
-spec name(fifo_dt:tid(), binary(), network()) -> network().
?REG_SET(name).

-spec uuid(network()) -> binary().
?REG_GET(uuid).
-spec uuid(fifo_dt:tid(), binary(), network()) -> network().
?REG_SET(uuid).

-spec ipranges(network()) -> [binary()].
?SET_GET(ipranges).

-spec add_iprange(fifo_dt:tid(), binary(), network()) -> network().
?SET_ADD(add_iprange, ipranges).

-spec remove_iprange(fifo_dt:tid(), binary(), network()) -> network().
?SET_REM(remove_iprange, ipranges).

-spec resolvers(network()) -> [binary()].
?SET_GET(resolvers).

-spec add_resolver(fifo_dt:tid(), binary(), network()) -> network().
?SET_ADD(add_resolver, resolvers).

-spec remove_resolver(fifo_dt:tid(), binary(), network()) -> network().
?SET_REM(remove_resolver, resolvers).

?META.
?SET_META_3.
?SET_META_4.
