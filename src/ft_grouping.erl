%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_grouping).
-behaviour(fifo_dt).

-include("ft_grouping.hrl").
-include("ft_helper.hrl").

-export([
         is_a/1,
         uuid/3,
         uuid/1,
         getter/2,
         load/2,
         merge/2,
         name/1,
         name/3,
         new/1,
         to_json/1,
         type/1,
         type/3,
         add_element/3,
         remove_element/3,
         add_grouping/3,
         remove_grouping/3,
         elements/1,
         groupings/1,
         config/1, set_config/3, set_config/4,
         metadata/1, set_metadata/3, set_metadata/4
        ]).

-ignore_xref([
              getter/2, load/2, merge/2, new/1, to_json/1,
              uuid/1, uuid/3,
              name/1, name/3,
              type/1, type/3,
              elements/1, add_element/3, remove_element/3,
              groupings/1, add_grouping/3, remove_grouping/3,
              metadata/1, set_metadata/3, set_metadata/4
             ]).

-type grouping() :: #{
                type           => ?TYPE,
                version        => non_neg_integer(),
                uuid           => riak_dt_lwwreg:lwwreg(),
                name           => riak_dt_lwwreg:lwwreg(),
                grouping_type  => riak_dt_lwwreg:lwwreg(),
                groupings      => riak_dt_orswot:orswot(),
                elements       => riak_dt_orswot:orswot(),
                config         => riak_dt_map:riak_dt_map(),
                metadata       => riak_dt_map:riak_dt_map()
               }.
-export_type([grouping/0]).

?IS_A.

-spec new(fifo_dt:tid()) -> grouping().
new(_) ->
    #{
     type           => ?TYPE,
     version        => ?VERSION,
     uuid           => riak_dt_lwwreg:new(),
     name           => riak_dt_lwwreg:new(),
     grouping_type  => riak_dt_lwwreg:new(),
     groupings      => riak_dt_orswot:new(),
     elements       => riak_dt_orswot:new(),
     config         => riak_dt_map:new(),
     metadata       => riak_dt_map:new()
    }.


?REG_GET(uuid).
?REG_SET(uuid).

?REG_GET(name).
?REG_SET(name).


type(#{type := ?TYPE, grouping_type := T}) ->
    riak_dt_lwwreg:value(T).

type({T, _ID}, V, G = #{type := ?TYPE, grouping_type := Old}) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, Old),
    G#{grouping_type => V1}.

-spec elements(grouping()) -> [binary()].
?SET_GET(elements).

-spec add_element(fifo_dt:tid(), binary(), grouping()) -> grouping().
?SET_ADD(add_element, elements).

-spec remove_element(fifo_dt:tid(), binary(), grouping()) -> grouping().
?SET_REM(remove_element, elements).

-spec groupings(grouping()) -> [binary()].
?SET_GET(groupings).

-spec add_grouping(fifo_dt:tid(), binary(), grouping()) -> grouping().
?SET_ADD(add_grouping, groupings).

-spec remove_grouping(fifo_dt:tid(), binary(), grouping()) -> grouping().
?SET_REM(remove_grouping, groupings).

?G(<<"name">>, name);
?G(<<"uuid">>, uuid);
?G(<<"elements">>, elements);
?G(<<"groupings">>, groupings);
?G(<<"type">>, type);
?G_JSX.

-spec load(fifo_dt:tid(), term()) -> grouping().
load(_, #{type := Other}) when Other =/= ?TYPE ->
    error(bad_arg);

load(_, #{version := ?VERSION} = G) ->
    G;
load(TID, #grouping_1{
             uuid = UUID,
             name = Name,
             type = Type,
             groupings = Groupings,
             elements = Elements,
             metadata = Metadata,
             config = Config
            }) ->
    G1 = #{
      version       => ?VERSION,
      type          => ?TYPE,
      uuid          => UUID,
      name          => Name,
      grouping_type => Type,
      groupings     => Groupings,
      elements      => Elements,
      metadata      => Metadata,
      config        => Config
     },
    load(TID, G1);

load(TID, #grouping_0{
             uuid = UUID,
             name = Name,
             type = Type,
             groupings = Groupings,
             elements = Elements,
             metadata = Metadata,
             config = Config
            }) ->
    G1 = #grouping_1{
            uuid = UUID,
            name = Name,
            type = Type,
            groupings = fifo_dt:update_set(Groupings),
            elements = fifo_dt:update_set(Elements),
            metadata = fifo_dt:update_map(Metadata),
            config = fifo_dt:update_map(Config)
           },
    load(TID, G1);

load(TID, #grouping_0_1_0{
             uuid = UUID,
             name = Name,
             type = Type,
             groupings = Groupings,
             elements = Elements,
             metadata = Metadata
            }) ->
    G1 = #grouping_0{
            uuid = UUID,
            name = Name,
            type = Type,
            groupings = Groupings,
            elements = Elements,
            metadata = Metadata
           },
    load(TID, G1).

?META.
?SET_META_3.
?SET_META_4.

?MAP_GET(config).
?MAP_SET_3(set_config).
?MAP_SET_4(set_config, config).

to_json(G) ->
    Type = case type(G) of
               A when is_atom(A) ->
                   list_to_binary(atom_to_list(A));
               _ ->
                   <<"none">>
           end,
    #{
       <<"config">> => config(G),
       <<"elements">> => elements(G),
       <<"groupings">> => groupings(G),
       <<"metadata">> => metadata(G),
       <<"name">> => name(G),
       <<"type">> => Type,
       <<"uuid">> => uuid(G)
     }.

merge(#{
         type          := ?TYPE,
         version       := ?VERSION,
         elements      := Elements1,
         groupings     := Groupings1,
         metadata      := Metadata1,
         config        := Config1,
         name          := Name1,
         grouping_type := Type1,
         uuid          := UUID1
       } = G,
      #{
         type          := ?TYPE,
         version       := ?VERSION,
         elements      := Elements2,
         groupings     := Groupings2,
         metadata      := Metadata2,
         config        := Config2,
         name          := Name2,
         grouping_type := Type2,
         uuid          := UUID2
       }) ->
    G#{
      elements      => riak_dt_orswot:merge(Elements1, Elements2),
      groupings     => riak_dt_orswot:merge(Groupings1, Groupings2),
      metadata      => fifo_map:merge(Metadata1, Metadata2),
      config        => fifo_map:merge(Config1, Config2),
      name          => riak_dt_lwwreg:merge(Name1, Name2),
      grouping_type => riak_dt_lwwreg:merge(Type1, Type2),
      uuid          => riak_dt_lwwreg:merge(UUID1, UUID2)
     }.
