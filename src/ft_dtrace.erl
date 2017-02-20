%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_dtrace).
-behaviour(fifo_dt).

-include("ft_dtrace.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         to_json/1,
         getter/2,
         merge/2,
         is_a/1
        ]).

-export([
         name/1, name/3,
         uuid/1, uuid/3,
         script/1, script/3,
         metadata/1, set_metadata/3, set_metadata/4,
         config/1, add_config/4, remove_config/3
        ]).

-ignore_xref([merge/2, load/2, getter/2, uuid/1]).

-ignore_xref([
              set/4,
              name/1, name/3,
              uuid/1, uuid/3,
              script/1, script/3,
              metadata/1, set_metadata/3, set_metadata/4,
              config/1, add_config/4, remove_config/3
             ]).

-type dtrace() :: #{
              type           => ?TYPE,
              version        => non_neg_integer(),
              uuid           => riak_dt_lwwreg:lwwreg(),
              name           => riak_dt_lwwreg:lwwreg(),
              script         => riak_dt_lwwreg:lwwreg(),
              config         => riak_dt_map:riak_dt_map(),
              metadata       => riak_dt_map:riak_dt_map()
             }.
-export_type([dtrace/0]).

?IS_A.

new(_) ->
    #{
     type           => ?TYPE,
     version        => ?VERSION,
     uuid           => riak_dt_lwwreg:new(),
     name           => riak_dt_lwwreg:new(),
     script         => riak_dt_lwwreg:new(),
     config         => riak_dt_map:new(),
     metadata       => riak_dt_map:new()
    }.

?REG_GET(uuid).
?REG_SET(uuid).
?REG_GET(name).
?REG_SET(name).
?REG_GET(script).
?REG_SET(script).


?META.
?SET_META_3.
?SET_META_4.

?MAP_GET(config).
?MAP_SET(add_config, config).
?MAP_REM(remove_config, config).

?G(<<"name">>, name);
?G(<<"uuid">>, uuid);
?G_JSX.

load(_, #{type := Type})  when Type =/= ?TYPE ->
    error(bad_arg);

load(_, #{version := ?VERSION} = Org) ->
    Org;

load(TID, #dtrace_0{
             config = Config,
             metadata = Metadata,
             script = Script,
             name = Name,
             uuid = UUID
            }) ->
    D1 = #{
      version  => 1,
      type     => ?TYPE,
      config   => Config,
      metadata => Metadata,
      script   => Script,
      name     => Name,
      uuid     => UUID
     },
    load(TID, D1);

load(TID, #dtrace_0_1_0{
             config = Config,
             metadata = Metadata,
             script = Script,
             name = Name,
             uuid = UUID
            }) ->
    D1 = #dtrace_0{
            config = fifo_dt:update_map(Config),
            metadata = fifo_dt:update_map(Metadata),
            script = Script,
            name = Name,
            uuid = UUID
           },
    load(TID, D1).

to_json(D) ->
    #{
       <<"config">>   => config(D),
       <<"metadata">> => metadata(D),
       <<"name">>     => name(D),
       <<"script">>   => script(D),
       <<"uuid">>     => uuid(D)
     }.


merge(#{
         type     := ?TYPE,
         version  := ?VERSION,
         config   := Config1,
         metadata := Metadata1,
         script   := Script1,
         name     := Name1,
         uuid     := UUID1
       } = D,
      #{
         type     := ?TYPE,
         config   := Config2,
         metadata := Metadata2,
         script   := Script2,
         name     := Name2,
         uuid     :=UUID2
       }) ->
    D#{
        config   => fifo_map:merge(Config1, Config2),
        metadata => fifo_map:merge(Metadata1, Metadata2),
        script   => riak_dt_lwwreg:merge(Script1, Script2),
        name     => riak_dt_lwwreg:merge(Name1, Name2),
        uuid     => riak_dt_lwwreg:merge(UUID1, UUID2)
       }.
