%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_dtrace).

-include("sniffle.hrl").
-include("ft.hrl").

-export([
         set/4,
         new/1,
         load/2,
         to_json/1,
         getter/2,
         merge/2
        ]).

-export([
         name/1, name/3,
         uuid/1, uuid/3,
         script/1, script/3,
         metadata/1, set_metadata/4,
         config/1, set_config/4
        ]).

-ignore_xref([merge/2, load/2, getter/2, uuid/1]).

-ignore_xref([
         set/4,
         name/1, name/3,
         uuid/1, uuid/3,
         script/1, script/3,
         metadata/1, set_metadata/4,
         config/1, set_config/4
        ]).


uuid(H) ->
    riak_dt_lwwreg:value(H#?DTRACE.uuid).

uuid({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DTRACE.uuid),
    H#?DTRACE{uuid = V1}.

name(H) ->
    riak_dt_lwwreg:value(H#?DTRACE.name).

name({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DTRACE.name),
    H#?DTRACE{name = V1}.

script(H) ->
    riak_dt_lwwreg:value(H#?DTRACE.script).

script({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DTRACE.script),
    H#?DTRACE{script = V1}.

metadata(H) ->
    fifo_map:value(H#?DTRACE.metadata).

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?DTRACE.metadata),
    G#?DTRACE{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?DTRACE.metadata),
    G#?DTRACE{metadata = M1}.

config(H) ->
    fifo_map:value(H#?DTRACE.config).

set_config({T, ID}, P, Value, User) when is_binary(P) ->
    set_config({T, ID}, fifo_map:split_path(P), Value, User);

set_config({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?DTRACE.config),
    G#?DTRACE{config = M1};

set_config({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?DTRACE.config),
    G#?DTRACE{config = M1}.

getter(#sniffle_obj{val=S0}, <<"name">>) ->
    name(S0);
getter(#sniffle_obj{val=S0}, <<"uuid">>) ->
    uuid(S0).

load(_, #?DTRACE{} = D) ->
    D;
load({T, ID}, D) ->
    {ok, UUID} = jsxd:get([<<"uuid">>], D),
    {ok, Name} = jsxd:get([<<"name">>], D),
    {ok, Script} = jsxd:get([<<"script">>], D),
    Metadata = jsxd:get([<<"metadata">>], [], D),
    {ok, Config} = jsxd:get([<<"config">>], D),

    {ok, UUID1} = ?NEW_LWW(UUID, T),
    {ok, Name1} = ?NEW_LWW(Name, T),
    {ok, Script1} = ?NEW_LWW(Script, T),
    Metadata1 = fifo_map:from_orddict(Metadata, ID, T),
    Config1 = fifo_map:from_orddict(Config, ID, T),
    D1 = #?DTRACE{
             config = Config1,
             metadata = Metadata1,
             script = Script1,
             name = Name1,
             uuid = UUID1
            },
    load({T, ID}, D1).

new(_) ->
    #?DTRACE{}.

to_json(D) ->
    [
     {<<"config">>, config(D)},
     {<<"metadata">>, metadata(D)},
     {<<"name">>, name(D)},
     {<<"script">>, script(D)},
     {<<"uuid">>, uuid(D)}
    ].

set(ID, [K], V, H) ->
    set(ID, K, V, H);

set({T, ID}, <<"config">>, V, H) ->
    H#?DTRACE{config = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"config.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"config">> | R], V, H) ->
    set_config(ID, R, V, H);

set({T, ID}, <<"metadata">>, V, H) ->
    H#?DTRACE{metadata = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H).

merge(#?DTRACE{
          config = Config1,
          metadata = Metadata1,
          script = Script1,
          name = Name1,
          uuid = UUID1
         },
      #?DTRACE{
          config = Config2,
          metadata = Metadata2,
          script = Script2,
          name = Name2,
          uuid = UUID2
         }) ->
    #?DTRACE{
        config = fifo_map:merge(Config1, Config2),
        metadata = fifo_map:merge(Metadata1, Metadata2),
        script = riak_dt_lwwreg:merge(Script1, Script2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2)
       }.
