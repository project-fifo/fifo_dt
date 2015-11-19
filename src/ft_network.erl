%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_network).
-behaviour(fifo_dt).

-include("ft_network.hrl").
-define(OBJ, ?NETWORK).
-include("ft_helper.hrl").

-export([
         is_a/1,
         new/1,
         load/2,
         to_json/1,
         set/4,
         name/1, name/3,
         uuid/1, uuid/3,
         add_iprange/3, remove_iprange/3, ipranges/1,
         metadata/1, set_metadata/3, set_metadata/4,
         getter/2,
         merge/2
        ]).

-ignore_xref([load/2, to_json/1, getter/2, merge/2]).

-ignore_xref([
              set/4,
              add_iprange/3, remove_iprange/3, ipranges/1,
              name/1, name/3,
              uuid/1, uuid/3,
              metadata/1, set_metadata/3, set_metadata/4
             ]).

-type network() :: #?OBJ{}.
-export_type([network/0]).

?IS_A.

new(_) ->
    #?NETWORK{}.

set({T, ID}, <<"metadata">>, V, H) ->
    H#?NETWORK{metadata = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H).

to_json(N) ->
    [
     {<<"ipranges">>, ipranges(N)},
     {<<"metadata">>, metadata(N)},
     {<<"name">>, name(N)},
     {<<"uuid">>, uuid(N)}
    ].

?G(uuid).
?G(name).

?S(uuid).
?S(name).

ipranges(H) ->
    riak_dt_orswot:value(H#?NETWORK.ipranges).

add_iprange({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?NETWORK.ipranges),
    H#?NETWORK{ipranges = O1}.

remove_iprange({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?NETWORK.ipranges) of
        {error, {precondition, {not_present, _}}} ->
            H;
        {ok, O1} ->
            H#?NETWORK{ipranges = O1}
    end.

?G(<<"name">>, name);
?G(<<"uuid">>, uuid);
?G_JSX.

load(_, #?NETWORK{} = N) ->
    N;

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


metadata(H) ->
    fifo_map:value(H#?NETWORK.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?NETWORK.metadata),
    G#?NETWORK{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?NETWORK.metadata),
    G#?NETWORK{metadata = M1}.

merge(#?NETWORK{
          ipranges = IPRanges1,
          metadata = Metadata1,
          name = Name1,
          uuid = UUID1
         },
      #?NETWORK{
          ipranges = IPRanges2,
          metadata = Metadata2,
          name = Name2,
          uuid = UUID2
         }) ->
    #?NETWORK{
        ipranges = riak_dt_orswot:merge(IPRanges1, IPRanges2),
        metadata = fifo_map:merge(Metadata1, Metadata2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2)
       }.
