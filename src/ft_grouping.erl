%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_grouping).

-include("ft_grouping.hrl").
-define(OBJ, ?GROUPING).
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

-type grouping() :: #?OBJ{}.
-export_type([grouping/0]).

?IS_A.

uuid(H) ->
    riak_dt_lwwreg:value(H#?GROUPING.uuid).

uuid({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?GROUPING.uuid),
    H#?GROUPING{uuid = V1}.

name(H) ->
    riak_dt_lwwreg:value(H#?GROUPING.name).

name({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?GROUPING.name),
    H#?GROUPING{name = V1}.

type(H) ->
    riak_dt_lwwreg:value(H#?GROUPING.type).

type({T, _ID}, V, H) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?GROUPING.type),
    H#?GROUPING{type = V1}.

elements(H) ->
    riak_dt_orswot:value(H#?GROUPING.elements).

add_element({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?GROUPING.elements),
    H#?GROUPING{elements = O1}.

remove_element({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?GROUPING.elements) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?GROUPING{elements = O1}
    end.

groupings(H) ->
    riak_dt_orswot:value(H#?GROUPING.groupings).

add_grouping({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?GROUPING.groupings),
    H#?GROUPING{groupings = O1}.

remove_grouping({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?GROUPING.groupings) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?GROUPING{groupings = O1}
    end.

?G(<<"name">>, name);
?G(<<"uuid">>, uuid);
?G(<<"elements">>, elements);
?G(<<"groupings">>, groupings);
?G(<<"type">>, type);
?G_JSX.


load(_, #?GROUPING{} = G) ->
    G.

new({T, _ID}) ->
    {ok, Name} = ?NEW_LWW(<<>>, T),
    {ok, Type} = ?NEW_LWW(none, T),
    #?GROUPING{
        name = Name,
        type = Type
       }.

metadata(G) ->
    fifo_map:value(G#?GROUPING.metadata).

set_metadata(ID, [{K, V} | R] , Vm) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Vm));

set_metadata(_ID, _, Vm) ->
    Vm.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?GROUPING.metadata),
    G#?GROUPING{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?GROUPING.metadata),
    G#?GROUPING{metadata = M1}.

to_json(G) ->
    [
     {<<"elements">>, elements(G)},
     {<<"groupings">>, groupings(G)},
     {<<"metadata">>, metadata(G)},
     {<<"name">>, name(G)},
     {<<"type">>, list_to_binary(atom_to_list(type(G)))},
     {<<"uuid">>, uuid(G)}
    ].

merge(#?GROUPING{
          elements = Elements1,
          groupings = Groupings1,
          metadata = Metadata1,
          name = Name1,
          type = Type1,
          uuid = UUID1
         },
      #?GROUPING{
          elements = Elements2,
          groupings = Groupings2,
          metadata = Metadata2,
          name = Name2,
          type = Type2,
          uuid = UUID2
         }) ->
    #?GROUPING{
        elements = riak_dt_orswot:merge(Elements1, Elements2),
        groupings = riak_dt_orswot:merge(Groupings1, Groupings2),
        metadata = fifo_map:merge(Metadata1, Metadata2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        type = riak_dt_lwwreg:merge(Type1, Type2),
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2)
       }.
