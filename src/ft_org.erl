%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>


-module(ft_org).

-include("ft_org.hrl").
-define(OBJ, ?ORG).
-include("ft_helper.hrl").

-ifdef(EQC).
-export([jsonify_trigger/1]).
-endif.

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         triggers/1, add_trigger/4, remove_trigger/3,
         metadata/1, set_metadata/3, set_metadata/4,
         remove_target/3,
         merge/2,
         to_json/1,
         getter/2,
         is_a/1
        ]).

-ignore_xref([
              new/1,
              load/2,
              uuid/1, uuid/3,
              name/1, name/3,
              triggers/1, add_trigger/4, remove_trigger/3,
              metadata/1, set_metadata/3, set_metadata/4,
              remove_target/3,
              merge/2,
              to_json/1,
              getter/2,
              is_a/1
             ]).

-type org() :: #?OBJ{}.
-export_type([org/0]).

?IS_A.

?G(<<"uuid">>, uuid);
?G(<<"name">>, name);
?G_JSX.

new(_) ->
    #?ORG{}.

%%-spec load({non_neg_integer(), atom()}, any_organisation()) -> organisation().

load(_, #?ORG{} = Org) ->
    Org;

load(TID, #organisation_0{
           uuid = UUID,
           name = Name,
           triggers = Triggers,
           metadata = Metadata
          }) ->
    O = #organisation_1{
           uuid = UUID,
           name = Name,
           triggers = Triggers,
           metadata = Metadata
          },
    load(TID, O);

load(TID, #organisation_0_1_5{
           uuid = UUID,
           name = Name,
           resources = Resources,
           triggers = Triggers,
           metadata = Metadata
          }) ->
    O = #organisation_0{
           uuid = UUID,
           name = Name,
           resources = fifo_dt:update_set(Resources),
           triggers = fifo_dt:update_map(Triggers),
           metadata = fifo_dt:update_map(Metadata)
          },
    load(TID, O);

load(TID,
     #organisation_0_1_4{
        uuid = UUID,
        name = Name,
        triggers = Triggers,
        resources = Resources,
        metadata = Metadata
       }) ->
    O = #organisation_0_1_5{
           uuid = UUID,
           name = Name,
           resources = Resources,
           triggers = Triggers,
           metadata = Metadata
          },
    load(TID, O);

load(TID,
     #organisation_0_1_3{
        uuid = UUID,
        name = Name,
        triggers = Triggers,
        metadata = Metadata
       }) ->
    O = #organisation_0_1_4{
           uuid = UUID,
           name = Name,
           triggers = Triggers,
           metadata = Metadata
          },
    load(TID, O).

jsonify_trigger({Trigger, Action}) ->
    jsxd:set(<<"trigger">>, list_to_binary(atom_to_list(Trigger)),
             jsonify_action(Action)).

jsonify_action({grant, role, Target, Permission}) ->
    [{<<"action">>, <<"role_grant">>},
     {<<"permission">>, jsonify_permission(Permission)},
     {<<"target">>, Target}];

jsonify_action({grant, user, Target, Permission}) ->
    [{<<"action">>, <<"user_grant">>},
     {<<"permission">>, jsonify_permission(Permission)},
     {<<"target">>, Target}];

jsonify_action({join, org, Org}) ->
    [{<<"action">>, <<"join_org">>},
     {<<"target">>, Org}];

jsonify_action({join, role, Role}) ->
    [{<<"action">>, <<"join_role">>},
     {<<"target">>, Role}].


jsonify_permission(Permission) ->
    lists:map(fun (placeholder) ->
                      <<"$">>;
                  (E) ->
                      E
              end, Permission).

-spec to_json(Org::org()) -> fifo:attr_list().
to_json(Org) ->
    jsxd:from_list(
      [
       {<<"uuid">>, uuid(Org)},
       {<<"name">>, name(Org)},
       {<<"triggers">>, [{U, jsonify_trigger(T)} || {U, T} <- triggers(Org)]},
       {<<"metadata">>, metadata(Org)}
      ]).

merge(#?ORG{
          uuid = UUID1,
          name = Name1,
          triggers = Triggers1,
          metadata = Metadata1
         },
      #?ORG{
          uuid = UUID2,
          name = Name2,
          triggers = Triggers2,
          metadata = Metadata2
         }) ->
    #?ORG{
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        triggers = fifo_map:merge(Triggers1, Triggers2),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.

?G(name).
?S(name).
?G(uuid).
?S(uuid).

-spec triggers(Org::org()) -> [{ID::fifo:uuid(), Trigger::term()}].

triggers(Org) ->
    fifo_map:value(Org#?ORG.triggers).

add_trigger({T, ID}, UUID, Trigger, Org) ->
    {ok, T1} = fifo_map:set(UUID, {reg, Trigger}, ID, T, Org#?ORG.triggers),
    Org#?ORG{triggers = T1}.

remove_trigger({_T, ID}, Trigger, Org) ->
    {ok, V} = fifo_map:remove(Trigger, ID, Org#?ORG.triggers),
    Org#?ORG{triggers = V}.

metadata(Org) ->
    fifo_map:value(Org#?ORG.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, Org) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, Org);

set_metadata({_T, ID}, Attribute, delete, Org) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Org#?ORG.metadata),
    Org#?ORG{metadata = M1};

set_metadata({T, ID}, Attribute, Value, Org) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Org#?ORG.metadata),
    Org#?ORG{metadata = M1}.

remove_target(TID, Target, Org) ->
    Triggers = triggers(Org),
    GrantTriggers = [UUID || {UUID, {_, {grant, _, T, _}}} <- Triggers,
                             T =:= Target],
    JoinTriggers = [UUID || {UUID, {_, {join, _, T}}} <- Triggers,
                            T =:= Target],
    lists:foldl(fun(UUID, Acc) ->
                        remove_trigger(TID, UUID, Acc)
                end, Org, GrantTriggers ++ JoinTriggers).
