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

-ifdef(TEST).
-export([update_triggers/2, jsonify_trigger/1, res_json/3]).
-endif.

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         triggers/1, add_trigger/4, remove_trigger/3,
         metadata/1, set_metadata/3, set_metadata/4,
         resource_action/6, resources/1,
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
              resource_action/6, resources/1,
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
    load(TID, O);

load(TID,
     #organisation_0_1_2{
        uuid = UUID,
        name = Name,
        triggers = Triggers,
        metadata = Metadata
       }) ->
    O = #organisation_0_1_3{
           uuid = UUID,
           name = Name,
           triggers = Triggers,
           metadata = Metadata
          },
    load(TID, update_triggers(TID, O));

load({T, ID},
     #organisation_0_1_1{
        uuid = UUID,
        name = Name,
        triggers = Triggers,
        metadata = Metadata
       }) ->
    UUIDb = riak_dt_lwwreg:value(UUID),
    Ts = [{trigger_uuid(UUIDb, Tr), Tr} || Tr <- riak_dt_orswot:value(Triggers)],
    Triggers1 = fifo_map:from_orddict(orddict:from_list(Ts), ID, T),
    load({T, ID},
         #organisation_0_1_2{
            uuid = UUID,
            name = Name,
            triggers = Triggers1,
            metadata = Metadata
           });

load({T, ID},
     #organisation_0_1_0{
        uuid = UUID,
        name = Name,
        triggers = Triggers,
        metadata = Metadata
       }) ->
    {ok, UUID1} = ?NEW_LWW(vlwwregister:value(UUID), T),
    {ok, Name1} = ?NEW_LWW(vlwwregister:value(Name), T),
    {ok, Triggers1} = ?CONVERT_VORSET(Triggers),
    Metadata1 = fifo_map:from_orddict(statebox:value(Metadata), ID, T),
    load({T, ID},
         #organisation_0_1_1{
            uuid = UUID1,
            name = Name1,
            triggers = Triggers1,
            metadata = Metadata1
           });

load({T, ID}, OrgSB) ->
    Size = 50,
    Org = statebox:value(OrgSB),
    {ok, Name} = jsxd:get([<<"name">>], Org),
    {ok, UUID} = jsxd:get([<<"uuid">>], Org),
    ID0 = {{0,0,0}, load},
    Triggers0 = jsxd:get([<<"triggers">>], [], Org),
    Metadata = jsxd:get([<<"metadata">>], [], Org),
    Triggers = lists:foldl(
                 fun (G, Acc) ->
                         vorsetg:add(ID0, G, Acc)
                 end, vorsetg:new(Size), Triggers0),
    load({T, ID},
         #organisation_0_1_0{
            uuid = vlwwregister:new(UUID),
            name = vlwwregister:new(Name),
            triggers = Triggers,
            metadata = statebox:new(fun () -> Metadata end)
           }).

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
    Res = lists:foldl(fun ({E, T, A, O}, [{E, D} | Acc]) ->
                              [{E, [res_json(T, A, O) | D]} | Acc];
                          ({E, T, A, O}, Acc) ->
                              [{E, [res_json(T, A, O)]} | Acc]
                      end, [], resources(Org)),
    jsxd:from_list(
      [
       {<<"uuid">>, uuid(Org)},
       {<<"name">>, name(Org)},
       {<<"resources">>, Res},
       {<<"triggers">>, [{U, jsonify_trigger(T)} || {U, T} <- triggers(Org)]},
       {<<"metadata">>, metadata(Org)}
      ]).

res_json(T, A, O) ->
    [
     {<<"action">>, a2b(A)},
     {<<"opts">>, jsxd:from_list([{a2b(K), V} || {K, V} <- O])},
     {<<"time">>, T}
    ].

merge(#?ORG{
          uuid = UUID1,
          name = Name1,
          triggers = Triggers1,
          resources = Res1,
          metadata = Metadata1
         },
      #?ORG{
          uuid = UUID2,
          name = Name2,
          triggers = Triggers2,
          resources = Res2,
          metadata = Metadata2
         }) ->
    #?ORG{
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        triggers = fifo_map:merge(Triggers1, Triggers2),
        resources = riak_dt_orswot:merge(Res1, Res2),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.

resource_action({_T, ID}, Resource, TimeStamp, Action, Opts, Org) ->
    {ok, Res} = riak_dt_orswot:update({add, {Resource, TimeStamp, Action, Opts}}, ID,
                                      Org#?ORG.resources),
    Org#?ORG{
           resources = Res
          }.

resources(Org) ->
    riak_dt_orswot:value(Org#?ORG.resources).

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

-spec trigger_uuid(UUID::uuid:uuid_string(), Trigger::term()) -> uuid:uuid().

trigger_uuid(UUID, Trigger) ->
    list_to_binary(uuid:to_string(uuid:uuid5(UUID, term_to_binary(Trigger)))).


remove_target(TID, Target, Org) ->
    Triggers = triggers(Org),
    GrantTriggers = [UUID || {UUID, {_, {grant, _, T, _}}} <- Triggers,
                             T =:= Target],
    JoinTriggers = [UUID || {UUID, {_, {join, _, T}}} <- Triggers,
                            T =:= Target],
    lists:foldl(fun(UUID, Acc) ->
                        remove_trigger(TID, UUID, Acc)
                end, Org, GrantTriggers ++ JoinTriggers).


update_triggers({T, ID}, O = #organisation_0_1_3{triggers = Triggers}) ->
    T1 = lists:foldl(fun ({UUID, {A, {grant, group, R, Tr}}}, Acc) ->
                             {ok, Acc1} = fifo_map:remove(UUID, ID, Acc),
                             Trig = {A, {grant, role, R, replace_group(Tr)}},
                             {ok, Acc2} = fifo_map:set(UUID, {reg, Trig}, ID, T,
                                                       Acc1),
                             Acc2;
                         ({UUID, {A, {grant, E, R, Tr}}}, Acc) ->
                             {ok, Acc1} = fifo_map:remove(UUID, ID, Acc),
                             Trig = {A, {grant, E, R, replace_group(Tr)}},
                             {ok, Acc2} = fifo_map:set(UUID, {reg, Trig}, ID, T,
                                                       Acc1),
                             Acc2;
                         ({UUID, {A, {join, group, R}}}, Acc) ->
                             {ok, Acc1} = fifo_map:remove(UUID, ID, Acc),
                             Tr = {A, {join, role, R}},
                             {ok, Acc2} = fifo_map:set(UUID, {reg, Tr}, ID, T,
                                                       Acc1),
                             Acc2;
                         (_, Acc) ->
                             Acc
                     end, Triggers, fifo_map:value(Triggers)),
    O#organisation_0_1_3{triggers = T1}.

replace_group(R) ->
    replace_group(R, []).

replace_group([<<"groups">> | R], Acc) ->
    replace_group(R, [<<"roles">> | Acc]);

replace_group([F | R], Acc) ->
    replace_group(R, [F | Acc]);

replace_group([], Acc) ->
    lists:reverse(Acc).

a2b(A) ->
    list_to_binary(atom_to_list(A)).
