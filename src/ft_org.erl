%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_org).

-include("ft.hrl").
-define(OBJ, ?ORG).
-include("ft_helper.hrl").

-ifdef(TEST).
-export([update_triggers/2, jsonify_trigger/1]).
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

-export_type([organisation/0, any_organisation/0]).

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

-type organisation() :: #?ORG{}.

-type any_organisation() :: organisation() |
                            #organisation_0_1_0{} |
                            #organisation_0_1_1{} |
                            #organisation_0_1_2{} |
                            statebox:statebox().

?IS_A.

?G(<<"uuid">>, uuid);
?G_JSX.

new(_) ->
    #?ORG{}.

%%-spec load({non_neg_integer(), atom()}, any_organisation()) -> organisation().

load(_, #?ORG{} = Org) ->
    Org;

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

-spec to_json(Org::organisation()) -> fifo:org().
to_json(#?ORG{
            uuid = UUID,
            name = Name,
            triggers = Triggers,
            metadata = Metadata
           }) ->
    jsxd:from_list(
      [
       {<<"uuid">>, riak_dt_lwwreg:value(UUID)},
       {<<"name">>, riak_dt_lwwreg:value(Name)},
       {<<"triggers">>, [{U, jsonify_trigger(T)} || {U, T} <- fifo_map:value(Triggers)]},
       {<<"metadata">>, fifo_map:value(Metadata)}
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

name(Org) ->
    riak_dt_lwwreg:value(Org#?ORG.name).

name({T, _ID}, Name, Org) ->
    {ok, V} = riak_dt_lwwreg:update({assign, Name, T}, none, Org#?ORG.name),
    Org#?ORG{name = V}.

uuid(Org) ->
    riak_dt_lwwreg:value(Org#?ORG.uuid).

uuid({T, _ID}, UUID, Org) ->
    {ok, V} = riak_dt_lwwreg:update({assign, UUID, T}, none, Org#?ORG.uuid),
    Org#?ORG{uuid = V}.

-spec triggers(Org::organisation()) -> [{ID::fifo:uuid(), Trigger::term()}].

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


update_triggers(TID, Org) ->
    lists:foldl(fun ({UUID, {A, {grant, group, R, T}}}, Acc) ->
                        Acc1 = remove_trigger(TID, UUID, Acc),
                        add_trigger(TID, UUID, {A, {grant, role, R, replace_group(T)}}, Acc1);
                    ({UUID, {A, {grant, E, R, T}}}, Acc) ->
                        Acc1 = remove_trigger(TID, UUID, Acc),
                        add_trigger(TID, UUID, {A, {grant, E, R, replace_group(T)}}, Acc1);
                    ({UUID, {A, {join, group, R}}}, Acc) ->
                        Acc1 = remove_trigger(TID, UUID, Acc),
                        add_trigger(TID, UUID, {A, {join, role, R}}, Acc1);
                    (_, Acc) ->
                        Acc
                end, Org, triggers(Org)).

replace_group(R) ->
    replace_group(R, []).

replace_group([<<"groups">> | R], Acc) ->
    replace_group(R, [<<"roles">> | Acc]);

replace_group([F | R], Acc) ->
    replace_group(R, [F | Acc]);

replace_group([], Acc) ->
    lists:reverse(Acc).
