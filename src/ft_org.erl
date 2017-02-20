%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_org).
-behaviour(fifo_dt).

-include("ft_org.hrl").
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
         resources/1, resource/2, resource_inc/4, resource_dec/4,
         resource_remove/3,
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
              resources/1, resource/2, resource_inc/4, resource_dec/4,
              resource_remove/3,
              is_a/1
             ]).

-type org() :: #{
           type           => ?TYPE,
           version        => non_neg_integer(),
           uuid           => riak_dt_lwwreg:lwwreg(),
           name           => riak_dt_lwwreg:lwwreg(),
           triggers       => riak_dt_map:riak_dt_map(),
           resources      => riak_dt_map:map(),
           metadata       => riak_dt_map:riak_dt_map()
          }.
-export_type([org/0]).

?IS_A.

?G(<<"uuid">>, uuid);
?G(<<"name">>, name);
?G_JSX.

new(_) ->
    #{
     type        => ?TYPE,
     version     => ?VERSION,
     uuid        => riak_dt_lwwreg:new(),
     name        => riak_dt_lwwreg:new(),
     triggers    => riak_dt_map:new(),
     resources   => ft_cmap:new(),
     metadata    => riak_dt_map:new()
    }.

%%-spec load({non_neg_integer(), atom()}, any_organisation()) -> organisation().

load(_, #{type := Type})  when Type =/= ?TYPE ->
    error(bad_arg);

load(_, #{version := ?VERSION} = Org) ->
    Org;

load(TID, #{version := 0} = Org) ->
    load(TID, Org#{resources => ft_cmap:new(), version => 1});

load(TID, #organisation_1{
             uuid = UUID,
             name = Name,
             triggers = Triggers,
             metadata = Metadata
            }) ->
    load(TID,
         #{
           type      => ?TYPE,
           version   => 0,
           uuid      => UUID,
           name      => Name,
           resources => #{},
           triggers  => Triggers,
           metadata  => Metadata
          });

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
    grant(<<"role_grant">>, Target, Permission);

jsonify_action({grant, user, Target, Permission}) ->
    grant(<<"user_grant">>, Target, Permission);

jsonify_action({join, org, Org}) ->
    join(<<"join_org">>, Org);

jsonify_action({join, role, Role}) ->
    join(<<"join_role">>, Role).

join(Action, Role) ->
    [{<<"action">>, Action},
     {<<"target">>, Role}].

grant(Action, Target, Permission) ->
    [{<<"action">>, Action},
     {<<"permission">>, jsonify_permission(Permission)},
     {<<"target">>, Target}].

jsonify_permission(Permission) ->
    lists:map(fun (placeholder) ->
                      <<"$">>;
                  (E) ->
                      E
              end, Permission).

-spec to_json(Org::org()) -> fifo:attr_list().
to_json(Org) ->
    Triggers = maps:map(fun(_, V) -> jsonify_trigger(V) end, triggers(Org)),
    jsxd:from_list(
      [
       {<<"uuid">>, uuid(Org)},
       {<<"name">>, name(Org)},
       {<<"triggers">>, Triggers},
       {<<"resources">>, resources(Org)},
       {<<"metadata">>, metadata(Org)}
      ]).

merge(O = #{
        type      := ?TYPE,
        uuid      := UUID1,
        name      := Name1,
        triggers  := Triggers1,
        resources := Resources1,
        metadata  := Metadata1
       },
      #{
         type      := ?TYPE,
         uuid      := UUID2,
         name      := Name2,
         triggers  := Triggers2,
         resources := Resources2,
         metadata  := Metadata2
       }) ->
    O#{
      uuid      := riak_dt_lwwreg:merge(UUID1, UUID2),
      name      := riak_dt_lwwreg:merge(Name1, Name2),
      triggers  := fifo_map:merge(Triggers1, Triggers2),
      resources := ft_cmap:merge(Resources1, Resources2),
      metadata  := fifo_map:merge(Metadata1, Metadata2)
     }.

resources(#{type := ?TYPE, resources := Rs}) ->
    ft_cmap:value(Rs).

resource(#{type := ?TYPE, resources := Rs}, K) ->
    ft_cmap:get(K, Rs).

resource_inc({_T, ID}, K, V, O = #{type := ?TYPE, resources := Rs}) ->
    {ok, Rs1} = ft_cmap:inc(ID, K, V, Rs),
    O#{resources := Rs1}.

resource_remove({_T, ID}, K, O = #{type := ?TYPE, resources := Rs}) ->
    {ok, Rs1} = ft_cmap:remove(ID, K, Rs),
    O#{resources := Rs1}.

resource_dec({_T, ID}, K, V, O = #{type := ?TYPE, resources := Rs}) ->
    {ok, Rs1} = ft_cmap:dec(ID, K, V, Rs),
    O#{resources := Rs1}.

?REG_GET(name).
?REG_SET(name).
?REG_GET(uuid).
?REG_SET(uuid).

-spec triggers(Org::org()) -> #{fifo:uuid() => Trigger::term()}.

?MAP_GET(triggers).
?MAP_SET(add_trigger, triggers).
?MAP_REM(remove_trigger, triggers).

remove_target(TID, Target, Org) ->
    Triggers = maps:to_list(triggers(Org)),
    GrantTriggers = [UUID || {UUID, {_, {grant, _, T, _}}} <- Triggers,
                             T =:= Target],
    JoinTriggers = [UUID || {UUID, {_, {join, _, T}}} <- Triggers,
                            T =:= Target],
    lists:foldl(fun(UUID, Acc) ->
                        remove_trigger(TID, UUID, Acc)
                end, Org, GrantTriggers ++ JoinTriggers).

?META.
?SET_META_3.
?SET_META_4.
