%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_role).
-behaviour(fifo_dt).

-include("ft_role.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         ptree/1,
         permissions/1, grant/3, revoke/3, revoke_prefix/3,
         metadata/1, set_metadata/3, set_metadata/4,
         merge/2,
         to_json/1,
         getter/2,
         is_a/1
        ]).

-ignore_xref([
              new/1,
              load/2,
              uuid/1, uuid/2,
              name/1, name/2,
              ptree/1,
              permissions/1, grant/3, revoke/3, revoke_prefix/3,
              metadata/1, set_metadata/3, set_metadata/4,
              getter/2,
              to_json/1
             ]).

-type role() :: #{
            type        => ?TYPE,
            version        => pos_integer(),
            uuid        => riak_dt_lwwreg:lwwreg(),
            name        => riak_dt_lwwreg:lwwreg(),
            permissions => riak_dt_orswot:orswot(),
            ptree       => term(),
            metadata    => riak_dt_map:riak_dt_map()
         }.
-export_type([role/0]).

?IS_A.

?G(<<"uuid">>, uuid);
?G_JSX.

new({_T, _ID}) ->
    #{
     type        => ?TYPE,
     version     => ?VERSION,
     uuid        => riak_dt_lwwreg:new(),
     name        => riak_dt_lwwreg:new(),
     permissions => riak_dt_orswot:new(),
     ptree       => libsnarlmatch_tree:new(),
     metadata    => riak_dt_map:new()
    }.

%%-spec load({integer(), atom()}, any_role()) -> role().

load(_, #{type := ?TYPE, version := ?VERSION} = Role) ->
    Role;

load(TID, #role_1{
             uuid = UUID,
           name = Name,
           permissions = Permissions,
           metadata = Metadata
            }) ->
    Role = #{
      type        => ?TYPE,
      version     => 0,
      uuid        => UUID,
      name        => Name,
      permissions => Permissions,
      ptree       => fifo_dt:to_ptree(Permissions),
      metadata    => Metadata
     },
    load(TID, Role);

load(TID, #role_0{
           uuid = UUID,
           name = Name,
           permissions = Permissions,
           ptree = PTree,
           metadata = Metadata
          }) ->
    R = #role_1{
           uuid = UUID,
           name = Name,
           permissions = fifo_dt:update_set(Permissions),
           ptree = PTree,
           metadata = fifo_dt:update_map(Metadata)
          },
    load(TID, R);

load(TID, #role_0_1_0{
            uuid = UUID,
            name = Name,
            permissions = Permissions,
            metadata = Metadata
           }) ->
    R = #role_0{
           uuid = UUID,
           name = Name,
           permissions = Permissions,
           ptree = fifo_dt:to_ptree(fifo_dt:update_set(Permissions)),
           metadata = Metadata
          },
    load(TID, R).

to_json(Role) ->
    jsxd:from_list(
      [
       {<<"uuid">>, uuid(Role)},
       {<<"name">>, name(Role)},
       {<<"permissions">>, permissions(Role)},
       {<<"metadata">>, metadata(Role)}
      ]).

-spec merge(role(), role()) -> role().

merge(R = #{
        type := ?TYPE,
        uuid := UUID1,
        name := Name1,
        permissions := Permissions1,
        metadata := Metadata1
       },
      #{
         type         := ?TYPE,
         uuid         := UUID2,
         name         := Name2,
         permissions  := Permissions2,
         metadata     := Metadata2
       }) ->
    P1 = riak_dt_orswot:merge(Permissions1, Permissions2),
    R#{
      uuid        => riak_dt_lwwreg:merge(UUID1, UUID2),
      name        => riak_dt_lwwreg:merge(Name1, Name2),
      permissions => P1,
      ptree       => fifo_dt:to_ptree(P1),
      metadata    => fifo_map:merge(Metadata1, Metadata2)
     }.

-spec name(role()) -> binary().
?REG_GET(name).
-spec name(fifo_dt:tid(), binary(), role()) -> role().
?REG_SET(name).

-spec uuid(role()) -> binary().
?REG_GET(uuid).
-spec uuid(fifo_dt:tid(), binary(), role()) -> role().
?REG_SET(uuid).

ptree(#{type := ?TYPE, ptree := PTree}) ->
    PTree.

?SET_GET(permissions).

grant({_T, ID}, P, Role = #{type := ?TYPE, permissions := Ps0}) ->
    {ok, Ps1} = riak_dt_orswot:update({add, P}, ID, Ps0),
    Role#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}.


revoke({_T, ID}, P, Role = #{type := ?TYPE, permissions := Ps0}) ->
    case riak_dt_orswot:update({remove, P}, ID, Ps0) of
        {error, {precondition, {not_present, P}}} ->
            Role;
        {ok, Ps1} ->
            Role#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}
    end.

revoke_prefix({_T, ID}, Prefix, Role = #{type := ?TYPE, permissions := Ps0}) ->
    PVs = permissions(Role),
    Ps1 = lists:foldl(fun (P, PAcc) ->
                              case lists:prefix(Prefix, P) of
                                  true ->
                                      {ok, R} = riak_dt_orswot:update(
                                                  {remove, P}, ID, PAcc),
                                      R;
                                  _ ->
                                      PAcc
                              end
                      end, Ps0, PVs),
    Role#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}.

?META.
?SET_META_3.
?SET_META_4.
