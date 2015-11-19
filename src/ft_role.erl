%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_role).
-behaviour(fifo_dt).

-include("ft_role.hrl").
-define(OBJ, ?ROLE).
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

-type role() :: #?OBJ{}.
-export_type([role/0]).

?IS_A.

?G(<<"uuid">>, uuid);
?G_JSX.

new({_T, _ID}) ->
    #?ROLE{}.

%%-spec load({integer(), atom()}, any_role()) -> role().

load(_, #?ROLE{} = Role) ->
    Role;
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

to_json(#?ROLE{
            uuid = UUID,
            name = Name,
            permissions = Permissions,
            metadata = Metadata
           }) ->
    jsxd:from_list(
      [
       {<<"uuid">>, riak_dt_lwwreg:value(UUID)},
       {<<"name">>, riak_dt_lwwreg:value(Name)},
       {<<"permissions">>, riak_dt_orswot:value(Permissions)},
       {<<"metadata">>, fifo_map:value(Metadata)}
      ]).

-spec merge(role(), role()) -> role().

merge(#?ROLE{
          uuid = UUID1,
          name = Name1,
          permissions = Permissions1,
          metadata = Metadata1
         },
      #?ROLE{
          uuid = UUID2,
          name = Name2,
          permissions = Permissions2,
          metadata = Metadata2
         }) ->
    P1 = riak_dt_orswot:merge(Permissions1, Permissions2),
    #?ROLE{
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        permissions = P1,
        ptree = fifo_dt:to_ptree(P1),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.

name(Role) ->
    riak_dt_lwwreg:value(Role#?ROLE.name).

name({T, _ID}, Name, Role) ->
    {ok, V} = riak_dt_lwwreg:update({assign, Name, T}, none, Role#?ROLE.name),
    Role#?ROLE{name = V}.

uuid(Role) ->
    riak_dt_lwwreg:value(Role#?ROLE.uuid).

-spec uuid(Actor::term(), UUID::binary(), Role) ->
                  Role.
uuid({T, _ID}, UUID, Role = #?ROLE{}) ->
    {ok, V} = riak_dt_lwwreg:update({assign, UUID, T}, none, Role#?ROLE.uuid),
    Role#?ROLE{uuid = V}.

ptree(#?ROLE{ptree = PTree}) ->
    PTree.

permissions(Role) ->
    riak_dt_orswot:value(Role#?ROLE.permissions).

grant({_T, ID}, Permission, Role = #?ROLE{}) ->
    {ok, V} = riak_dt_orswot:update({add, Permission},
                                    ID, Role#?ROLE.permissions),
    Role#?ROLE{permissions = V, ptree = fifo_dt:to_ptree(V)}.


revoke({_T, ID}, Permission, Role) ->
    case riak_dt_orswot:update({remove, Permission}, ID,
                               Role#?ROLE.permissions) of
        {error, {precondition, {not_present, Permission}}} ->
            Role;
        {ok, V} ->
            Role#?ROLE{permissions = V, ptree = fifo_dt:to_ptree(V)}
    end.

revoke_prefix({_T, ID}, Prefix, Role) ->
    P0 = Role#?ROLE.permissions,
    Ps = permissions(Role),
    P1 = lists:foldl(fun (P, PAcc) ->
                             case lists:prefix(Prefix, P) of
                                 true ->
                                     {ok, V} = riak_dt_orswot:update(
                                                 {remove, P}, ID, PAcc),
                                     V;
                                 _ ->
                                     PAcc
                             end
                     end, P0, Ps),
    Role#?ROLE{
            permissions = P1,
            ptree = fifo_dt:to_ptree(P1)
           }.

metadata(Role) ->
    fifo_map:value(Role#?ROLE.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, Role) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, Role);

set_metadata({_T, ID}, Attribute, delete, Role) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Role#?ROLE.metadata),
    Role#?ROLE{metadata = M1};

set_metadata({T, ID}, Attribute, Value, Role) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Role#?ROLE.metadata),
    Role#?ROLE{metadata = M1}.
