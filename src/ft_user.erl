%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_user).

-include("ft.hrl").
-define(OBJ, ?USER).
-include("ft_helper.hrl").


-export([update_permissions/2]).
-ignore_xref([update_permissions/2]).

-export([new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         password/1, password/3,
         permissions/1, grant/3, revoke/3, revoke_prefix/3,
         roles/1, join/3, leave/3,
         join_org/3, leave_org/3, select_org/3, orgs/1, active_org/1,
         add_key/4, revoke_key/3, keys/1,
         metadata/1, set_metadata/4,
         add_yubikey/3, yubikeys/1, remove_yubikey/3,
         merge/2,
         to_json/1,
         is_a/1, getter/2
        ]).

-export_type([user/0, any_user/0]).

-ignore_xref([
              load/1,
              uuid/1, uuid/3,
              name/1, name/3,
              password/1, password/3,
              permissions/1, grant/3, revoke/3, revoke_prefix/3,
              roles/1, join/3, leave/3,
              add_key/4, revoke_key/3, keys/1,
              metadata/1, set_metadata/4,
              join_org/3, leave_org/3, select_org/3, orgs/1, active_org/1,
              add_yubikey/3, yubikeys/1, remove_yubikey/3,
              merge/2, getter/2,
              to_json/1
             ]).

-type user() :: #?USER{}.

-type any_user() :: user() |
                    #user_0_1_5{} |
                    #user_0_1_4{} |
                    #user_0_1_3{} |
                    #user_0_1_2{} |
                    #user_0_1_1{} |
                    #user_0_1_0{} |
                    statebox:statebox().

?G(<<"uuid">>, uuid).

is_a(#?USER{}) ->
    true;
is_a(_) ->
    false.

%% -spec load({non_neg_integer(), atom()}, any_user()) -> user().

load(_, #?USER{} = User) ->
    User;

load(TID,
     #user_0_1_4{
        uuid = UUID,
        name = Name,
        password = Passwd,
        active_org = ActiveOrg,
        permissions = Permissions,
        roles = Roles,
        ssh_keys = Keys,
        orgs = Orgs,
        yubikeys = YubiKeys,
        metadata = Metadata
       }) ->
    U = #user_0_1_5{
           uuid = UUID,
           name = Name,
           password = Passwd,
           active_org = ActiveOrg,
           permissions = Permissions,
           roles = Roles,
           ssh_keys = Keys,
           orgs = Orgs,
           yubikeys = YubiKeys,
           metadata = Metadata
          },
    load(TID, update_permissions(TID, U));

load(TID,
     #user_0_1_3{
        uuid = UUID1,
        name = Name1,
        password = Passwd1,
        active_org = ActiveOrg1,
        permissions = Permissions1,
        roles = Roles1,
        ssh_keys = Keys1,
        orgs = Orgs1,
        metadata = Metadata1
       }) ->
    load(TID, #user_0_1_4{
                 uuid = UUID1,
                 name = Name1,
                 password = Passwd1,
                 active_org = ActiveOrg1,
                 permissions = Permissions1,
                 roles = Roles1,
                 ssh_keys = Keys1,
                 orgs = Orgs1,
                 yubikeys = riak_dt_orswot:new(),
                 metadata = Metadata1
                });

load({T, ID},
     #user_0_1_2{
        uuid = UUID,
        name = Name,
        password = Passwd,
        permissions = Permissions,
        active_org = ActiveOrg,
        roles = Roles,
        ssh_keys = Keys,
        orgs = Orgs,
        metadata = Metadata}) ->
    {ok, UUID1} = ?NEW_LWW(vlwwregister:value(UUID), T),
    {ok, Name1} = ?NEW_LWW(vlwwregister:value(Name), T),
    {ok, Passwd1} = ?NEW_LWW(vlwwregister:value(Passwd), T),
    {ok, ActiveOrg1} = ?NEW_LWW(vlwwregister:value(ActiveOrg), T),
    {ok, Permissions1} = ?CONVERT_VORSET(Permissions),
    {ok, Roles1} = ?CONVERT_VORSET(Roles),
    {ok, Keys1} = ?CONVERT_VORSET(Keys),
    {ok, Orgs1} = ?CONVERT_VORSET(Orgs),
    Metadata1 = fifo_map:from_orddict(statebox:value(Metadata), ID, T),
    load({T, ID},
         #user_0_1_3{
            uuid = UUID1,
            name = Name1,
            password = Passwd1,
            active_org = ActiveOrg1,
            permissions = Permissions1,
            roles = Roles1,
            ssh_keys = Keys1,
            orgs = Orgs1,
            metadata = Metadata1
           });

load(CT,
     #user_0_1_1{
        uuid = UUID,
        name = Name,
        password = Passwd,
        permissions = Permissions,
        roles = Roles,
        ssh_keys = Keys,
        metadata = Metadata
       }) ->
    Size = 50,
    load(CT,
         #user_0_1_2{
            uuid = UUID,
            name = Name,
            password = Passwd,
            permissions = Permissions,
            active_org = vlwwregister:new(<<"">>),
            roles = Roles,
            ssh_keys = Keys,
            orgs = vorsetg:new(Size),
            metadata = Metadata
           });

load(CT,
     #user_0_1_0{
        uuid = UUID,
        name = Name,
        password = Passwd,
        permissions = Permissions,
        roles = Roles,
        metadata = Metadata
       }) ->
    Size = 50,
    load(CT,
         #user_0_1_1{
            uuid = UUID,
            name = Name,
            password = Passwd,
            permissions = Permissions,
            roles = Roles,
            ssh_keys = vorsetg:new(Size),
            metadata = Metadata});

load(CT, UserSB) ->
    Size = 50,
    User = statebox:value(UserSB),
    {ok, Name} = jsxd:get([<<"name">>], User),
    {ok, UUID} = jsxd:get([<<"uuid">>], User),
    Password = jsxd:get([<<"password">>], <<>>, User),
    ID0 = {{0,0,0}, load},
    Roles0 = jsxd:get([<<"roles">>], [], User),
    Permissions0 = jsxd:get([<<"permissions">>], [], User),
    Metadata = jsxd:get([<<"metadata">>], [], User),
    Roles = lists:foldl(
              fun (G, Acc) ->
                      vorsetg:add(ID0, G, Acc)
              end, vorsetg:new(Size), Roles0),
    Permissions = lists:foldl(
                    fun (G, Acc) ->
                            vorsetg:add(ID0, G, Acc)
                    end, vorsetg:new(Size), Permissions0),
    load(CT,
         #user_0_1_0{
            uuid = vlwwregister:new(UUID),
            name = vlwwregister:new(Name),
            password = vlwwregister:new(Password),
            roles = Roles,
            permissions = Permissions,
            metadata = statebox:new(fun() -> Metadata end)}).

new({T, _ID}) ->
    {ok, UUID} = ?NEW_LWW(<<>>, T),
    {ok, Name} = ?NEW_LWW(<<>>, T),
    {ok, Passwd} = ?NEW_LWW(<<>>, T),
    {ok, ActiveOrg} = ?NEW_LWW(<<>>, T),
    #?USER{
        uuid = UUID,
        name = Name,
        password = Passwd,
        active_org = ActiveOrg,
        roles = riak_dt_orswot:new(),
        permissions = riak_dt_orswot:new(),
        yubikeys = riak_dt_orswot:new(),
        ssh_keys = riak_dt_orswot:new(),
        orgs = riak_dt_orswot:new(),
        metadata = fifo_map:new()
       }.

to_json(#?USER{
            uuid = UUID,
            name = Name,
            roles = Roles,
            ssh_keys = Keys,
            permissions = Permissions,
            active_org = Org,
            orgs = Orgs,
            yubikeys = YubiKeys,
            metadata = Metadata
           }) ->
    jsxd:from_list(
      [
       {<<"uuid">>, riak_dt_lwwreg:value(UUID)},
       {<<"name">>, riak_dt_lwwreg:value(Name)},
       {<<"roles">>, riak_dt_orswot:value(Roles)},
       {<<"permissions">>, riak_dt_orswot:value(Permissions)},
       {<<"yubikeys">>, riak_dt_orswot:value(YubiKeys)},
       {<<"keys">>, riak_dt_orswot:value(Keys)},
       {<<"org">>, riak_dt_lwwreg:value(Org)},
       {<<"orgs">>, riak_dt_orswot:value(Orgs)},
       {<<"metadata">>, fifo_map:value(Metadata)}
      ]).

merge(#?USER{
          uuid = UUID1,
          name = Name1,
          password = Password1,
          roles = Roles1,
          permissions = Permissions1,
          ssh_keys = Keys1,
          active_org = Org1,
          orgs = Orgs1,
          yubikeys = YubiKeys1,
          metadata = Metadata1
         },
      #?USER{
          uuid = UUID2,
          name = Name2,
          password = Password2,
          roles = Roles2,
          permissions = Permissions2,
          ssh_keys = Keys2,
          active_org = Org2,
          orgs = Orgs2,
          yubikeys = YubiKeys2,
          metadata = Metadata2
         }) ->
    #?USER{
        uuid = riak_dt_lwwreg:merge(UUID1, UUID2),
        name = riak_dt_lwwreg:merge(Name1, Name2),
        password = riak_dt_lwwreg:merge(Password1, Password2),
        active_org = riak_dt_lwwreg:merge(Org1, Org2),
        roles = riak_dt_orswot:merge(Roles1, Roles2),
        ssh_keys = riak_dt_orswot:merge(Keys1, Keys2),
        yubikeys = riak_dt_orswot:merge(YubiKeys1, YubiKeys2),
        permissions = riak_dt_orswot:merge(Permissions1, Permissions2),
        orgs = riak_dt_orswot:merge(Orgs1, Orgs2),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.

join_org({_T, ID}, Org, User) ->
    {ok, O1} = riak_dt_orswot:update({add, Org}, ID, User#?USER.orgs),
    User#?USER{orgs = O1}.

leave_org(TID={_T, ID}, Org, User) ->
    case riak_dt_orswot:update({remove, Org}, ID, User#?USER.orgs) of
        {error,{precondition,{not_present, Org}}} ->
            User;
        {ok, O1} ->
            User1 = User#?USER{orgs = O1},
            case active_org(User1) of
                _O when _O =:= Org ->
                    select_org(TID, <<>>, User1);
                _ ->
                    User1
            end
    end.

select_org({T, _ID}, Org, User) ->
    {ok, O1} = riak_dt_lwwreg:update({assign, Org, T}, none, User#?USER.active_org),
    User#?USER{active_org = O1}.

orgs(User) ->
    riak_dt_orswot:value(User#?USER.orgs).

active_org(User) ->
    riak_dt_lwwreg:value(User#?USER.active_org).

add_key({_T, ID}, KeyID, Key, User) ->
    {ok, S1} = riak_dt_orswot:update({add, {KeyID, Key}}, ID, User#?USER.ssh_keys),
    User#?USER{ssh_keys = S1}.

revoke_key({_T, ID}, KeyID, User) ->
    case lists:keyfind(KeyID, 1, keys(User)) of
        false ->
            User;
        Tpl ->
            case riak_dt_orswot:update({remove, Tpl}, ID, User#?USER.ssh_keys) of
                {error,{precondition,{not_present, Tpl}}} ->
                    User;
                {ok, S1} ->
                    User#?USER{ssh_keys = S1}
            end
    end.

keys(User) ->
    riak_dt_orswot:value(User#?USER.ssh_keys).

add_yubikey({_T, ID}, KeyID, User) ->
    {ok, S1} = riak_dt_orswot:update({add, KeyID}, ID, User#?USER.yubikeys),
    User#?USER{yubikeys = S1}.

remove_yubikey({_T, ID}, KeyID, User) ->
    case riak_dt_orswot:update({remove, KeyID}, ID, User#?USER.yubikeys) of
        {error,{precondition,{not_present, KeyID}}} ->
            User;
        {ok, S1} ->
            User#?USER{yubikeys = S1}
    end.

yubikeys(User) ->
    riak_dt_orswot:value(User#?USER.yubikeys).

name(User) ->
    riak_dt_lwwreg:value(User#?USER.name).

name({T, _ID}, Name, User) ->
    {ok, Name1} = riak_dt_lwwreg:update({assign, Name, T}, none, User#?USER.name),
    User#?USER{name = Name1}.

uuid(User) ->
    riak_dt_lwwreg:value(User#?USER.uuid).

uuid({T, _ID}, UUID, User) ->
    {ok, UUID1} = riak_dt_lwwreg:update({assign, UUID, T}, none, User#?USER.uuid),
    User#?USER{uuid = UUID1}.

password(User) ->
    riak_dt_lwwreg:value(User#?USER.password).

password({T, _ID}, Hash, User) ->
    {ok, PWD1} = riak_dt_lwwreg:update({assign, Hash, T}, none, User#?USER.password),
    User#?USER{password = PWD1}.

permissions(User) ->
    riak_dt_orswot:value(User#?USER.permissions).

grant({_T, ID}, P, User) ->
    {ok, P1} = riak_dt_orswot:update({add, P}, ID, User#?USER.permissions),
    User#?USER{permissions = P1}.


revoke({_T, ID}, P, User) ->
    case riak_dt_orswot:update({remove, P}, ID, User#?USER.permissions) of
        {error,{precondition,{not_present, P}}} ->
            User;
        {ok, P1} ->
            User#?USER{permissions = P1}
    end.

revoke_prefix({_T, ID}, Prefix, User) ->
    P0 = User#?USER.permissions,
    Ps = permissions(User),
    P1 = lists:foldl(fun (P, PAcc) ->
                             case lists:prefix(Prefix, P) of
                                 true ->
                                     {ok, R} = riak_dt_orswot:update(
                                                 {remove, P}, ID, PAcc),
                                     R;
                                 _ ->
                                     PAcc
                             end
                     end, P0, Ps),
    User#?USER{
            permissions = P1
           }.

roles(User) ->
    riak_dt_orswot:value(User#?USER.roles).


join({_T, ID}, Role, User) ->
    {ok, G} = riak_dt_orswot:update({add, Role}, ID, User#?USER.roles),
    User#?USER{roles = G}.

leave({_T, ID}, Role, User) ->
    case riak_dt_orswot:update({remove, Role}, ID, User#?USER.roles) of
        {error,{precondition,{not_present, Role}}} ->
            User;
        {ok, G} ->
            User#?USER{roles = G}
    end.

metadata(User) ->
    fifo_map:value(User#?USER.metadata).

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, User) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, User#?USER.metadata),
    User#?USER{metadata = M1};

set_metadata({T, ID}, Attribute, Value, User) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, User#?USER.metadata),
    User#?USER{metadata = M1}.

update_permissions(TID, U) ->
    Permissions = permissions(U),
    lists:foldl(fun ([<<"groups">> | R] = P, Acc) ->
                        Acc0 = revoke(TID, P, Acc),
                        grant(TID, [<<"roles">> | R], Acc0);
                    ([F, <<"groups">> | R] = P, Acc) ->
                        Acc0 = revoke(TID, P, Acc),
                        grant(TID, [F, <<"roles">> | R], Acc0);
                    ([F, S, <<"groups">> | R] = P, Acc) ->
                        Acc0 = revoke(TID, P, Acc),
                        grant(TID, [F, S, <<"roles">> | R], Acc0);
                    ([F, S, T, <<"groups">> | R] = P, Acc) ->
                        Acc0 = revoke(TID, P, Acc),
                        grant(TID, [F, S, T, <<"roles">> | R], Acc0);
                    (_, Acc) ->
                        Acc
                end, U, Permissions).
