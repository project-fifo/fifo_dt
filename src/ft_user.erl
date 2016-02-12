%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_user).
-behaviour(fifo_dt).

-include("ft_user.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         password/1, password/3,
         permissions/1, grant/3, revoke/3, revoke_prefix/3,
         ptree/1,
         roles/1, join/3, leave/3,
         join_org/3, leave_org/3, select_org/3, orgs/1, active_org/1,
         add_key/4, revoke_key/3, keys/1,
         metadata/1, set_metadata/3, set_metadata/4,
         add_yubikey/3, yubikeys/1, remove_yubikey/3,
         tokens/1, add_token/3, remove_token/3, get_token/2, get_token_by_id/2,
         merge/2,
         to_json/1,
         is_a/1, getter/2
        ]).

-ignore_xref([
              load/1,
              uuid/1, uuid/3,
              name/1, name/3,
              password/1, password/3,
              permissions/1, grant/3, revoke/3, revoke_prefix/3,
              roles/1, join/3, leave/3,
              add_key/4, revoke_key/3, keys/1,
              metadata/1, set_metadata/3, set_metadata/4,
              join_org/3, leave_org/3, select_org/3, orgs/1, active_org/1,
              add_yubikey/3, yubikeys/1, remove_yubikey/3,
              tokens/1, add_token/3,
              remove_token/3, get_token/2,
              remove_token_by_id/3, get_token_by_id/2,
              merge/2, getter/2,
              to_json/1
             ]).


-type password() :: {binary() | bcrypt, binary()}.

-type user() ::
        #{
           type        => ?TYPE,
           version     => non_neg_integer(),
           uuid        => riak_dt_lwwreg:lwwreg(),
           name        => riak_dt_lwwreg:lwwreg(),
           password    => riak_dt_lwwreg:lwwreg(),
           active_org  => riak_dt_lwwreg:lwwreg(),
           permissions => riak_dt_orswot:orswot(),
           ptree       => libsnarlmatch_tree:tree(),
           roles       => riak_dt_orswot:orswot(),
           ssh_keys    => riak_dt_orswot:orswot(),
           orgs        => riak_dt_orswot:orswot(),
           yubikeys    => riak_dt_orswot:orswot(),
           tokens      => riak_dt_orswot:orswot(),
           metadata    => riak_dt_map:riak_dt_map()
         }.

-export_type([user/0, token/0]).

-type token() :: #{
             id => binary(),
             type => access|refresh,
             token => binary(),
             expiery => pos_integer() | infinity,
             client => binary() | undefined,
             scope => [binary()],
             comment => binary() | undefined
            }.

-spec is_a(any()) -> true | false.
?IS_A.

?G(<<"uuid">>, uuid);
?G_JSX.

-spec new({pos_integer(), atom()}) -> user().

new({_T, _ID}) ->
    #{
     type        => ?TYPE,
     version     => ?VERSION,
     uuid        => riak_dt_lwwreg:new(),
     name        => riak_dt_lwwreg:new(),
     password    => riak_dt_lwwreg:new(),
     active_org  => riak_dt_lwwreg:new(),
     permissions => riak_dt_orswot:new(),
     ptree       => libsnarlmatch_tree:new(),
     roles       => riak_dt_orswot:new(),
     ssh_keys    => riak_dt_orswot:new(),
     orgs        => riak_dt_orswot:new(),
     yubikeys    => riak_dt_orswot:new(),
     tokens      => riak_dt_orswot:new(),
     metadata    => riak_dt_map:new()
    }.

-spec load(fifo_dt:tid(), term()) -> user().

load(_, #{type := Other}) when Other =/= ?TYPE ->
    error(bad_arg);

load(TID, User) ->
    clean_tokens(TID, load_(TID, User)).

-spec clean_tokens(fifo_dt:tid(), user()) -> user().

clean_tokens({_T, ID}, User = #{tokens := Ts}) ->
    Tvs = riak_dt_orswot:value(Ts),
    T0 = erlang:system_time(seconds),
    Expired = [T || T = #{expiery := E} <- Tvs,
                    E =< T0],
    {ok, Ts1} = riak_dt_orswot:update({remove_all, Expired}, ID, Ts),
    User#{tokens => Ts1}.

-spec load_(fifo_dt:tid(), term()) -> user().

load_(_, #{type := ?TYPE, version := ?VERSION} = User) ->
    User;

load_(TID, #user_2{
              uuid = UUID,
              name = Name,
              password = Passwd,
              active_org = ActiveOrg,
              permissions = Permissions,
              roles = Roles,
              ssh_keys = Keys,
              orgs = Orgs,
              yubikeys = YubiKeys,
              tokens = Tokens,
              metadata = Metadata
             }) ->
    load(TID,
         #{
           type        => ?TYPE,
           version     => 0,
           uuid        => UUID,
           name        => Name,
           password    => Passwd,
           active_org  => ActiveOrg,
           permissions => Permissions,
           ptree       => fifo_dt:to_ptree(Permissions),
           roles       => Roles,
           ssh_keys    => Keys,
           orgs        => Orgs,
           yubikeys    => YubiKeys,
           tokens      => Tokens,
           metadata    => Metadata
          });

load_(TID, #user_1{
              uuid = UUID,
              name = Name,
              password = Passwd,
              active_org = ActiveOrg,
              permissions = Permissions,
              ptree = PTree,
              roles = Roles,
              ssh_keys = Keys,
              orgs = Orgs,
              yubikeys = YubiKeys,
              metadata = Metadata
             }) ->
    U = #user_2{
           uuid = UUID,
           name = Name,
           password = Passwd,
           active_org = ActiveOrg,
           permissions = Permissions,
           ptree = PTree,
           roles = Roles,
           ssh_keys = Keys,
           orgs = Orgs,
           yubikeys = YubiKeys,
           tokens = riak_dt_orswot:new(),
           metadata = Metadata
          },
    load_(TID, U);

load_(TID, #user_0{
              uuid = UUID,
              name = Name,
              password = Passwd,
              active_org = ActiveOrg,
              permissions = Permissions,
              ptree = PTree,
              roles = Roles,
              ssh_keys = Keys,
              orgs = Orgs,
              yubikeys = YubiKeys,
              metadata = Metadata
             }) ->
    U = #user_1{
           uuid = UUID,
           name = Name,
           password = Passwd,
           active_org = ActiveOrg,
           permissions = fifo_dt:update_set(Permissions),
           ptree = PTree,
           roles = fifo_dt:update_set(Roles),
           ssh_keys = fifo_dt:update_set(Keys),
           orgs = fifo_dt:update_set(Orgs),
           yubikeys = fifo_dt:update_set(YubiKeys),
           metadata = fifo_dt:update_map(Metadata)
          },
    load_(TID, U);

load_(TID, #user_0_1_5{
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
    U = #user_0{
           uuid = UUID,
           name = Name,
           password = Passwd,
           active_org = ActiveOrg,
           permissions = Permissions,
           ptree = fifo_dt:to_ptree(fifo_dt:update_set(Permissions)),
           roles = Roles,
           ssh_keys = Keys,
           orgs = Orgs,
           yubikeys = YubiKeys,
           metadata = Metadata
          },
    load_(TID, U).

-spec to_json(user()) -> [{binary(), term()}].

to_json(#{
            uuid        := UUID,
            name        := Name,
            roles       := Roles,
            ssh_keys    := Keys,
            permissions := Permissions,
            active_org  := Org,
            orgs        := Orgs,
            yubikeys    := YubiKeys,
            tokens      := Tokens,
            metadata    := Metadata
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
       {<<"tokens">>, lists:usort([token_to_json(T)
                                   || T <- riak_dt_orswot:value(Tokens)])},
       {<<"metadata">>, fifo_map:value(Metadata)}
      ]).

-spec merge(user(), user()) -> user().
merge(U = #{
        type := ?TYPE,
        uuid := UUID1,
        name := Name1,
        password := Password1,
        roles := Roles1,
        permissions := Permissions1,
        ssh_keys := Keys1,
        active_org := Org1,
        orgs := Orgs1,
        tokens := Tokens1,
        yubikeys := YubiKeys1,
        metadata := Metadata1
       },
      #{
         type := ?TYPE,
         uuid := UUID2,
         name := Name2,
         password := Password2,
         roles := Roles2,
         permissions := Permissions2,
         ssh_keys := Keys2,
         active_org := Org2,
         orgs := Orgs2,
         tokens := Tokens2,
         yubikeys := YubiKeys2,
         metadata := Metadata2
       }) ->
    Permissions = riak_dt_orswot:merge(Permissions1, Permissions2),
    U#{
      uuid := riak_dt_lwwreg:merge(UUID1, UUID2),
      name := riak_dt_lwwreg:merge(Name1, Name2),
      password := riak_dt_lwwreg:merge(Password1, Password2),
      active_org := riak_dt_lwwreg:merge(Org1, Org2),
      roles := riak_dt_orswot:merge(Roles1, Roles2),
      ssh_keys := riak_dt_orswot:merge(Keys1, Keys2),
      yubikeys := riak_dt_orswot:merge(YubiKeys1, YubiKeys2),
      permissions := Permissions,
      ptree := fifo_dt:to_ptree(Permissions),
      orgs := riak_dt_orswot:merge(Orgs1, Orgs2),
      tokens := riak_dt_orswot:merge(Tokens1, Tokens2),
      metadata := fifo_map:merge(Metadata1, Metadata2)
     }.

-spec token_to_json(token()) -> [{binary(), term()}].
token_to_json(#{type := Type, id := ID, expiery := Exp, client := Client,
                scope := Scope, comment := Comment}) ->
    J = [{<<"type">>, atom_to_binary(Type, utf8)},
         {<<"id">>, ID},
         {<<"scope">>, Scope}],
    J2 = case Exp of
             infinity ->
                 J;
             _ ->
                 [{<<"expiery">>, Exp} | J]
         end,
    J3 = case Client of
             undefined ->
                 J2;
             _ ->
                 [{<<"client">>, Client} | J2]
         end,
    J4 = case Comment of
             undefined ->
                 J3;
             _ ->
                 [{<<"comment">>, Comment} | J3]
         end,
    lists:sort(J4).

-spec add_token(fifo_dt:tid(),
                {TokenID :: binary(),
                 Type :: access | refresh,
                 Token :: binary(),
                 Expiery :: pos_integer() | infinity,
                 Client :: binary() | undefined,
                 Scope :: [binary()] | undefined,
                 Comment :: binary() | undefined},
                user()) ->
                       user().

add_token({_T, ID}, {TokenID, Type, Token, Expiery, Client, Scope, Comment},
          User = #{tokens := Tokens})
  when is_binary(TokenID),
       (Type =:= access orelse Type =:= refresh),
       is_binary(Token),
       ((is_integer(Expiery) andalso Expiery) > 0 orelse Expiery =:= infinity),
       (is_binary(Client) orelse Client =:= undefined),
       (is_binary(Comment) orelse Comment =:= undefined),
       is_list(Scope) ->
    T = #{id => TokenID,
          type => Type,
          token => Token,
          expiery => Expiery,
          client => Client,
          scope => Scope,
          comment => Comment},
    {ok, Ts1} = riak_dt_orswot:update({add, T}, ID, Tokens),
    User#{tokens := Ts1}.

-spec remove_token(fifo_dt:tid(), binary(), user()) -> user().
remove_token({_T, ID}, Token, User = #{tokens := Tokens}) ->
    case get_token(Token, User) of
        not_found ->
            User;
        T ->
            {ok, Ts} = riak_dt_orswot:update({remove, T}, ID, Tokens),
            User#{tokens := Ts}
    end.

-spec get_token(binary(), user()) -> token() | not_found.
get_token(Token, User) ->
    Ts = tokens(User),
    case [T || T = #{token := T1} <-  Ts, T1 =:= Token] of
        [] ->
            not_found;
        [T] ->
            T
    end.

-spec get_token_by_id(binary(), user()) -> token() | not_found.
get_token_by_id(TokenID, User) ->
    Ts = tokens(User),
    case [T || T = #{id := T1} <-  Ts, T1 =:= TokenID] of
        [] ->
            not_found;
        [T] ->
            T
    end.

-spec tokens(user()) -> [token()].
?SET_GET(tokens).

-spec orgs(user()) -> [fifo:org_id()].
?SET_GET(orgs).
-spec join_org(fifo_dt:tid(), fifo:org_id(), user()) -> user().
?SET_ADD(join_org, orgs).
-spec leave_org(fifo_dt:tid(), fifo:org_id(), user()) -> user().
leave_org(TID={_T, ID}, Org, User = #{orgs := Orgs}) ->
    case riak_dt_orswot:update({remove, Org}, ID, Orgs) of
        {error, {precondition, {not_present, Org}}} ->
            User;
        {ok, O1} ->
            User1 = User#{orgs := O1},
            case active_org(User1) of
                _O when _O =:= Org ->
                    select_org(TID, <<>>, User1);
                _ ->
                    User1
            end
    end.

-spec select_org(fifo_dt:tid(), fifo:org_id(), user()) -> user().
select_org({T, _ID}, Org, User = #{active_org := Active}) ->
    {ok, O1} = riak_dt_lwwreg:update({assign, Org, T}, none, Active),
    User#{active_org := O1}.

-spec active_org(user()) -> fifo:org_id().
?REG_GET(active_org).

-spec keys(user()) -> [{binary(), binary()}].
?SET_GET(keys, ssh_keys).

-spec add_key(fifo_dt:tid(), binary(), binary(), user()) -> user().
add_key({_T, ID}, KeyID, Key, User = #{ssh_keys := Keys}) ->
    {ok, S1} = riak_dt_orswot:update({add, {KeyID, Key}}, ID, Keys),
    User#{ssh_keys := S1}.

-spec revoke_key(fifo_dt:tid(), binary(), user()) -> user().
revoke_key({_T, ID}, KeyID, User = #{ssh_keys := Keys}) ->
    case lists:keyfind(KeyID, 1, keys(User)) of
        false ->
            User;
        Tpl ->
            case riak_dt_orswot:update({remove, Tpl}, ID, Keys) of
                {error, {precondition, {not_present, Tpl}}} ->
                    User;
                {ok, S1} ->
                    User#{ssh_keys := S1}
            end
    end.

-spec yubikeys(user()) -> [binary()].
?SET_GET(yubikeys).

-spec add_yubikey(fifo_dt:tid(), binary(), user()) -> user().
?SET_ADD(add_yubikey, yubikeys).

-spec remove_yubikey(fifo_dt:tid(), binary(), user()) -> user().
?SET_REM(remove_yubikey, yubikeys).

-spec name(user()) -> binary().
?REG_GET(name).

-spec name(fifo_dt:tid(), binary(), user()) -> user().
?REG_SET(name).

-spec uuid(user()) -> binary().
?REG_GET(uuid).
-spec uuid(fifo_dt:tid(), binary(), user()) -> user().
?REG_SET(uuid).
-spec password(user()) -> password() | <<>>.
?REG_GET(password).
-spec password(fifo_dt:tid(), password(), user()) -> user().
?REG_SET(password).

ptree(#{type := ?TYPE, ptree := PTree}) ->
    PTree.

?SET_GET(permissions).

grant({_T, ID}, P, User = #{type := ?TYPE, permissions := Ps0}) ->
    {ok, Ps1} = riak_dt_orswot:update({add, P}, ID, Ps0),
    User#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}.


revoke({_T, ID}, P, User = #{type := ?TYPE, permissions := Ps0}) ->
    case riak_dt_orswot:update({remove, P}, ID, Ps0) of
        {error, {precondition, {not_present, P}}} ->
            User;
        {ok, Ps1} ->
            User#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}
    end.

revoke_prefix({_T, ID}, Prefix, User = #{type := ?TYPE, permissions := Ps0}) ->
    PVs = permissions(User),
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
    User#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}.

?SET_GET(roles).
?SET_ADD(join, roles).
?SET_REM(leave, roles).

?META.
?SET_META_3.
?SET_META_4.
