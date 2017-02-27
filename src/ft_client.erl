%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_client).
-behaviour(fifo_dt).

-include("ft_client.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         name/1, name/3,
         client_id/1, client_id/3,
         secret/1, secret/3,
         type/1, type/3,
         uris/1, add_uri/3, remove_uri/3,
         ptree/1,
         permissions/1, grant/3, revoke/3,
         roles/1, join/3, leave/3,
         metadata/1, set_metadata/3, set_metadata/4,
         merge/2,
         to_json/1,
         is_a/1, getter/2
        ]).

-type client() ::
        #{
           type          => ?TYPE,
           version       => non_neg_integer(),
           uuid          => riak_dt_lwwreg:lwwreg(),
           name          => riak_dt_lwwreg:lwwreg(),
           client_id     => riak_dt_lwwreg:lwwreg(),
           secret        => riak_dt_lwwreg:lwwreg(),
           client_type   => riak_dt_lwwreg:lwwreg(),
           redirect_uris => riak_dt_orswot:orswot(),
           permissions   => riak_dt_orswot:orswot(),
           ptree         => libsnarlmatch_tree:new(),
           roles         => riak_dt_orswot:orswot(),
           metadata      => riak_dt_map:riak_dt_map()
         }.

-export_type([client/0]).

?IS_A.

new({_T, _ID}) ->
    {ok, Type} = ?NEW_LWW(public, 1),
    #{
       type          => ?TYPE,
       version       => ?VERSION,
       uuid          => riak_dt_lwwreg:new(),
       name          => riak_dt_lwwreg:new(),
       client_id     => riak_dt_lwwreg:new(),
       secret        => riak_dt_lwwreg:new(),
       client_type   => Type,
       redirect_uris => riak_dt_orswot:new(),
       permissions   => riak_dt_orswot:new(),
       ptree         => libsnarlmatch_tree:new(),
       roles         => riak_dt_orswot:new(),
       metadata      => riak_dt_map:new()
     }.

?G(<<"uuid">>, uuid);
?G_JSX.


load(_, #{type := ?TYPE, version := ?VERSION} = Client) ->
    Client;


load(TID, C = #{version := 0, type := {_, _} = T}) ->
    C1 = C#{
           version       => 1,
           type          => ?TYPE,
           client_type   => T
     },
    load(TID, C1);

%% If this was already correct we don't need to fix it.
load(TID, C = #{version := 0, type := client}) ->
    C1 = C#{
           version       => 1
          },
    load(TID, C1);


load(TID, #client_2{
             uuid          = UUID1,
             name          = Name1,
             client_id     = ClientID1,
             secret        = Secret1,
             type          = Type1,
             redirect_uris = RedirectURIs1,
             permissions   = Permissions1,
             ptree         = PTree,
             roles         = Roles1,
             metadata      = Metadata1
            }) ->
    C = #{
      version       => 0,
      type          => ?TYPE,
      uuid          => UUID1,
      name          => Name1,
      client_id     => ClientID1,
      secret        => Secret1,
      client_type   => Type1,
      redirect_uris => RedirectURIs1,
      permissions   => Permissions1,
      ptree         => PTree,
      roles         => Roles1,
      metadata      => Metadata1
     },
    load(TID, C);

load(TID, #client_1{
             uuid          = UUID1,
             name          = Name1,
             client_id     = ClientID1,
             secret        = Secret1,
             type          = Type1,
             redirect_uris = RedirectURIs1,
             permissions   = Permissions1,
             ptree         = PTree,
             roles         = Roles1,
             metadata      = Metadata1
            }) ->
    C = #client_2{
           uuid          = UUID1,
           name          = Name1,
           client_id     = ClientID1,
           secret        = Secret1,
           type          = Type1,
           redirect_uris = fifo_dt:update_set(RedirectURIs1),
           permissions   = fifo_dt:update_set(Permissions1),
           ptree         = PTree,
           roles         = fifo_dt:update_set(Roles1),
           metadata      = fifo_dt:update_map(Metadata1)
          },
    load(TID, C);

load(TID, #client_0{
             uuid          = UUID1,
             name          = Name1,
             client_id     = ClientID1,
             secret        = Secret1,
             type          = Type1,
             redirect_uris = RedirectURIs1,
             permissions   = Permissions1,
             roles         = Roles1,
             metadata      = Metadata1
            }) ->
    C = #client_1{
           uuid          = UUID1,
           name          = Name1,
           client_id     = ClientID1,
           secret        = Secret1,
           type          = Type1,
           redirect_uris = RedirectURIs1,
           permissions   = Permissions1,
           ptree         = fifo_dt:to_ptree(fifo_dt:update_set(Permissions1)),
           roles         = Roles1,
           metadata      = Metadata1
          },
    load(TID, C).



to_json(C) ->
    jsxd:from_list(
      [
       {<<"uuid">>, uuid(C)},
       {<<"name">>, name(C)},
       {<<"client_id">>, client_id(C)},
       {<<"type">>, type_to_json(type(C))},
       {<<"redirect_uris">>, uris(C)},
       {<<"permissions">>, permissions(C)},
       {<<"roles">>, roles(C)},
       {<<"metadata">>, metadata(C)}
      ]).


type_to_json(confidential) ->
    <<"confidential">>;
type_to_json(public) ->
    <<"public">>.


merge(C = #{
        uuid          := UUID1,
        name          := Name1,
        client_id     := ClientID1,
        secret        := Secret1,
        client_type   := Type1,
        redirect_uris := RedirectURIs1,
        permissions   := Permissions1,
        roles         := Roles1,
        metadata      := Metadata1
       },
      #{
         uuid          := UUID2,
         name          := Name2,
         client_id     := ClientID2,
         secret        := Secret2,
         client_type   := Type2,
         redirect_uris := RedirectURIs2,
         permissions   := Permissions2,
         roles         := Roles2,
         metadata      := Metadata2
       }) ->
    P1 = riak_dt_orswot:merge(Permissions1, Permissions2),
    C#{
      uuid          => riak_dt_lwwreg:merge(UUID1, UUID2),
      name          => riak_dt_lwwreg:merge(Name1, Name2),
      client_id     => riak_dt_lwwreg:merge(ClientID1, ClientID2),
      secret        => riak_dt_lwwreg:merge(Secret1, Secret2),
      client_type   => riak_dt_lwwreg:merge(Type1, Type2),
      redirect_uris => riak_dt_orswot:merge(RedirectURIs1, RedirectURIs2),
      permissions   => P1,
      ptree         => fifo_dt:to_ptree(P1),
      roles         => riak_dt_orswot:merge(Roles1, Roles2),
      metadata      => fifo_map:merge(Metadata1, Metadata2)
     }.

?REG_GET(uuid).
?REG_SET(uuid).
?REG_GET(name).
?REG_SET(name).
?REG_GET(client_id).
?REG_SET(client_id).
?REG_GET(secret).
?REG_SET(secret).
?REG_GET(type, client_type).
?REG_SET(type, client_type).

?SET_GET(uris, redirect_uris).
?SET_ADD(add_uri, redirect_uris).
?SET_REM(remove_uri, redirect_uris).

ptree(#{type := ?TYPE, ptree := PTree}) ->
    PTree.

?SET_GET(permissions).

grant({_T, ID}, P, Client = #{type := ?TYPE, permissions := Ps0}) ->
    {ok, Ps1} = riak_dt_orswot:update({add, P}, ID, Ps0),
    Client#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}.


revoke({_T, ID}, P, Client = #{type := ?TYPE, permissions := Ps0}) ->
    case riak_dt_orswot:update({remove, P}, ID, Ps0) of
        {error, {precondition, {not_present, P}}} ->
            Client;
        {ok, Ps1} ->
            Client#{permissions := Ps1, ptree := fifo_dt:to_ptree(Ps1)}
    end.

?SET_GET(roles).
?SET_ADD(join, roles).
?SET_REM(leave, roles).

?META.
?SET_META_3.
?SET_META_4.
