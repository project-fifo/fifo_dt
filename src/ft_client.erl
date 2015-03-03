%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_client).

-include("ft_client.hrl").
-define(OBJ, ?CLIENT).
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

-type client() :: #?CLIENT{}.
-export_type([client/0]).

?IS_A.

new({_T, _ID}) ->
    {ok, Type} = ?NEW_LWW(public, 1),
    #?CLIENT{
        type = Type
       }.

?G(<<"uuid">>, uuid);
?G_JSX.


load(_, #?CLIENT{} = Client) ->
    Client;
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
           ptree         = to_ptree(Permissions1),
           roles         = Roles1,
           metadata      = Metadata1
          },
    load(TID, C).



to_json(#?CLIENT{} = C) ->
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


merge(#?CLIENT{
          uuid          = UUID1,
          name          = Name1,
          client_id     = ClientID1,
          secret        = Secret1,
          type          = Type1,
          redirect_uris = RedirectURIs1,
          permissions   = Permissions1,
          roles         = Roles1,
          metadata      = Metadata1
         },
      #?CLIENT{
          uuid          = UUID2,
          name          = Name2,
          client_id     = ClientID2,
          secret        = Secret2,
          type          = Type2,
          redirect_uris = RedirectURIs2,
          permissions   = Permissions2,
          roles         = Roles2,
          metadata      = Metadata2
         }) ->
    P1 = riak_dt_orswot:merge(Permissions1, Permissions2),
    #?CLIENT{
        uuid          = riak_dt_lwwreg:merge(UUID1, UUID2),
        name          = riak_dt_lwwreg:merge(Name1, Name2),
        client_id     = riak_dt_lwwreg:merge(ClientID1, ClientID2),
        secret        = riak_dt_lwwreg:merge(Secret1, Secret2),
        type          = riak_dt_lwwreg:merge(Type1, Type2),
        redirect_uris = riak_dt_orswot:merge(RedirectURIs1, RedirectURIs2),
        permissions   = P1,
        ptree         = to_ptree(P1),
        roles         = riak_dt_orswot:merge(Roles1, Roles2),
        metadata      = fifo_map:merge(Metadata1, Metadata2)
       }.

?G(uuid).
?S(uuid).
?G(name).
?S(name).
?G(client_id).
?S(client_id).
?G(secret).
?S(secret).
?G(type).
?S(type).

uris(Client) ->
    riak_dt_orswot:value(Client#?CLIENT.redirect_uris).

add_uri({_T, ID}, Uri, Client) ->
    {ok, O1} = riak_dt_orswot:update({add, Uri}, ID, Client#?CLIENT.redirect_uris),
    Client#?CLIENT{redirect_uris = O1}.

remove_uri({_T, ID}, Uri, Client) ->
    case riak_dt_orswot:update({remove, Uri}, ID, Client#?CLIENT.redirect_uris) of
        {error,{precondition,{not_present, Uri}}} ->
            Client;
        {ok, O1} ->
            Client#?CLIENT{redirect_uris = O1}
    end.

ptree(#?CLIENT{ptree = PTree}) ->
    PTree.

permissions(Client) ->
    riak_dt_orswot:value(Client#?CLIENT.permissions).

grant({_T, ID}, P, Client) ->
    {ok, P1} = riak_dt_orswot:update({add, P}, ID, Client#?CLIENT.permissions),
    Client#?CLIENT{permissions = P1, ptree = to_ptree(P1)}.


revoke({_T, ID}, P, Client) ->
    case riak_dt_orswot:update({remove, P}, ID, Client#?CLIENT.permissions) of
        {error,{precondition,{not_present, P}}} ->
            Client;
        {ok, P1} ->
            Client#?CLIENT{permissions = P1, ptree = to_ptree(P1)}
    end.

roles(Client) ->
    riak_dt_orswot:value(Client#?CLIENT.roles).


join({_T, ID}, Role, Client) ->
    {ok, G} = riak_dt_orswot:update({add, Role}, ID, Client#?CLIENT.roles),
    Client#?CLIENT{roles = G}.

leave({_T, ID}, Role, Client) ->
    case riak_dt_orswot:update({remove, Role}, ID, Client#?CLIENT.roles) of
        {error,{precondition,{not_present, Role}}} ->
            Client;
        {ok, G} ->
            Client#?CLIENT{roles = G}
    end.


metadata(Client) ->
    fifo_map:value(Client#?CLIENT.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, Client) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, Client);

set_metadata({_T, ID}, Attribute, delete, Client) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, Client#?CLIENT.metadata),
    Client#?CLIENT{metadata = M1};

set_metadata({T, ID}, Attribute, Value, Client) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Client#?CLIENT.metadata),
    Client#?CLIENT{metadata = M1}.

to_ptree(Perms) ->
    libsnarlmatch_tree:from_list(riak_dt_orswot:value(Perms)).
