%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(ft_iprange).

-include("ft_iprange.hrl").
-define(OBJ, ?IPRANGE).
-include("ft_helper.hrl").

-define(GSub(N, F),
        sub_getter(O, N) ->
               F(O)).

-define(IS_IP, is_integer(V), V > 0, V < 16#FFFFFFFF).

-export([
         is_a/1,
         new/3, load/2, merge/2, to_json/1,
         release_ip/3, claim_ip/3,
         set/4,
         to_bin/1,
         parse_bin/1,
         getter/2
        ]).

-export([
         name/1, name/3,
         uuid/1, uuid/3,
         network/1, network/3,
         netmask/1, netmask/3,
         gateway/1, gateway/3,
         metadata/1, set_metadata/3, set_metadata/4,
         tag/1, tag/3,
         vlan/1, vlan/3,
         free/1, used/1
        ]).

-ignore_xref([
              name/1, name/3,
              uuid/1, uuid/3,
              network/1, network/3,
              netmask/1, netmask/3,
              gateway/1, gateway/3,
              metadata/1, set_metadata/3, set_metadata/4,
              tag/1, tag/3,
              vlan/1, vlan/3,
              free/1, used/1,
              release_ip/3, claim_ip/3,
              merge/2
             ]).

-ignore_xref([load/2, name/1, set/4, getter/2, uuid/1]).

-type iprange() :: #?OBJ{}.
-export_type([iprange/0]).

?IS_A.

?G(uuid).
?S(uuid).

?G(name).
?S(name).

?G(network).
network({T, _ID}, V, H) when ?IS_IP ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?IPRANGE.network),
    H#?IPRANGE{network = V1}.

?G(netmask).
netmask({T, _ID}, V, H) when ?IS_IP ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?IPRANGE.netmask),
    H#?IPRANGE{netmask = V1}.

?G(gateway).
gateway({T, _ID}, V, H) when ?IS_IP ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?IPRANGE.gateway),
    H#?IPRANGE{gateway = V1}.

?G(tag).
tag({T, _ID}, V, H) when is_binary(V) ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?IPRANGE.tag),
    H#?IPRANGE{tag = V1}.

?G(vlan).
vlan({T, _ID}, V, H) when is_integer(V),
                          V >= 0, V < 4096 ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?IPRANGE.vlan),
    H#?IPRANGE{vlan = V1}.

free(H) ->
    riak_dt_orswot:value(H#?IPRANGE.free).

used(H) ->
    riak_dt_orswot:value(H#?IPRANGE.used).

-spec getter(ft_obj:obj() | iprange(), jsxd:key()) -> jsxd:value().

getter(#?OBJ{} = O, E) ->
    sub_getter(O, E);

getter(O, E) ->
    S0 = ft_obj:val(O),
    sub_getter(S0, E).

-spec sub_getter(iprange(), jsxd:key()) -> jsxd:value().
?GSub(<<"uuid">>, uuid);
?GSub(<<"name">>, name);
?GSub(<<"network">>, network);
?GSub(<<"netmask">>, netmask);
?GSub(<<"gateway">>, gateway);
?GSub(<<"tag">>, tag);
?GSub(<<"vlan">>, vlan);
sub_getter(O, K) ->
    lager:warning("[~s] Accessing unsupported getter ~p,"
                  " reverting to jsxd.", [?MODULE, K]),
    jsxd:get(K, to_json(O)).

load(_, #?IPRANGE{} = I) ->
    I;
load(TID, #iprange_0_1_0{
             uuid           = UUID,
             name           = Name,

             network        = Network,
             netmask        = Netmask,
             gateway        = Gateway,
             tag            = Tag,
             vlan           = VLan,

             free           = Free,
             used           = Used,
             metadata       = Metadata
            }) ->
    I = #iprange_0{
           uuid           = UUID,
           name           = Name,

           network        = Network,
           netmask        = Netmask,
           gateway        = Gateway,
           tag            = Tag,
           vlan           = VLan,

           free           = fifo_dt:update_set(Free),
           used           = fifo_dt:update_set(Used),
           metadata       = fifo_dt:update_map(Metadata)
          },
    load(TID, I).

new({_T, ID}, S, E) when S < E ->
    {ok, Free} = riak_dt_orswot:update({add_all, lists:seq(S, E)}, ID,
                                       riak_dt_orswot:new()),
    #?IPRANGE{free=Free}.

claim_ip({_T, ID}, IP, I) when
      is_integer(IP) ->
    case riak_dt_orswot:update({remove, IP}, ID, I#?IPRANGE.free) of
        {error,{precondition,{not_present,_}}} ->
            {error, used};
        {ok, Free} ->
            {ok, Used} = riak_dt_orswot:update({add, IP}, ID, I#?IPRANGE.used),
            {ok, I#?IPRANGE{free=Free, used=Used}}
    end.

release_ip({_T, ID}, IP, I) when
      is_integer(IP) ->
    case riak_dt_orswot:update({remove, IP}, ID, I#?IPRANGE.used) of
        {error,{precondition,{not_present,_}}} ->
            {ok, I};
        {ok, Used} ->
            {ok, Free} = riak_dt_orswot:update({add, IP}, ID, I#?IPRANGE.free),
            {ok, I#?IPRANGE{free=Free, used=Used}}
    end.

set({T, ID}, <<"metadata">>, V, H) ->
    H#?IPRANGE{metadata = fifo_map:from_orddict(V, ID, T)};

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);

set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H).

metadata(I) ->
    fifo_map:value(I#?IPRANGE.metadata).

set_metadata(ID, [{K, V} | R] , Obj) ->
    set_metadata(ID, R, set_metadata(ID, K, V, Obj));

set_metadata(_ID, _, Obj) ->
    Obj.

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, I) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, I#?IPRANGE.metadata),
    I#?IPRANGE{metadata = M1};

set_metadata({T, ID}, Attribute, Value, I) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, I#?IPRANGE.metadata),
    I#?IPRANGE{metadata = M1}.

to_json(I) ->
    [
     {<<"free">>, free(I)},
     {<<"gateway">>, gateway(I)},
     {<<"metadata">>, metadata(I)},
     {<<"name">>, name(I)},
     {<<"netmask">>, netmask(I)},
     {<<"network">>, network(I)},
     {<<"tag">>, tag(I)},
     {<<"used">>, used(I)},
     {<<"uuid">>, uuid(I)},
     {<<"vlan">>, vlan(I)}
    ].

merge(#?IPRANGE{
          uuid     = UUID1,
          name     = Name1,

          network  = Network1,
          netmask  = Netmask1,
          gateway  = Gateway1,
          tag      = Tag1,
          vlan     = Vlan1,

          free     = Free1,
          used     = Used1,
          metadata = Metadata1
         },
      #?IPRANGE{
          uuid     = UUID2,
          name     = Name2,

          network  = Network2,
          netmask  = Netmask2,
          gateway  = Gateway2,
          tag      = Tag2,
          vlan     = Vlan2,

          free     = Free2,
          used     = Used2,
          metadata = Metadata2
         }) ->
    #?IPRANGE{
        uuid     = riak_dt_lwwreg:merge(UUID1, UUID2),
        name     = riak_dt_lwwreg:merge(Name1, Name2),

        network  = riak_dt_lwwreg:merge(Network1, Network2),
        netmask  = riak_dt_lwwreg:merge(Netmask1, Netmask2),
        gateway  = riak_dt_lwwreg:merge(Gateway1, Gateway2),
        tag      = riak_dt_lwwreg:merge(Tag1, Tag2),
        vlan     = riak_dt_lwwreg:merge(Vlan1, Vlan2),

        free     = riak_dt_orswot:merge(Free1, Free2),
        used     = riak_dt_orswot:merge(Used1, Used2),
        metadata = fifo_map:merge(Metadata1, Metadata2)
       }.

to_bin(IP) ->
    <<A, B, C, D>> = <<IP:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

parse_bin(Bin) ->
    [A,B,C,D] = [ list_to_integer(binary_to_list(P)) || P <- re:split(Bin, "[.]")],
    <<IP:32>> = <<A:8, B:8, C:8, D:8>>,
    IP.
