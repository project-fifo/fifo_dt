%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_iprange).
-behaviour(fifo_dt).

-include("ft_iprange.hrl").
-include("ft_helper.hrl").

-define(G_SUB(N, F),
        sub_getter(O, N) ->
               F(O)).

-define(IS_IP, is_integer(V), V > 0, V < 16#FFFFFFFF).

-export([
         is_a/1, new/1,
         new/3, load/2, merge/2, to_json/1,
         release_ip/3, claim_ip/3,
         to_bin/1,
         parse_bin/1,
         cidr_to_mask/1,
         mask_to_cidr/1,
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

-ignore_xref([load/2, name/1, getter/2, uuid/1]).

-type iprange() :: #{
               type           => ?TYPE,
               version        => non_neg_integer(),
               uuid           => riak_dt_lwwreg:lwwreg(),
               name           => riak_dt_lwwreg:lwwreg(),

               network        => riak_dt_lwwreg:lwwreg(),
               netmask        => riak_dt_lwwreg:lwwreg(),
               gateway        => riak_dt_lwwreg:lwwreg(),
               tag            => riak_dt_lwwreg:lwwreg(),
               vlan           => riak_dt_lwwreg:lwwreg(),

               free           => riak_dt_orswot:orswot(),
               used           => riak_dt_orswot:orswot(),
               metadata       => riak_dt_map:riak_dt_map()
              }.
-export_type([iprange/0]).

?IS_A.

?REG_GET(uuid).
?REG_SET(uuid).

?REG_GET(name).
?REG_SET(name).

-define(REG_SET_IP(Field),
        Field({T, _ID}, V, O = #{type := ?TYPE, Field := Reg0})
        when ?IS_IP ->
               ?REG_SET_BODY(Field)).

?REG_GET(network).
?REG_SET_IP(network).

?REG_GET(netmask).
?REG_SET_IP(netmask).

?REG_GET(gateway).
?REG_SET_IP(gateway).

?REG_GET(tag).
?REG_SET(tag).

?REG_GET(vlan).
vlan({T, _ID}, V, O = #{type := ?TYPE, vlan := Reg0})
  when is_integer(V), V >= 0, V < 4096 ->
    ?REG_SET_BODY(vlan).

?SET_GET(free).
?SET_GET(used).

-spec getter(ft_obj:obj() | iprange(), jsxd:key()) -> jsxd:value().

getter(O = #{type := ?TYPE}, E) ->
    sub_getter(O, E);

getter(O, E) ->
    S0 = ft_obj:val(O),
    sub_getter(S0, E).

-spec sub_getter(iprange(), jsxd:key()) -> jsxd:value().
?G_SUB(<<"uuid">>, uuid);
?G_SUB(<<"name">>, name);
?G_SUB(<<"network">>, network);
?G_SUB(<<"netmask">>, netmask);
?G_SUB(<<"gateway">>, gateway);
?G_SUB(<<"tag">>, tag);
?G_SUB(<<"vlan">>, vlan);
sub_getter(O, K) ->
    lager:warning("[~s] Accessing unsupported getter ~p,"
                  " reverting to jsxd.", [?MODULE, K]),
    jsxd:get(K, to_json(O)).

-spec load(fifo_dt:tid(), term()) -> iprange().
load(_, #{type := Other}) when Other =/= ?TYPE ->
    error(bad_arg);

load(_, #{version := ?VERSION} = I) ->
    I;
load(TID, #iprange_0{
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
    I = #{
      type           => ?TYPE,
      version        => 1,
      uuid           => UUID,
      name           => Name,

      network        => Network,
      netmask        => Netmask,
      gateway        => Gateway,
      tag            => Tag,
      vlan           => VLan,

      free           => Free,
      used           => Used,
      metadata       => Metadata
     },
    load(TID, I);
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

new({_T, _ID}) ->
    #{
     type           => ?TYPE,
     version        => ?VERSION,
     uuid           => riak_dt_lwwreg:new(),
     name           => riak_dt_lwwreg:new(),

     network        => riak_dt_lwwreg:new(),
     netmask        => riak_dt_lwwreg:new(),
     gateway        => riak_dt_lwwreg:new(),
     tag            => riak_dt_lwwreg:new(),
     vlan           => riak_dt_lwwreg:new(),

     free           => riak_dt_orswot:new(),
     used           => riak_dt_orswot:new(),
     metadata       => riak_dt_map:new()
    }.

new(TID = {_T, ID}, S, E) when S < E ->
    {ok, Free} = riak_dt_orswot:update({add_all, lists:seq(S, E)}, ID,
                                       riak_dt_orswot:new()),
    I0 = new(TID),
    I0#{free => Free}.

claim_ip({_T, ID}, IP, I = #{type := ?TYPE, free := Free, used := Used}) when
      is_integer(IP) ->
    case riak_dt_orswot:update({remove, IP}, ID, Free) of
        {error, {precondition, {not_present, _}}} ->
            {error, used};
        {ok, Free1} ->
            {ok, Used1} = riak_dt_orswot:update({add, IP}, ID, Used),
            {ok, I#{free => Free1, used => Used1}}
    end.

release_ip({_T, ID}, IP, I = #{type := ?TYPE, free := Free, used := Used}) when
      is_integer(IP) ->
    case riak_dt_orswot:update({remove, IP}, ID, Used) of
        {error, {precondition, {not_present, _}}} ->
            {ok, I};
        {ok, Used1} ->
            {ok, Free1} = riak_dt_orswot:update({add, IP}, ID, Free),
            {ok, I#{free => Free1, used => Used1}}
    end.


?META.
?SET_META_3.
?SET_META_4.

to_json(I) ->
    #{
       <<"free">>     => free(I),
       <<"gateway">>  => gateway(I),
       <<"metadata">> => metadata(I),
       <<"name">>     => name(I),
       <<"netmask">>  => netmask(I),
       <<"network">>  => network(I),
       <<"tag">>      => tag(I),
       <<"used">>     => used(I),
       <<"uuid">>     => uuid(I),
       <<"vlan">>     => vlan(I)
     }.

merge(#{
         type     := ?TYPE,
         version  := ?VERSION,
         uuid     := UUID1,
         name     := Name1,

         network  := Network1,
         netmask  := Netmask1,
         gateway  := Gateway1,
         tag      := Tag1,
         vlan     := Vlan1,

         free     := Free1,
         used     := Used1,
         metadata := Metadata1
       } = I,
      #{
         type     := ?TYPE,
         version  := ?VERSION,
         uuid     := UUID2,
         name     := Name2,

         network  := Network2,
         netmask  := Netmask2,
         gateway  := Gateway2,
         tag      := Tag2,
         vlan     := Vlan2,

         free     := Free2,
         used     := Used2,
         metadata := Metadata2
       }) ->
    I#{
      uuid     => riak_dt_lwwreg:merge(UUID1, UUID2),
      name     => riak_dt_lwwreg:merge(Name1, Name2),

      network  => riak_dt_lwwreg:merge(Network1, Network2),
      netmask  => riak_dt_lwwreg:merge(Netmask1, Netmask2),
      gateway  => riak_dt_lwwreg:merge(Gateway1, Gateway2),
      tag      => riak_dt_lwwreg:merge(Tag1, Tag2),
      vlan     => riak_dt_lwwreg:merge(Vlan1, Vlan2),

      free     => riak_dt_orswot:merge(Free1, Free2),
      used     => riak_dt_orswot:merge(Used1, Used2),
      metadata => fifo_map:merge(Metadata1, Metadata2)
     }.

to_bin(IP) ->
    <<A, B, C, D>> = <<IP:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

parse_bin(Bin) ->
    [A, B, C, D] = [list_to_integer(binary_to_list(P))
                    || P <- re:split(Bin, "[.]")],
    <<IP:32>> = <<A:8, B:8, C:8, D:8>>,
    IP.

mask_to_cidr(<<"255.255.255.255">>) ->  32;
mask_to_cidr(<<"255.255.255.254">>) ->  31;
mask_to_cidr(<<"255.255.255.252">>) ->  30;
mask_to_cidr(<<"255.255.255.248">>) ->  29;
mask_to_cidr(<<"255.255.255.240">>) ->  28;
mask_to_cidr(<<"255.255.255.224">>) ->  27;
mask_to_cidr(<<"255.255.255.192">>) ->  26;
mask_to_cidr(<<"255.255.255.128">>) ->  25;
mask_to_cidr(<<"255.255.255.0">>) ->    24;
mask_to_cidr(<<"255.255.254.0">>) ->    23;
mask_to_cidr(<<"255.255.252.0">>) ->    22;
mask_to_cidr(<<"255.255.248.0">>) ->    21;
mask_to_cidr(<<"255.255.240.0">>) ->    20;
mask_to_cidr(<<"255.255.224.0">>) ->    19;
mask_to_cidr(<<"255.255.192.0">>) ->    18;
mask_to_cidr(<<"255.255.128.0">>) ->    17;
mask_to_cidr(<<"255.255.0.0">>) ->      16;
mask_to_cidr(<<"255.254.0.0">>) ->      15;
mask_to_cidr(<<"255.252.0.0">>) ->      14;
mask_to_cidr(<<"255.248.0.0">>) ->      13;
mask_to_cidr(<<"255.240.0.0">>) ->      12;
mask_to_cidr(<<"255.224.0.0">>) ->      11;
mask_to_cidr(<<"255.192.0.0">>) ->      10;
mask_to_cidr(<<"255.128.0.0">>) ->       9;
mask_to_cidr(<<"255.0.0.0">>) ->         8;
mask_to_cidr(<<"254.0.0.0">>) ->         7;
mask_to_cidr(<<"252.0.0.0">>) ->         6;
mask_to_cidr(<<"248.0.0.0">>) ->         5;
mask_to_cidr(<<"240.0.0.0">>) ->         4;
mask_to_cidr(<<"224.0.0.0">>) ->         3;
mask_to_cidr(<<"192.0.0.0">>) ->         2;
mask_to_cidr(<<"128.0.0.0">>) ->         1;
mask_to_cidr(<<"0.0.0.0">>) ->           0.

cidr_to_mask(32) -> <<"255.255.255.255">>;
cidr_to_mask(31) -> <<"255.255.255.254">>;
cidr_to_mask(30) -> <<"255.255.255.252">>;
cidr_to_mask(29) -> <<"255.255.255.248">>;
cidr_to_mask(28) -> <<"255.255.255.240">>;
cidr_to_mask(27) -> <<"255.255.255.224">>;
cidr_to_mask(26) -> <<"255.255.255.192">>;
cidr_to_mask(25) -> <<"255.255.255.128">>;
cidr_to_mask(24) -> <<"255.255.255.0">>;
cidr_to_mask(23) -> <<"255.255.254.0">>;
cidr_to_mask(22) -> <<"255.255.252.0">>;
cidr_to_mask(21) -> <<"255.255.248.0">>;
cidr_to_mask(20) -> <<"255.255.240.0">>;
cidr_to_mask(19) -> <<"255.255.224.0">>;
cidr_to_mask(18) -> <<"255.255.192.0">>;
cidr_to_mask(17) -> <<"255.255.128.0">>;
cidr_to_mask(16) -> <<"255.255.0.0">>;
cidr_to_mask(15) -> <<"255.254.0.0">>;
cidr_to_mask(14) -> <<"255.252.0.0">>;
cidr_to_mask(13) -> <<"255.248.0.0">>;
cidr_to_mask(12) -> <<"255.240.0.0">>;
cidr_to_mask(11) -> <<"255.224.0.0">>;
cidr_to_mask(10) -> <<"255.192.0.0">>;
cidr_to_mask( 9) -> <<"255.128.0.0">>;
cidr_to_mask( 8) -> <<"255.0.0.0">>;
cidr_to_mask( 7) -> <<"254.0.0.0">>;
cidr_to_mask( 6) -> <<"252.0.0.0">>;
cidr_to_mask( 5) -> <<"248.0.0.0">>;
cidr_to_mask( 4) -> <<"240.0.0.0">>;
cidr_to_mask( 3) -> <<"224.0.0.0">>;
cidr_to_mask( 2) -> <<"192.0.0.0">>;
cidr_to_mask( 1) -> <<"128.0.0.0">>;
cidr_to_mask( 0) -> <<"0.0.0.0">>.
