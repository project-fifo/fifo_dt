-module(fifo_dt).

-export([type/1, js2req/1, req2js/1, update_set/1, update_map/1, to_ptree/1]).

-type tid() :: {pos_integer(), atom()}.
-export_type([tid/0]).

-spec type(any()) ->
                  ft_dataset | ft_dtrace | ft_grouping | ft_hypervisor |
                  ft_iprange | ft_network | ft_org | ft_package | ft_role |
                  ft_user | ft_vm.

type(O) ->
    Types = [
             ft_dataset,
             ft_dtrace,
             ft_grouping,
             ft_hypervisor,
             ft_iprange,
             ft_network,
             ft_org,
             ft_package,
             ft_role,
             ft_user,
             ft_vm
            ],
    type(Types, O).


type([M | R], O) ->
    case M:is_a(O) of
        true ->
            M;
        _ ->
            type(R, O)
    end;

type([], _) ->
    undefined.


js2req(C) ->
    case jsxd:get(<<"weight">>, <<"must">>, C) of
        <<"must">> ->
            make_rule(must, C);
        <<"cant">> ->
            make_rule(cant, C);
        <<"scale">> ->
            make_scale(C);
        <<"random">> ->
            make_random(C);
        I when is_integer(I) ->
            make_rule(I, C)
    end.

make_rule(Weight, C) ->
    Condition = case jsxd:get(<<"condition">>, C) of
                    {ok, <<">=">>} -> '>=';
                    {ok, <<">">>} -> '>';
                    {ok, <<"=<">>} -> '=<';
                    {ok, <<"<">>} -> '<';
                    {ok, <<"=:=">>} -> '=:=';
                    {ok, <<"=/=">>} -> '=/=';
                    {ok, <<"subset">>} -> 'subset';
                    {ok, <<"superset">>} -> 'superset';
                    {ok, <<"disjoint">>} -> 'disjoint';
                    {ok, <<"element">>} -> 'element';
                    {ok, <<"allowed">>} -> 'allowed'
                end,
    {ok, Attribute} = jsxd:get(<<"attribute">>, C),
    {ok, Value} = jsxd:get(<<"value">>, C),
    {Weight, Condition, Attribute, Value}.

req2js({random, Low, High}) ->
    [
     {<<"high">>, High},
     {<<"low">>, Low},
     {<<"weight">>, <<"random">>}
    ];
req2js({scale, Attribute, Low, High}) ->
    [
     {<<"attribute">>, Attribute},
     {<<"high">>, High},
     {<<"low">>, Low},
     {<<"weight">>, <<"scale">>}
    ];
req2js({Weight, Condition, Attribute, Value}) ->
    [
     {<<"attribute">>, Attribute},
     {<<"condition">>, v(Condition)},
     {<<"value">>, Value},
     {<<"weight">>, v(Weight)}
    ].

v(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
v(V) ->
    V.

make_scale(C) ->
    {ok, Attribute} = jsxd:get(<<"attribute">>, C),
    {ok, Low} = jsxd:get(<<"low">>, C),
    {ok, High} = jsxd:get(<<"high">>, C),
    {scale, Attribute, Low, High}.

make_random(C) ->
    {ok, Low} = jsxd:get(<<"low">>, C),
    {ok, High} = jsxd:get(<<"high">>, C),
    {random, Low, High}.

-spec update_set(old_set:orswot()) -> riak_dt_orswot:orswot().
update_set(Old) ->
    Values = old_set:value(Old),
    {ok, New} = riak_dt_orswot:update({add_all, Values}, update,
                                      riak_dt_orswot:new()),
    New.

-spec update_map(old_map:old_map()) -> riak_dt_map:riak_dt_map().
update_map(Old) ->
    V = fifo_old_map:value(Old),
    fifo_map:from_orddict(V, update, 1).

to_ptree(Perms) ->
    libsnarlmatch_tree:from_list(riak_dt_orswot:value(Perms)).

