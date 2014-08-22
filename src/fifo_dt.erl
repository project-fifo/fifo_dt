-module(fifo_dt).

-export([type/1]).

type(O) ->
    Types = [
             {ft_dataset, dataset},
             {ft_dtrace, dtrace},
             {ft_grouping, grouping},
             {ft_hypervisor, hypervisor},
             {ft_iprange, iprange},
             {ft_network, network},
             {ft_org, org},
             {ft_package, package},
             {ft_role, role},
             {ft_user, user},
             {ft_vm, vm}

            ],
    type(Types, O).


type([{M, T} | R], O) ->
    case M:is_a(O) of
        true ->
            T;
        _ ->
            type(R, O)
    end;

type([], _) ->
    undefined.
