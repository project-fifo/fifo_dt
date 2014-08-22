-module(fifo_dt).

-export([type/1]).

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
