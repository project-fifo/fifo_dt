-module(hypervisor_state_eqc).

-import(ft_test_helper, [id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).

-define(H, ft_hypervisor).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

hypervisor() ->
    ?SIZED(Size, hypervisor(Size)).

hypervisor(Size) ->
    ?LAZY(oneof([{call, ?H, new, [id(Size)]} || Size == 0] ++
                    [?LETSHRINK(
                        [O], [hypervisor(Size - 1)],
                        oneof([
                               {call, ?H, load, [id(Size), O]},
                               %% {call, ?H, merge, [O, O]},

                               {call, ?H, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?H, alias, [id(Size), non_blank_string(), O]},
                               {call, ?H, host, [id(Size), non_blank_string(), O]},
                               {call, ?H, port, [id(Size), int(), O]},
                               {call, ?H, version, [id(Size), non_blank_string(), O]},

                               {call, ?H, etherstubs, [id(Size), list(non_blank_string()), O]},
                               {call, ?H, networks, [id(Size), list(non_blank_string()), O]},
                               {call, ?H, path, [id(Size), path(), O]},
                               {call, ?H, virtualisation, [id(Size), list(non_blank_string()), O]},

                               {call, ?H, set_characteristic, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?H, set_characteristic, [id(Size), maybe_oneof(calc_map(set_characteristic, O)), delete, O]},
                               {call, ?H, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?H, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]},

                               {call, ?H, set_pool, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?H, set_pool, [id(Size), maybe_oneof(calc_map(set_pool, O)), delete, O]},

                               {call, ?H, set_resource, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?H, set_resource, [id(Size), maybe_oneof(calc_map(set_resource, O)), delete, O]},

                               {call, ?H, set_service, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?H, set_service, [id(Size), maybe_oneof(calc_map(set_service, O)), delete, O]}


                              ]))
                     || Size > 0])).

path_element() ->
    {non_blank_string(), int()}.

path() ->
    list(path_element()).

calc_map(M, {call, _, M, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_map(M, U)));
calc_map(M, {call, _, M, [_, I, _K, U]}) ->
    [I | calc_map(M, U)];
calc_map(M, {call, _, _, P}) ->
    calc_map(M, lists:last(P));
calc_map(_M, _) ->
    [].

r(K, V, U) ->
    lists:keystore(K, 1, U, {K, V}).

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_alias(N, R) ->
    r(<<"alias">>, N, R).

model_host(N, R) ->
    r(<<"host">>, N, R).

model_port(N, R) ->
    r(<<"port">>, N, R).

model_version(N, R) ->
    r(<<"version">>, N, R).

model_etherstubs(N, R) ->
    r(<<"etherstubs">>, N, R).

model_networks(N, R) ->
    r(<<"networks">>, N, R).

model_path(N, R) ->
    r(<<"path">>, [[{<<"cost">>, C}, {<<"name">>, Name}] || {Name, C} <- N], R).

model_virtualisation(N, R) ->
    r(<<"virtualisation">>, N, R).


model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).


model_set_pool(K, V, U) ->
    r(<<"pools">>, lists:usort(r(K, V, pools(U))), U).

model_delete_pool(K, U) ->
    r(<<"pools">>, lists:keydelete(K, 1, pools(U)), U).

model_set_characteristic(K, V, U) ->
    r(<<"characteristics">>, lists:usort(r(K, V, characteristics(U))), U).

model_delete_characteristic(K, U) ->
    r(<<"characteristics">>, lists:keydelete(K, 1, characteristics(U)), U).

model_set_resource(K, V, U) ->
    r(<<"resources">>, lists:usort(r(K, V, resources(U))), U).

model_delete_resource(K, U) ->
    r(<<"resources">>, lists:keydelete(K, 1, resources(U)), U).

model_set_service(K, V, U) ->
    r(<<"services">>, lists:usort(r(K, V, services(U))), U).

model_delete_service(K, U) ->
    r(<<"services">>, lists:keydelete(K, 1, services(U)), U).

model(R) ->
    ?H:to_json(R).

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

pools(U) ->
    {<<"pools">>, M} = lists:keyfind(<<"pools">>, 1, U),
    M.

characteristics(U) ->
    {<<"characteristics">>, M} = lists:keyfind(<<"characteristics">>, 1, U),
    M.

resources(U) ->
    {<<"resources">>, M} = lists:keyfind(<<"resources">>, 1, U),
    M.

services(U) ->
    {<<"services">>, M} = lists:keyfind(<<"services">>, 1, U),
    M.

prop_merge() ->
    ?FORALL(R,
            hypervisor(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            hypervisor(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).

prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_alias() ->
    ?FORALL({N, R},
            {non_blank_string(), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?H:alias(id(?BIG_TIME), N, Hv)) ==
                              model_alias(N, model(Hv)))
            end).
prop_host() ->
    ?FORALL({N, R},
            {non_blank_string(), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:host(id(?BIG_TIME), N, Hv)) ==
                              model_host(N, model(Hv)))
            end).

prop_port() ->
    ?FORALL({N, R},
            {int(), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:port(id(?BIG_TIME), N, Hv)) ==
                              model_port(N, model(Hv)))
            end).

prop_version() ->
    ?FORALL({N, R},
            {non_blank_string(), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:version(id(?BIG_TIME), N, Hv)) ==
                              model_version(N, model(Hv)))
            end).


prop_etherstubs() ->
    ?FORALL({N, R},
            {list(non_blank_string()), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:etherstubs(id(?BIG_TIME), N, Hv)) ==
                              model_etherstubs(N, model(Hv)))
            end).

prop_networks() ->
    ?FORALL({N, R},
            {list(non_blank_string()), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:networks(id(?BIG_TIME), N, Hv)) ==
                              model_networks(N, model(Hv)))
            end).

prop_path() ->
    ?FORALL({N, R},
            {path(), hypervisor()},
            begin
                Hv = eval(R),
                VAct = model(?H:path(id(?BIG_TIME), N, Hv)),
                VExp = model_path(N, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n~p /= ~p",
                                    [R, Hv, VAct, VExp]),
                          VAct == VExp)
            end).

prop_virtualisation() ->
    ?FORALL({N, R},
            {list(non_blank_string()), hypervisor()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?H:virtualisation(id(?BIG_TIME), N, Hv)) ==
                              model_virtualisation(N, model(Hv)))
            end).

prop_set_characteristic() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), hypervisor()},
            begin
                Hv = eval(O),
                O1 = ?H:set_characteristic(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_characteristic(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_characteristic() ->
    ?FORALL({O, K}, ?LET(O, hypervisor(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?H:set_characteristic(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_characteristic(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), hypervisor()},
            begin
                Hv = eval(O),
                O1 = ?H:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, hypervisor(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?H:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_pool() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), hypervisor()},
            begin
                Hv = eval(O),
                O1 = ?H:set_pool(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_pool(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_pool() ->
    ?FORALL({O, K}, ?LET(O, hypervisor(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?H:set_pool(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_pool(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_resource() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), hypervisor()},
            begin
                Hv = eval(O),
                O1 = ?H:set_resource(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_resource(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_resource() ->
    ?FORALL({O, K}, ?LET(O, hypervisor(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?H:set_resource(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_resource(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_service() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), hypervisor()},
            begin
                Hv = eval(O),
                O1 = ?H:set_service(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_service(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_service() ->
    ?FORALL({O, K}, ?LET(O, hypervisor(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?H:set_service(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_service(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, hypervisor(),
            jsx:encode(?H:to_json(eval(E))) /= []).
