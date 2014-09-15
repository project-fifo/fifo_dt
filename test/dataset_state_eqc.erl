-module(dataset_state_eqc).

-ifdef(TEST).
-ifdef(EQC).

-import(ft_test_helper, [id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc_fsm.hrl").
-include_lib("fqc/include/fqc.hrl").

-compile(export_all).

-define(D, ft_dataset).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

dataset_type() ->
    oneof([kvm, zone]).
dataset() ->
    ?SIZED(Size, dataset(Size+1)).



dataset(Size) ->
    ?LAZY(oneof([{call, ?D, new, [id(Size)]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [dataset(Size - 1)],
                        oneof([
                               {call, ?D, load, [id(Size), O]},
                               %%{call, ?D, merge, [O, O]},

                               {call, ?D, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?D, status, [id(Size), non_blank_string(), O]},
                               {call, ?D, imported, [id(Size), int(), O]},
                               {call, ?D, type, [id(Size), dataset_type(), O]},

                               {call, ?D, description, [id(Size), non_blank_string(), O]},
                               {call, ?D, disk_driver, [id(Size), non_blank_string(), O]},
                               {call, ?D, homepage, [id(Size), non_blank_string(), O]},
                               {call, ?D, image_size, [id(Size), non_blank_string(), O]},
                               {call, ?D, name, [id(Size), non_blank_string(), O]},
                               {call, ?D, networks, [id(Size), list(non_blank_string()), O]},
                               {call, ?D, nic_driver, [id(Size), non_blank_string(), O]},
                               {call, ?D, os, [id(Size), non_blank_string(), O]},
                               {call, ?D, sha1, [id(Size), non_blank_string(), O]},
                               {call, ?D, users, [id(Size), list(non_blank_string()), O]},
                               {call, ?D, version, [id(Size), non_blank_string(), O]},

                               {call, ?D, add_requirement, [id(Size), non_blank_string(), O]},
                               {call, ?D, remove_requirement, [id(Size), maybe_oneof(calc_requirements(O)), O]},

                               {call, ?D, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?D, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]}

                              ]))
                     || Size > 1])).

calc_map(M, {call, _, M, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_map(M, U)));
calc_map(M, {call, _, M, [_, I, _K, U]}) ->
    [I | calc_map(M, U)];
calc_map(M, {call, _, _, P}) ->
    calc_map(M, lists:last(P));
calc_map(_M, _) ->
    [].

calc_requirements({call, _, remove_requirement, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_requirements(U)));
calc_requirements({call, _, add_requirements, [_, E, U]}) ->
    [E | calc_requirements(U)];
calc_requirements({call, _, _, P}) ->
    calc_requirements(lists:last(P));
calc_requirements(_) ->
    [].

r(K, V, U) ->
    lists:usort(lists:keystore(K, 1, U, {K, V})).

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_status(N, R) ->
    r(<<"status">>, N, R).

model_sha1(N, R) ->
    r(<<"sha1">>, N, R).

model_type(zone, R) ->
    r(<<"type">>, <<"zone">>, R);

model_type(kvm, R) ->
    r(<<"type">>, <<"kvm">>, R).

model_imported(N, R) ->
    r(<<"imported">>, N, R).

model_description(N, R) ->
    r(<<"description">>, N, R).

model_disk_driver(N, R) ->
    r(<<"disk_driver">>, N, R).

model_homepage(N, R) ->
    r(<<"homepage">>, N, R).

model_image_size(N, R) ->
    r(<<"image_size">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_networks(N, R) ->
    r(<<"networks">>, N, R).

model_nic_driver(N, R) ->
    r(<<"nic_driver">>, N, R).

model_os(N, R) ->
    r(<<"os">>, N, R).

model_users(N, R) ->
    r(<<"users">>, N, R).

model_version(N, R) ->
    r(<<"version">>, N, R).

model_add_requirement(E, U) ->
    r(<<"requirements">>, lists:usort([E | requirements(U)]), U).

model_remove_requirement(E, U) ->
    r(<<"requirements">>, lists:delete(E, requirements(U)), U).

model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).

model(R) ->
    ?D:to_json(R).

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

requirements(U) ->
    {<<"requirements">>, M} = lists:keyfind(<<"requirements">>, 1, U),
    M.


prop_merge() ->
    ?FORALL(R,
            dataset(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "[prop_merge]~nHistory: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            dataset(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "[prop_load]~nHistory: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_type() ->
    ?FORALL({N, R},
            {dataset_type(), dataset()},
            begin
                Hv = eval(R),
                Hv1 = ?D:type(id(?BIG_TIME), N, Hv),
                M1 = model(Hv1),
                M1x = model_type(N, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nHv1: ~p~n"
                                    "M1: ~p~nM1x: ~p~n",
                                    [R, Hv, Hv1, M1, M1x]),
                          M1 == M1x)
            end).

prop_status() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:status(id(?BIG_TIME), N, Hv)) ==
                              model_status(N, model(Hv)))
            end).

prop_sha1() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:sha1(id(?BIG_TIME), N, Hv)) ==
                              model_sha1(N, model(Hv)))
            end).

prop_imported() ->
    ?FORALL({N, R},
            {int(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:imported(id(?BIG_TIME), N, Hv)) ==
                              model_imported(N, model(Hv)))
            end).

prop_description() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:description(id(?BIG_TIME), N, Hv)) ==
                              model_description(N, model(Hv)))
            end).

prop_disk_driver() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:disk_driver(id(?BIG_TIME), N, Hv)) ==
                              model_disk_driver(N, model(Hv)))
            end).

prop_homepage() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:homepage(id(?BIG_TIME), N, Hv)) ==
                              model_homepage(N, model(Hv)))
            end).

prop_image_size() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:image_size(id(?BIG_TIME), N, Hv)) ==
                              model_image_size(N, model(Hv)))
            end).

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:name(id(?BIG_TIME), N, Hv)) ==
                              model_name(N, model(Hv)))
            end).

prop_networks() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:networks(id(?BIG_TIME), N, Hv)) ==
                              model_networks(N, model(Hv)))
            end).

prop_nic_driver() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:nic_driver(id(?BIG_TIME), N, Hv)) ==
                              model_nic_driver(N, model(Hv)))
            end).

prop_os() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:os(id(?BIG_TIME), N, Hv)) ==
                              model_os(N, model(Hv)))
            end).

prop_users() ->
    ?FORALL({N, O},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(O),
                O1 = ?D:users(id(?BIG_TIME), N, Hv),
                M1 = model_users(N, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_version() ->
    ?FORALL({N, R},
            {non_blank_string(), dataset()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:version(id(?BIG_TIME), N, Hv)) ==
                              model_version(N, model(Hv)))
            end).

prop_add_requirement() ->
    ?FORALL({E, O}, {non_blank_string(), dataset()},
            begin
                Hv = eval(O),
                O1 = ?D:add_requirement(id(?BIG_TIME), E, Hv),
                M1 = model_add_requirement(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_requirement() ->
    ?FORALL({O, K}, ?LET(O, dataset(), {O, maybe_oneof(calc_requirements(O))}),
            begin
                Hv = eval(O),
                O1 = ?D:remove_requirement(id(?BIG_TIME), K, Hv),
                M1 = model_remove_requirement(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), dataset()},
            begin
                Hv = eval(O),
                O1 = ?D:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, dataset(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?D:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

-endif.
-endif.
