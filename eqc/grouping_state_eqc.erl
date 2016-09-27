-module(grouping_state_eqc).

-import(ft_test_helper, [model_set_metadata/3, model_delete_metadata/2,
                         metadata/1, r/3, id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).

-define(G, ft_grouping).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

grouping() ->
    ?SIZED(Size, grouping(Size+1)).

grouping(Size) ->
    ?LAZY(oneof([{call, ?G, new, [id(Size)]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [grouping(Size - 1)],
                        oneof([
                               {call, ?G, load, [id(Size), O]},
                               %%{call, ?G, merge, [O, O]},

                               {call, ?G, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?G, name, [id(Size), non_blank_string(), O]},
                               {call, ?G, type, [id(Size), type(), O]},

                               {call, ?G, add_element, [id(Size), non_blank_string(), O]},
                               {call, ?G, remove_element, [id(Size), maybe_oneof(calc_elements(O)), O]},

                               {call, ?G, add_grouping, [id(Size), non_blank_string(), O]},
                               {call, ?G, remove_grouping, [id(Size), maybe_oneof(calc_groupings(O)), O]},

                               {call, ?G, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?G, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]},

                               {call, ?G, set_config, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?G, set_config, [id(Size), maybe_oneof(calc_map(set_config, O)), delete, O]}

                              ]))
                     || Size > 1])).

type() ->
    oneof([none, cluster, stack]).

calc_map(M, {call, _, M, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_map(M, U)));
calc_map(M, {call, _, M, [_, I, _K, U]}) ->
    [I | calc_map(M, U)];
calc_map(M, {call, _, _, P}) ->
    calc_map(M, lists:last(P));
calc_map(_M, _) ->
    [].


calc_elements({call, _, remove_element, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_elements(U)));
calc_elements({call, _, add_element, [_, E, U]}) ->
    [E | calc_elements(U)];
calc_elements({call, _, _, P}) ->
    calc_elements(lists:last(P));
calc_elements(_) ->
    [].

calc_groupings({call, _, remove_grouping, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_groupings(U)));
calc_groupings({call, _, add_grouping, [_, E, U]}) ->
    [E | calc_groupings(U)];
calc_groupings({call, _, _, P}) ->
    calc_groupings(lists:last(P));
calc_groupings(_) ->
    [].


model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_type(N, R) ->
    r(<<"type">>, list_to_binary(atom_to_list(N)), R).

model_set_config(K, V, U) ->
    r(<<"config">>, r(K, V, config(U)), U).

model_delete_config(K, U) ->
    r(<<"config">>, maps:remove(K, config(U)), U).

model_add_element(E, U) ->
    r(<<"elements">>, lists:usort([E | get_elements(U)]), U).

model_remove_element(E, U) ->
    r(<<"elements">>, lists:delete(E, get_elements(U)), U).

model_add_grouping(E, U) ->
    r(<<"groupings">>, lists:usort([E | get_groupings(U)]), U).

model_remove_grouping(E, U) ->
    r(<<"groupings">>, lists:delete(E, get_groupings(U)), U).

model(R) ->
    ?G:to_json(R).

config(#{ <<"config">> := C}) ->
    C.

get_elements(#{<<"elements">> := Es}) ->
    Es.

get_groupings(#{<<"groupings">> := Gs}) ->
    Gs.

prop_merge() ->
    ?FORALL(R,
            grouping(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?G:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            grouping(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?G:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), grouping()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?G:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), grouping()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?G:name(id(?BIG_TIME), N, Hv)) ==
                              model_name(N, model(Hv)))
            end).
prop_type() ->
    ?FORALL({N, R},
            {type(), grouping()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?G:type(id(?BIG_TIME), N, Hv)) ==
                              model_type(N, model(Hv)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), grouping()},
            begin
                Hv = eval(O),
                O1 = ?G:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, grouping(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?G:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_config() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), grouping()},
            begin
                Hv = eval(O),
                O1 = ?G:set_config(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_config(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_config() ->
    ?FORALL({O, K}, ?LET(O, grouping(), {O, maybe_oneof(calc_map(set_config, O))}),
            begin
                Hv = eval(O),
                O1 = ?G:set_config(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_config(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_element() ->
    ?FORALL({E, O}, {non_blank_string(), grouping()},
            begin
                Hv = eval(O),
                O1 = ?G:add_element(id(?BIG_TIME), E, Hv),
                M1 = model_add_element(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_element() ->
    ?FORALL({O, K}, ?LET(O, grouping(), {O, maybe_oneof(calc_elements(O))}),
            begin
                Hv = eval(O),
                O1 = ?G:remove_element(id(?BIG_TIME), K, Hv),
                M1 = model_remove_element(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_grouping() ->
    ?FORALL({E, O}, {non_blank_string(), grouping()},
            begin
                Hv = eval(O),
                O1 = ?G:add_grouping(id(?BIG_TIME), E, Hv),
                M1 = model_add_grouping(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_grouping() ->
    ?FORALL({O, K}, ?LET(O, grouping(), {O, maybe_oneof(calc_groupings(O))}),
            begin
                Hv = eval(O),
                O1 = ?G:remove_grouping(id(?BIG_TIME), K, Hv),
                M1 = model_remove_grouping(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, grouping(),
            jsone:encode(?G:to_json(eval(E))) /= #{}).
