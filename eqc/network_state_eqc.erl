-module(network_state_eqc).

-include_lib("eqc/include/eqc.hrl").
-include("ft_test_helper.hrl").

-compile(export_all).

-define(N, ft_network).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

network() ->
    ?SIZED(Size, network(Size+1)).

network(Size) ->
    ?LAZY(oneof([{call, ?N, new, [id(Size)]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [network(Size - 1)],
                        oneof([
                               {call, ?N, load, [id(Size), O]},
                               %%{call, ?N, merge, [O, O]},

                               {call, ?N, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?N, name, [id(Size), non_blank_string(), O]},

                               {call, ?N, add_iprange, [id(Size), non_blank_string(), O]},
                               {call, ?N, remove_iprange, [id(Size), maybe_oneof(calc_ipranges(O)), O]},

                               {call, ?N, add_resolver, [id(Size), non_blank_string(), O]},
                               {call, ?N, remove_resolver, [id(Size), maybe_oneof(calc_resolvers(O)), O]},

                               {call, ?N, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?N, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]}

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


calc_ipranges({call, _, remove_iprange, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_ipranges(U)));
calc_ipranges({call, _, add_iprange, [_, E, U]}) ->
    [E | calc_ipranges(U)];
calc_ipranges({call, _, _, P}) ->
    calc_ipranges(lists:last(P));
calc_ipranges(_) ->
    [].

calc_resolvers({call, _, remove_resolver, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_resolvers(U)));
calc_resolvers({call, _, add_resolver, [_, E, U]}) ->
    [E | calc_resolvers(U)];
calc_resolvers({call, _, _, P}) ->
    calc_resolvers(lists:last(P));
calc_resolvers(_) ->
    [].


model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_add_iprange(E, U) ->
    r(<<"ipranges">>, lists:usort([E | ipranges(U)]), U).

model_remove_iprange(E, U) ->
    r(<<"ipranges">>, lists:delete(E, ipranges(U)), U).

model_add_resolver(E, U) ->
    r(<<"resolvers">>, lists:usort([E | resolvers(U)]), U).

model_remove_resolver(E, U) ->
    r(<<"resolvers">>, lists:delete(E, resolvers(U)), U).

model(R) ->
    ?N:to_json(R).


ipranges(#{<<"ipranges">> := M}) ->
    M.

resolvers(#{<<"resolvers">> := M}) ->
    M.

prop_merge() ->
    ?FORALL(R,
            network(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?N:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            network(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?N:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), network()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?N:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), network()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?N:name(id(?BIG_TIME), N, Hv)) ==
                              model_name(N, model(Hv)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), network()},
            begin
                Hv = eval(O),
                O1 = ?N:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, network(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?N:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_iprange() ->
    ?FORALL({E, O}, {non_blank_string(), network()},
            begin
                Hv = eval(O),
                O1 = ?N:add_iprange(id(?BIG_TIME), E, Hv),
                M1 = model_add_iprange(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_iprange() ->
    ?FORALL({O, K}, ?LET(O, network(), {O, maybe_oneof(calc_ipranges(O))}),
            begin
                Hv = eval(O),
                O1 = ?N:remove_iprange(id(?BIG_TIME), K, Hv),
                M1 = model_remove_iprange(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_add_resolver() ->
    ?FORALL({E, O}, {non_blank_string(), network()},
            begin
                Hv = eval(O),
                O1 = ?N:add_resolver(id(?BIG_TIME), E, Hv),
                M1 = model_add_resolver(E, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_resolver() ->
    ?FORALL({O, K}, ?LET(O, network(), {O, maybe_oneof(calc_resolvers(O))}),
            begin
                Hv = eval(O),
                O1 = ?N:remove_resolver(id(?BIG_TIME), K, Hv),
                M1 = model_remove_resolver(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, network(),
            jsone:encode(?N:to_json(eval(E))) /= <<>>).
