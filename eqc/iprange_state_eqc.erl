-module(iprange_state_eqc).

-import(ft_test_helper, [id/1, maybe_oneof/1]).
-import(fqc, [non_blank_string/0, maybe_oneof/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(I, ft_iprange).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

iprange() ->
    ?SIZED(Size, iprange(Size+1)).

non_neg_int() ->
    ?SUCHTHAT(I, int(), I > 0).

iprange(Size) ->
    ?LAZY(oneof([{call, ?I, new, [id(Size), 100, 110]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [iprange(Size - 1)],
                        oneof([
                               {call, ?I, load, [id(Size), O]},
                               %%{call, ?I, merge, [O, O]},

                               {call, ?I, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?I, name, [id(Size), non_blank_string(), O]},
                               {call, ?I, network, [id(Size), non_neg_int(), O]},
                               {call, ?I, netmask, [id(Size), non_neg_int(), O]},
                               {call, ?I, gateway, [id(Size), non_neg_int(), O]},
                               {call, ?I, tag, [id(Size), non_blank_string(), O]},
                               {call, ?I, vlan, [id(Size), non_neg_int(), O]},

                               {call, ?I, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?I, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]}

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

r(K, V, U) ->
    lists:keystore(K, 1, U, {K, V}).

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_network(N, R) ->
    r(<<"network">>, N, R).

model_netmask(N, R) ->
    r(<<"netmask">>, N, R).

model_gateway(N, R) ->
    r(<<"gateway">>, N, R).

model_tag(N, R) ->
    r(<<"tag">>, N, R).

model_vlan(N, R) ->
    r(<<"vlan">>, N, R).

model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).

model_add_iprange(E, U) ->
    r(<<"ipranges">>, lists:usort([E | ipranges(U)]), U).

model_remove_iprange(E, U) ->
    r(<<"ipranges">>, lists:delete(E, ipranges(U)), U).

model(R) ->
    ?I:to_json(R).

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

ipranges(U) ->
    {<<"ipranges">>, M} = lists:keyfind(<<"ipranges">>, 1, U),
    M.

prop_merge() ->
    ?FORALL(R,
            iprange(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?I:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            iprange(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?I:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?I:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:name(id(?BIG_TIME), N, Hv)) ==
                              model_name(N, model(Hv)))
            end).

prop_network() ->
    ?FORALL({N, R},
            {non_neg_int(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:network(id(?BIG_TIME), N, Hv)) ==
                              model_network(N, model(Hv)))
            end).

prop_netmask() ->
    ?FORALL({N, R},
            {non_neg_int(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:netmask(id(?BIG_TIME), N, Hv)) ==
                              model_netmask(N, model(Hv)))
            end).

prop_gateway() ->
    ?FORALL({N, R},
            {non_neg_int(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:gateway(id(?BIG_TIME), N, Hv)) ==
                              model_gateway(N, model(Hv)))
            end).

prop_tag() ->
    ?FORALL({N, R},
            {non_blank_string(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:tag(id(?BIG_TIME), N, Hv)) ==
                              model_tag(N, model(Hv)))
            end).

prop_vlan() ->
    ?FORALL({N, R},
            {non_neg_int(), iprange()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?I:vlan(id(?BIG_TIME), N, Hv)) ==
                              model_vlan(N, model(Hv)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), iprange()},
            begin
                Hv = eval(O),
                O1 = ?I:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, iprange(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?I:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, iprange(),
            jsx:encode(?I:to_json(eval(E))) /= []).
