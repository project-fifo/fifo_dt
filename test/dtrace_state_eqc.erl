-module(dtrace_state_eqc).

-ifdef(TEST).
-ifdef(EQC).

-import(ft_test_helper, [id/1, maybe_oneof/1]).

-include_lib("fqc/include/fqc.hrl").

-compile(export_all).

-define(D, ft_dtrace).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

dtrace() ->
    ?SIZED(Size, dtrace(Size+1)).

dtrace(Size) ->
    ?LAZY(oneof([{call, ?D, new, [id(Size)]} || Size == 1] ++
                    [?LETSHRINK(
                        [O], [dtrace(Size - 1)],
                        oneof([
                               {call, ?D, load, [id(Size), O]},
                               %%{call, ?D, merge, [O, O]},

                               {call, ?D, uuid, [id(Size), non_blank_string(), O]},
                               {call, ?D, name, [id(Size), non_blank_string(), O]},
                               {call, ?D, script, [id(Size), non_blank_string(), O]},

                               {call, ?D, set_metadata, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?D, set_metadata, [id(Size), maybe_oneof(calc_map(set_metadata, O)), delete, O]},

                               {call, ?D, set_config, [id(Size), non_blank_string(), non_blank_string(), O]},
                               {call, ?D, set_config, [id(Size), maybe_oneof(calc_map(set_config, O)), delete, O]}

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


r(K, V, U) ->
    lists:keystore(K, 1, U, {K, V}).

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_script(N, R) ->
    r(<<"script">>, N, R).

model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).

model_set_config(K, V, U) ->
    r(<<"config">>, lists:usort(r(K, V, config(U))), U).

model_delete_config(K, U) ->
    r(<<"config">>, lists:keydelete(K, 1, config(U)), U).

model(R) ->
    ?D:to_json(R).

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

config(U) ->
    {<<"config">>, M} = lists:keyfind(<<"config">>, 1, U),
    M.

prop_merge() ->
    ?FORALL(R,
            dtrace(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:merge(Hv, Hv)) ==
                              model(Hv))
            end).

prop_load() ->
    ?FORALL(R,
            dtrace(),
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:load(id(?BIG_TIME), Hv)) ==
                              model(Hv))
            end).
prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), dtrace()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R, Hv]),
                          model(?D:uuid(id(?BIG_TIME), N, Hv)) ==
                              model_uuid(N, model(Hv)))
            end).

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), dtrace()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:name(id(?BIG_TIME), N, Hv)) ==
                              model_name(N, model(Hv)))
            end).

prop_script() ->
    ?FORALL({N, R},
            {non_blank_string(), dtrace()},
            begin
                Hv = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~n", [R,Hv]),
                          model(?D:script(id(?BIG_TIME), N, Hv)) ==
                              model_script(N, model(Hv)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), dtrace()},
            begin
                Hv = eval(O),
                O1 = ?D:set_metadata(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_metadata(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({O, K}, ?LET(O, dtrace(), {O, maybe_oneof(calc_map(set_metadata, O))}),
            begin
                Hv = eval(O),
                O1 = ?D:set_metadata(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_metadata(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_set_config() ->
    ?FORALL({K, V, O}, {non_blank_string(), non_blank_string(), dtrace()},
            begin
                Hv = eval(O),
                O1 = ?D:set_config(id(?BIG_TIME), K, V, Hv),
                M1 = model_set_config(K, V, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_remove_config() ->
    ?FORALL({O, K}, ?LET(O, dtrace(), {O, maybe_oneof(calc_map(set_config, O))}),
            begin
                Hv = eval(O),
                O1 = ?D:set_config(id(?BIG_TIME), K, delete, Hv),
                M1 = model_delete_config(K, model(Hv)),
                ?WHENFAIL(io:format(user, "History: ~p~nHv: ~p~nModel: ~p~n"
                                    "Hv': ~p~nModel': ~p~n", [O, Hv, model(Hv), O1, M1]),
                          model(O1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, dtrace(),
            jsx:encode(?D:to_json(eval(E))) /= []).

-endif.
-endif.
