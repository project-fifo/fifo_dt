-module(role_state_eqc).

-import(ft_test_helper, [model_set_metadata/3, model_delete_metadata/2,
                         metadata/1, r/3, permission/0, id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).
-define(R, ft_role).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

role() ->
    ?SIZED(Size, role(Size)).

role(Size) ->
    ?LAZY(oneof([{call, ?R, new, [id(Size)]} || Size == 0] ++
                    [?LETSHRINK(
                        [R], [role(Size - 1)],
                        oneof([
                               {call, ?R, load, [id(Size), R]},
                               {call, ?R, uuid, [id(Size), non_blank_string(), R]},
                               {call, ?R, name, [id(Size), non_blank_string(), R]},
                               {call, ?R, grant, [id(Size), permission(), R]},
                               {call, ?R, revoke, [id(Size), maybe_oneof(calc_perms(R)), R]}
                              ]))
                     || Size > 0])).

calc_metadata({call, _, set_metadata, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_metadata(U)));
calc_metadata({call, _, set_metadata, [_, I, _K, U]}) ->
    [I | calc_metadata(U)];
calc_metadata({call, _, _, P}) ->
    calc_metadata(lists:last(P));
calc_metadata(_) ->
    [].

calc_perms({call, _, grant, [_, P, R]}) ->
    [P | calc_perms(R)];
calc_perms({call, _, revoke, [_, P, R]}) ->
    lists:delete(P, lists:usort(calc_perms(R)));
calc_perms({call, _, _, R}) ->
    calc_perms(lists:last(R));
calc_perms(_) ->
    [].

model_uuid(N, R) ->
    r(<<"uuid">>, N, R).

model_name(N, R) ->
    r(<<"name">>, N, R).

model_revoke(P, R) ->
    r(<<"permissions">>, lists:delete(P, permissions(R)), R).

model_grant(P, R) ->
    r(<<"permissions">>, lists:usort([P | permissions(R)]), R).


model(R) ->
    ?R:to_json(R).

permissions(#{<<"permissions">> := Ps}) ->
    Ps.

prop_name() ->
    ?FORALL({N, R},
            {non_blank_string(), role()},
            begin
                Role = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nRole: ~p~n", [R,Role]),
                          model(?R:name(id(?BIG_TIME), N, Role)) ==
                              model_name(N, model(Role)))
            end).

prop_uuid() ->
    ?FORALL({N, R},
            {non_blank_string(), role()},
            begin
                Role = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nRole: ~p~n", [R, Role]),
                          model(?R:uuid(id(?BIG_TIME), N, Role)) ==
                              model_uuid(N, model(Role)))
            end).

prop_grant() ->
    ?FORALL({P, R},
            {permission(), role()},
            begin
                Role = eval(R),
                R1 = ?R:grant(id(?BIG_TIME), P, Role),
                M1 = model_grant(P, model(Role)),
                ?WHENFAIL(io:format(user, "History: ~p~nRole: ~p~nModel: ~p~n"
                                    "Role: ~p~nModel: ~p~n", [R, Role, model(Role), R1, M1]),
                          model(R1) == M1)
            end).

prop_revoke() ->
    ?FORALL({R, P}, ?LET(R, role(), {R, maybe_oneof(calc_perms(R))}),
            begin
                Role = eval(R),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [R, Role, model(Role)]),
                          model(?R:revoke(id(?BIG_TIME), P, Role)) ==
                              model_revoke(P, model(Role)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, R}, {non_blank_string(), non_blank_string(), role()},
            begin
                Role = eval(R),
                R1 = ?R:set_metadata(id(?BIG_TIME), K, V, Role),
                M1 = model_set_metadata(K, V, model(Role)),
                ?WHENFAIL(io:format(role, "History: ~p~nRole: ~p~nModel: ~p~n"
                                    "Role': ~p~nModel': ~p~n", [R, Role, model(Role), R1, M1]),
                          model(R1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({R, K}, ?LET(R, role(), {R, maybe_oneof(calc_metadata(R))}),
            begin
                Role = eval(R),
                R1 = ?R:set_metadata(id(?BIG_TIME), K, delete, Role),
                M1 = model_delete_metadata(K, model(Role)),
                ?WHENFAIL(io:format(role, "History: ~p~nRole: ~p~nModel: ~p~n"
                                    "Role': ~p~nModel': ~p~n", [R, Role, model(Role), R1, M1]),
                          model(R1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, role(),
            jsone:encode(?R:to_json(eval(E))) /= <<>>).
