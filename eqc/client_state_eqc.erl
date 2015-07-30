-module(client_state_eqc).


-import(ft_test_helper, [id/1, permission/0, maybe_oneof/1]).
-import(fqc, [non_blank_string/0, maybe_oneof/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(U, ft_client).

%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

client() ->
    ?SIZED(Size, client(Size)).

type() ->
    oneof([confidential, public]).

client(Size) ->
    ?LAZY(oneof([{call, ?U, new, [id(Size)]} || Size == 0] ++
                    [?LETSHRINK(
                        [U], [client(Size - 1)],
                        oneof([
                               {call, ?U, load, [id(Size), U]},
                               {call, ?U, uuid, [id(Size), non_blank_string(), U]},
                               {call, ?U, name, [id(Size), non_blank_string(), U]},
                               {call, ?U, client_id, [id(Size), non_blank_string(), U]},
                               {call, ?U, secret, [id(Size), non_blank_string(), U]},
                               {call, ?U, type, [id(Size), type(), U]},
                               {call, ?U, add_uri, [id(Size), non_blank_string(), U]},
                               {call, ?U, remove_uri, [id(Size), maybe_oneof(calc_uris(U)), U]},
                               {call, ?U, grant, [id(Size), permission(), U]},
                               {call, ?U, revoke, [id(Size), maybe_oneof(calc_perms(U)), U]},
                               {call, ?U, join, [id(Size), non_blank_string(), U]},
                               {call, ?U, leave, [id(Size), maybe_oneof(calc_roles(U)), U]},
                               {call, ?U, set_metadata, [id(Size), non_blank_string(), non_blank_string(), U]},
                               {call, ?U, set_metadata, [id(Size), maybe_oneof(calc_metadata(U)), delete, U]}]))
                     || Size > 0])).


calc_metadata({call, _, set_metadata, [_, delete, K, U]}) ->
    lists:delete(K, lists:usort(calc_metadata(U)));
calc_metadata({call, _, set_metadata, [_, I, _K, U]}) ->
    [I | calc_metadata(U)];
calc_metadata({call, _, _, P}) ->
    calc_metadata(lists:last(P));
calc_metadata(_) ->
    [].

calc_roles({call, _, join, [_, K, U]}) ->
    [K | calc_roles(U)];
calc_roles({call, _, leave, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_roles(U)));
calc_roles({call, _, _, P}) ->
    calc_roles(lists:last(P));
calc_roles(_) ->
    [].

calc_perms({call, _, grant, [_, P, U]}) ->
    [P | calc_perms(U)];
calc_perms({call, _, revoke, [_, P, U]}) ->
    lists:delete(P, lists:usort(calc_perms(U)));
calc_perms({call, _, _, P}) ->
    calc_perms(lists:last(P));
calc_perms(_) ->
    [].

calc_uris({call, _, add_uri, [_, P, U]}) ->
    [P | calc_uris(U)];
calc_uris({call, _, remove_uri, [_, P, U]}) ->
    lists:delete(P, lists:usort(calc_uris(U)));
calc_uris({call, _, _, P}) ->
    calc_uris(lists:last(P));
calc_uris(_) ->
    [].

r(K, V, U) ->
    lists:keystore(K, 1, U, {K, V}).

model(U) ->
    lists:sort([{<<"secret">>, ?U:secret(U)} | ?U:to_json(U)]).

model_uuid(N, U) ->
    r(<<"uuid">>, N, U).

model_name(N, U) ->
    r(<<"name">>, N, U).

model_client_id(N, U) ->
    r(<<"client_id">>, N, U).

model_secret(N, U) ->
    r(<<"secret">>, N, U).

model_remove_uri(URI, U) ->
    r(<<"redirect_uris">>, lists:delete(URI, uris(U)), U).

model_add_uri(URI, U) ->
    r(<<"redirect_uris">>, lists:usort([URI | uris(U)]), U).

model_revoke(P, U) ->
    r(<<"permissions">>, lists:delete(P, permissions(U)), U).

model_grant(P, U) ->
    r(<<"permissions">>, lists:usort([P | permissions(U)]), U).

model_set_metadata(K, V, U) ->
    r(<<"metadata">>, lists:usort(r(K, V, metadata(U))), U).

model_delete_metadata(K, U) ->
    r(<<"metadata">>, lists:keydelete(K, 1, metadata(U)), U).

model_leave_role(P, U) ->
    r(<<"roles">>, lists:delete(P, roles(U)), U).

model_add_role(P, U) ->
    r(<<"roles">>, lists:usort([P | roles(U)]), U).

permissions(U) ->
    {<<"permissions">>, Ps} = lists:keyfind(<<"permissions">>, 1, U),
    Ps.

uris(U) ->
    {<<"redirect_uris">>, Ps} = lists:keyfind(<<"redirect_uris">>, 1, U),
    Ps.

roles(U) ->
    {<<"roles">>, Ps} = lists:keyfind(<<"roles">>, 1, U),
    Ps.

metadata(U) ->
    {<<"metadata">>, M} = lists:keyfind(<<"metadata">>, 1, U),
    M.

has_permissions(U) ->
    ?U:permissions(U) =/= [].

has_roles(U) ->
    ?U:roles(U) =/= [].

has_uris(U) ->
    ?U:redirect_uris(U) =/= [].

prop_uuid() ->
    ?FORALL({N,U},
            {non_blank_string(),client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~n", [U, Client]),
                          model(?U:uuid(id(?BIG_TIME),N,Client)) ==
                              model_uuid(N, model(Client)))
            end).

prop_client_id() ->
    ?FORALL({N,U},
            {non_blank_string(),client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~n", [U, Client]),
                          model(?U:client_id(id(?BIG_TIME),N,Client)) ==
                              model_client_id(N, model(Client)))
            end).

prop_name() ->
    ?FORALL({N,U},
            {non_blank_string(),client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~n", [U, Client]),
                          model(?U:name(id(?BIG_TIME),N,Client)) ==
                              model_name(N, model(Client)))
            end).

prop_secret() ->
    ?FORALL({N,U},
            {non_blank_string(),client()},
            begin
                Client = eval(U),
                U1 = ?U:secret(id(?BIG_TIME),N,Client),
                M1 = model_secret(N, model(Client)),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n"
                                    "Client': ~p~nModel': ~p~n", [U, Client, model(Client), U1, M1]),
                          model(U1) == M1)
            end).

prop_grant() ->
    ?FORALL({P,U},
            {permission(),client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:grant(id(?BIG_TIME), P, Client)) ==
                              model_grant(P, model(Client)))
            end).

prop_revoke() ->
    ?FORALL({U, P}, ?LET(U, ?SUCHTHAT(T, client(), calc_perms(T) =/= []), {U, oneof(calc_perms(U))}),
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:revoke(id(?BIG_TIME), P, Client)) ==
                              model_revoke(P, model(Client)))
            end).

prop_add_role() ->
    ?FORALL({R,U},
            {non_blank_string(),client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:join(id(?BIG_TIME), R, Client)) ==
                              model_add_role(R, model(Client)))
            end).

prop_add_uri() ->
    ?FORALL({URI,U},
            {non_blank_string(), client()},
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:add_uri(id(?BIG_TIME), URI, Client)) ==
                              model_add_uri(URI, model(Client)))
            end).

prop_remove_uri() ->
    ?FORALL({U, URI}, ?LET(U, client(), {U, maybe_oneof(calc_uris(U))}),
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:remove_uri(id(?BIG_TIME), URI, Client)) ==
                              model_remove_uri(URI, model(Client)))
            end).

prop_leave_role() ->
    ?FORALL({U, P}, ?LET(U, client(), {U, maybe_oneof(calc_roles(U))}),
            begin
                Client = eval(U),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n", [U, Client, model(Client)]),
                          model(?U:leave(id(?BIG_TIME), P, Client)) ==
                              model_leave_role(P, model(Client)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, U}, {non_blank_string(), non_blank_string(), client()},
            begin
                Client = eval(U),
                U1 = ?U:set_metadata(id(?BIG_TIME), K, V, Client),
                M1 = model_set_metadata(K, V, model(Client)),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n"
                                    "Client': ~p~nModel': ~p~n", [U, Client, model(Client), U1, M1]),
                          model(U1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({U, K}, ?LET(U, client(), {U, maybe_oneof(calc_metadata(U))}),
            begin
                Client = eval(U),
                U1 = ?U:set_metadata(id(?BIG_TIME), K, delete, Client),
                M1 = model_delete_metadata(K, model(Client)),
                ?WHENFAIL(io:format(client, "History: ~p~nClient: ~p~nModel: ~p~n"
                                    "Client': ~p~nModel': ~p~n", [U, Client, model(Client), U1, M1]),
                          model(U1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, client(),
            jsx:encode(?U:to_json(eval(E))) /= []).
