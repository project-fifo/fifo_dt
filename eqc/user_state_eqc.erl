-module(user_state_eqc).
-import(ft_test_helper, [model_set_metadata/3, model_delete_metadata/2,
                         metadata/1, r/3, permission/0, id/1, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).


-define(U, ft_user).
-define(M, ?MODULE).
-define(NBS, non_blank_string()).
-define(C(Fn, Args), {call, ?U, Fn, Args}).

%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

token_type() ->
    oneof([access,refresh]).

%% We want the expiery time somewhere around our current time.
%% this is a bit problematic since test **might** run just at the
%% switch of a second leading to the real elemet being expired but the
%% model not. Need to figure out if this is a real problem or not.
expiery() ->
    oneof([?LET(S, choose(-20, 20), erlang:system_time(seconds) + S),
           infinity]).

uuid() ->
    fifo_utils:uuid(user).

prefixed_id(Pfx) ->
    ?LET(Prefix, Pfx, <<Prefix/binary, (uuid())/binary>>).

client() ->
    oneof([prefixed_id(<<"client-">>), undefined]).

comment() ->
    oneof([?NBS, undefined]).

scope() ->
    list(?NBS).

token_id() ->
    prefixed_id(<<"tid-">>).

token_token() ->
    prefixed_id(<<"token-">>).

token() ->
    {
      %% ID of the token
      token_id(),
      %% type of the token
      token_type(),
      %% token itself, for simplifications we use a uuid here since tokens
      %% are by definition unique. Any other unique binary would sufice.
      token_token(),
      %% the expiery time, we want to
      expiery(),
      %% Client that requested the token
      client(),
      %% requested scope
      scope(),
      %% Optional comment for the token
      comment()
    }.

user() ->
    ?SIZED(Size, user(Size)).

user_calls(Size) ->
    ?LETSHRINK(
       [U], [user(Size - 1)],
       oneof([
              %% General functions for users
              ?C(load, [id(Size), U]),
              ?C(uuid, [id(Size), ?NBS, U]),
              ?C(name, [id(Size), ?NBS, U]),
              ?C(password, [id(Size), ?NBS, U]),

              %% Metadata functions
              ?C(set_metadata, [id(Size), ?NBS, ?NBS, U]),
              ?C(set_metadata, [id(Size), maybe_oneof(calc_metadata(U)), delete, U]),

              %% permission related functions
              ?C(grant, [id(Size), permission(), U]),
              ?C(revoke, [id(Size), maybe_oneof(calc_perms(U)), U]),

              %% Role functions
              ?C(join, [id(Size), ?NBS, U]),
              ?C(leave, [id(Size), maybe_oneof(calc_roles(U)), U]),

              %% Org functions
              ?C(join_org, [id(Size), ?NBS, U]),
              ?C(leave_org, [id(Size), maybe_oneof(calc_orgs(U)), U]),

              %% SSH Key functions
              ?C(add_key, [id(Size), ?NBS, ?NBS, U]),
              ?C(revoke_key, [id(Size), maybe_oneof(calc_keys(U)), U]),

              %% Yubikey functions
              ?C(add_yubikey, [id(Size), ?NBS, U]),
              ?C(remove_yubikey, [id(Size), maybe_oneof(calc_yubikeys(U)), U]),

              %% Token functions
              {call, ?U, add_token, [id(Size), token(), U]},
              {call, ?M, remove_token, [id(Size), maybe_oneof(calc_tokens(U)), U]}
             ])).

user(Size) ->
    ?LAZY(
       oneof(
         [?C(new, [id(Size)]) || Size == 0] ++
             [user_calls(Size) || Size > 0])).

remove_token(ID, {_TokenID, Token}, User) ->
    ?U:remove_token(ID, Token, User);
remove_token(ID, Token, User) ->
    ?U:remove_token(ID, Token, User).

calc_tokens({call, _, add_token, [_, {ID, _, Token, _, _, _, _}, U]}) ->
    [{ID, Token} | calc_tokens(U)];
calc_tokens({call, _, remove_token, [_, TID, U]}) ->
    lists:delete(TID, lists:usort(calc_tokens(U)));
calc_tokens({call, _, remove_token_id, [_, TID, U]}) ->
    lists:delete(TID, lists:usort(calc_tokens(U)));
calc_tokens({call, _, _, P}) ->
    calc_tokens(lists:last(P));
calc_tokens(_) ->
    [].

calc_yubikeys({call, _, add_yubikey, [_, K, U]}) ->
    [K | calc_yubikeys(U)];
calc_yubikeys({call, _, remove_yubikey, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_yubikeys(U)));
calc_yubikeys({call, _, _, P}) ->
    calc_yubikeys(lists:last(P));
calc_yubikeys(_) ->
    [].

calc_keys({call, _, add_key, [_, I, _K, U]}) ->
    [I | calc_keys(U)];
calc_keys({call, _, revoke_key, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_keys(U)));
calc_keys({call, _, _, P}) ->
    calc_keys(lists:last(P));
calc_keys(_) ->
    [].

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

calc_orgs({call, _, join_org, [_, K, U]}) ->
    [K | calc_orgs(U)];
calc_orgs({call, _, leave_org, [_, K, U]}) ->
    lists:delete(K, lists:usort(calc_orgs(U)));
calc_orgs({call, _, _, P}) ->
    calc_orgs(lists:last(P));
calc_orgs(_) ->
    [].

calc_perms({call, _, grant, [_, P, U]}) ->
    [P | calc_perms(U)];
calc_perms({call, _, revoke, [_, P, U]}) ->
    lists:delete(P, lists:usort(calc_perms(U)));
calc_perms({call, _, _, P}) ->
    calc_perms(lists:last(P));
calc_perms(_) ->
    [].
model(U) ->
    U1 = ?U:to_json(U),
    U1#{<<"password">> => ?U:password(U)}.

model_uuid(N, U) ->
    r(<<"uuid">>, N, U).

model_name(N, U) ->
    r(<<"name">>, N, U).

model_password(N, U) ->
    r(<<"password">>, N, U).

model_revoke(P, U) ->
    r(<<"permissions">>, lists:delete(P, permissions(U)), U).

model_grant(P, U) ->
    r(<<"permissions">>, lists:usort([P | permissions(U)]), U).


model_leave_org(P, U) ->
    r(<<"orgs">>, lists:delete(P, orgs(U)), U).

model_join_org(P, U) ->
    r(<<"orgs">>, lists:usort([P | orgs(U)]), U).

model_leave_role(P, U) ->
    r(<<"roles">>, lists:delete(P, roles(U)), U).

model_add_role(P, U) ->
    r(<<"roles">>, lists:usort([P | roles(U)]), U).

model_add_key(I, K, U) ->
    r(<<"keys">>, r(I, K, keys(U)), U).

model_revoke_key(I, U) ->
    r(<<"keys">>, maps:remove(I, keys(U)), U).

model_add_yubikey(K, U) ->
    r(<<"yubikeys">>, lists:usort([K | yubikeys(U)]), U).

model_remove_yubikey(P, U) ->
    r(<<"yubikeys">>, lists:delete(P, yubikeys(U)), U).

model_token({ID, Type, _Token, Exp, Client, Scope, Comment}) ->
    J = #{
      <<"type">> => atom_to_binary(Type, utf8),
      <<"id">> => ID,
      <<"scope">> => Scope
     },
    J2 = case Exp of
             infinity ->
                 J;
             _ ->
                 J#{<<"expiery">> => Exp}
         end,
    J3 = case Client of
             undefined ->
                 J2;
             _ ->
                 J2#{<<"client">> => Client}
         end,
    case Comment of
        undefined ->
            J3;
        _ ->
            J3#{<<"comment">> => Comment} 
    end.

model_add_token(T, U) ->
    r(<<"tokens">>, lists:usort([model_token(T) | tokens(U)]), U).

%% We need to use the same model for removing tokens and token id's
%% since in the model the token itself is never printed!
model_remove_token({TID, _Token}, U) ->
    Tokens = lists:filter(fun (T) ->
                                  jsxd:get(<<"id">>, T) =/= {ok, TID}
                          end, tokens(U)),
    r(<<"tokens">>, Tokens, U);
%% if we have a non existant token we simply won't remove it.
model_remove_token(_, U) ->
    U.

permissions(#{<<"permissions">> :=Ps}) ->
    Ps.

roles(#{<<"roles">> := Ps}) ->
    Ps.

orgs(#{<<"orgs">> := Ps}) ->
    Ps.

yubikeys(#{<<"yubikeys">> := Ps}) ->
    Ps.

keys(#{<<"keys">> := Ps}) ->
    Ps.


tokens(#{<<"tokens">> := M}) ->
    M.

has_permissions(U) ->
    ?U:permissions(U) =/= [].

has_orgs(U) ->
    ?U:orgs(U) =/= [].

has_roles(U) ->
    ?U:roles(U) =/= [].

has_yubikeys(U) ->
    ?U:yubikeys(U) =/= [].

has_keys(U) ->
    ?U:keys(U) =/= [].

prop_name() ->
    ?FORALL({N,U},
            {?NBS,user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~n", [U, User]),
                          model(?U:name(id(?BIG_TIME),N,User)) ==
                              model_name(N, model(User)))
            end).

prop_uuid() ->
    ?FORALL({N,U},
            {?NBS,user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~n", [U, User]),
                          model(?U:uuid(id(?BIG_TIME),N,User)) ==
                              model_uuid(N, model(User)))
            end).

prop_password() ->
    ?FORALL({N,U},
            {?NBS,user()},
            begin
                User = eval(U),
                U1 = ?U:password(id(?BIG_TIME),N,User),
                M1 = model_password(N, model(User)),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n"
                                    "User': ~p~nModel': ~p~n", [U, User, model(User), U1, M1]),
                          model(U1) == M1)
            end).

prop_grant() ->
    ?FORALL({P,U},
            {permission(),user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:grant(id(?BIG_TIME), P, User)) ==
                              model_grant(P, model(User)))
            end).

prop_revoke() ->
    ?FORALL({U, P}, ?LET(U, ?SUCHTHAT(T, user(), calc_perms(T) =/= []), {U, oneof(calc_perms(U))}),
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:revoke(id(?BIG_TIME), P, User)) ==
                              model_revoke(P, model(User)))
            end).

prop_add_role() ->
    ?FORALL({R,U},
            {?NBS,user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:join(id(?BIG_TIME), R, User)) ==
                              model_add_role(R, model(User)))
            end).

prop_add_org() ->
    ?FORALL({O,U},
            {?NBS, user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:join_org(id(?BIG_TIME), O, User)) ==
                              model_join_org(O, model(User)))
            end).

prop_add_yubikey() ->
    ?FORALL({K, U},
            {?NBS, user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:add_yubikey(id(?BIG_TIME), K, User)) ==
                              model_add_yubikey(K, model(User)))
            end).

prop_add_token() ->
    ?FORALL({T, U},
            {token(), user()},
            begin
                User = eval(U),
                User1 = ?U:add_token(id(?BIG_TIME), T, User),
                M = model(User),
                M1 = model_add_token(T, M),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n"
                                    "U': ~p~n"
                                    "M': ~p~n"
                                    "M(U'): ~p~n",
                                    [U, User, M, User1, M1, model(User1)]),
                          model(User1) == M1)
            end).

prop_remove_token() ->
    ?FORALL({U, T}, ?LET(U, user(), {U, maybe_oneof(calc_tokens(U))}),
            begin
                User = eval(U),
                M = model(User),
                U1 = ?M:remove_token(id(?BIG_TIME), T, User),
                M1 = model_remove_token(T, model(User)),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n"
                                    "U': ~p~n"
                                    "T: ~p~n"
                                    "M': ~p~n"
                                    "M(U'): ~p~n",
                                    [U, User, M, U1, T, M1, model(U1)]),
                          model(U1) == M1)
            end).

prop_add_key() ->
    ?FORALL({I, K, U},
            {?NBS, ?NBS, user()},
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:add_key(id(?BIG_TIME), I, K, User)) ==
                              model_add_key(I, K, model(User)))
            end).

prop_leave_org() ->
    ?FORALL({U, O}, ?LET(U, user(), {U, maybe_oneof(calc_orgs(U))}),
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:leave_org(id(?BIG_TIME), O, User)) ==
                              model_leave_org(O, model(User)))
            end).

prop_leave_role() ->
    ?FORALL({U, P}, ?LET(U, user(), {U, maybe_oneof(calc_roles(U))}),
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:leave(id(?BIG_TIME), P, User)) ==
                              model_leave_role(P, model(User)))
            end).

prop_remove_yubikey() ->
    ?FORALL({U, K}, ?LET(U, user(), {U, maybe_oneof(calc_yubikeys(U))}),
            begin
                User = eval(U),
                U1 = ?U:remove_yubikey(id(?BIG_TIME), K, User),
                M1 = model_remove_yubikey(K, model(User)),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(U1) == M1)
            end).

prop_remove_key() ->
    ?FORALL({U, K}, ?LET(U, user(), {U, maybe_oneof(calc_keys(U))}),
            begin
                User = eval(U),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n", [U, User, model(User)]),
                          model(?U:revoke_key(id(?BIG_TIME), K, User)) ==
                              model_revoke_key(K, model(User)))
            end).

prop_set_metadata() ->
    ?FORALL({K, V, U}, {?NBS, ?NBS, user()},
            begin
                User = eval(U),
                U1 = ?U:set_metadata(id(?BIG_TIME), K, V, User),
                M1 = model_set_metadata(K, V, model(User)),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n"
                                    "User': ~p~nModel': ~p~n", [U, User, model(User), U1, M1]),
                          model(U1) == M1)
            end).

prop_remove_metadata() ->
    ?FORALL({U, K}, ?LET(U, user(), {U, maybe_oneof(calc_metadata(U))}),
            begin
                User = eval(U),
                U1 = ?U:set_metadata(id(?BIG_TIME), K, delete, User),
                M1 = model_delete_metadata(K, model(User)),
                ?WHENFAIL(io:format(user, "History: ~p~nUser: ~p~nModel: ~p~n"
                                    "User': ~p~nModel': ~p~n", [U, User, model(User), U1, M1]),
                          model(U1) == M1)
            end).

prop_to_json() ->
    ?FORALL(E, user(),
            jsone:encode(?U:to_json(eval(E))) /= #{}).
