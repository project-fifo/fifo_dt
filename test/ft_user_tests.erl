-module(ft_user_tests).

-include_lib("eunit/include/eunit.hrl").
-define(M, ft_user).

-ignore_xref([name_test/0, password_test/0, permissions_test/0, roles_test/0, test/0, to_json_test/0, update_perms_test/0, yubikey_test/0]).
mkid() ->
    {ft_test_helper:timestamp(), test}.

update_perms_test() ->
    %% Prepare ?M:permissions and expected update results
    P1 = [<<"groups">>, <<"g1">>, <<"get">>],
    P1u = [<<"roles">>, <<"g1">>, <<"get">>],
    P2 = [<<"cloud">>, <<"groups">>, <<"list">>],
    P2u = [<<"cloud">>, <<"roles">>, <<"list">>],
    P3 = [<<"vms">>, <<"v1">>, <<"get">>],

    %% Create user and ?M:grant ?M:permissions
    U = ?M:new(mkid()),
    U1 = ?M:grant(mkid(), P1, U),
    U2 = ?M:grant(mkid(), P2, U1),
    U3 = ?M:grant(mkid(), P3, U2),
    %% Check if the result is as expected
    Ps1 = lists:sort([P1, P2, P3]),
    RPs1 = ?M:permissions(U3),
    ?assertEqual(Ps1, RPs1),

    %% Update and check for changes.
    U4 = ?M:update_permissions(mkid(), U3),
    Ps1u = lists:sort([P1u, P2u, P3]),
    RPs1u = ?M:permissions(U4),
    ?assertEqual(Ps1u, RPs1u).

to_json_test() ->
    User = ?M:new(mkid()),
    UserJ = [
             {<<"keys">>, []},
             {<<"metadata">>, []},
             {<<"name">>, <<>>},
             {<<"org">>, <<>>},
             {<<"orgs">>, []},
             {<<"permissions">>, []},
             {<<"roles">>, []},
             {<<"uuid">>, <<>> },
             {<<"yubikeys">>, []}
            ],
    ?assertEqual(UserJ, ?M:to_json(User)).

name_test() ->
    Name0 = <<"Test0">>,
    User0 = ?M:new(mkid()),
    User1 = ?M:name(mkid(), Name0, User0),
    Name1 = <<"Test1">>,
    User2 = ?M:name(mkid(), Name1, User1),
    ?assertEqual(Name0, ?M:name(User1)),
    ?assertEqual(Name1, ?M:name(User2)).

yubikey_test() ->
    Key = <<"Test0">>,
    User0 = ?M:new(mkid()),
    User1 = ?M:add_yubikey(mkid(), Key, User0),
    ?assertEqual([<<"Test0">>], ?M:yubikeys(User1)).

password_test() ->
    Name = "Test",
    Password = "Test",
    User0 = ?M:new(mkid()),
    User1 = ?M:name(mkid(), Name, User0),
    User2 = ?M:password(mkid(), Password, User1),
    ?assertEqual(Password, ?M:password(User2)).

permissions_test() ->
    P0 = [<<"P0">>],
    P1 = [<<"P1">>],
    User0 = ?M:new(mkid()),
    User1 = ?M:grant(mkid(), P0, User0),
    User2 = ?M:grant(mkid(), P1, User1),
    User3 = ?M:grant(mkid(), P0, User2),
    User4 = ?M:revoke(mkid(), P0, User3),
    User5 = ?M:revoke(mkid(), P1, User3),
    ?assertEqual([P0], ?M:permissions(User1)),
    ?assertEqual([P0, P1], ?M:permissions(User2)),
    ?assertEqual([P0, P1], ?M:permissions(User3)),
    ?assertEqual([P1], ?M:permissions(User4)),
    ?assertEqual([P0], ?M:permissions(User5)).

roles_test() ->
    G0 = "G0",
    G1 = "G1",
    User0 = ?M:new(mkid()),
    User1 = ?M:join(mkid(), G0, User0),
    User2 = ?M:join(mkid(), G1, User1),
    User3 = ?M:join(mkid(), G0, User2),
    User4 = ?M:leave(mkid(), G0, User3),
    User5 = ?M:leave(mkid(), G1, User3),
    ?assertEqual([G0], ?M:roles(User1)),
    ?assertEqual([G0, G1], ?M:roles(User2)),
    ?assertEqual([G0, G1], ?M:roles(User3)),
    ?assertEqual([G1], ?M:roles(User4)),
    ?assertEqual([G0], ?M:roles(User5)).
