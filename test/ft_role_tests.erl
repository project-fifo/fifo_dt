-module(ft_role_tests).
-define(M, ft_role).
-include_lib("eunit/include/eunit.hrl").

mkid() ->
    {ft_test_helper:timestamp(), test}.

update_perms_test() ->
    %% Prepare permissions and expected update results
    P1 = [<<"groups">>, <<"g1">>, <<"get">>],
    P1u = [<<"roles">>, <<"g1">>, <<"get">>],
    P2 = [<<"cloud">>, <<"groups">>, <<"list">>],
    P2u = [<<"cloud">>, <<"roles">>, <<"list">>],
    P3 = [<<"vms">>, <<"v1">>, <<"get">>],

    %% Create role and ?M:grant permissions
    R = ?M:new(mkid()),
    R1 = ?M:grant(mkid(), P1, R),
    R2 = ?M:grant(mkid(), P2, R1),
    R3 = ?M:grant(mkid(), P3, R2),
    %% Check if the result is as expected
    Ps1 = lists:sort([P1, P2, P3]),
    RPs1 = ?M:permissions(R3),
    ?assertEqual(Ps1, RPs1),

    %% Update and check for changes.
    R4 = ?M:update_permissions(mkid(), R3),
    Ps1u = lists:sort([P1u, P2u, P3]),
    RPs1u = ?M:permissions(R4),
    ?assertEqual(Ps1u, RPs1u).

to_json_test() ->
    Role = ?M:new(mkid()),
    RoleJ = [{<<"metadata">>,[]},
             {<<"name">>,<<>>},
             {<<"permissions">>,[]},
             {<<"uuid">>,<<>>}],
    ?assertEqual(RoleJ, ?M:to_json(Role)).

name_test() ->
    Name0 = <<"Test0">>,
    Role0 = ?M:new(mkid()),
    Role1 = ?M:name(mkid(), Name0, Role0),
    Name1 = <<"Test1">>,
    Role2 = ?M:name(mkid(), Name1, Role1),
    ?assertEqual(Name0, ?M:name(Role1)),
    ?assertEqual(Name1, ?M:name(Role2)).

permissions_test() ->
    P0 = [<<"P0">>],
    P1 = [<<"P1">>],
    Role0 = ?M:new(mkid()),
    Role1 = ?M:grant(mkid(), P0, Role0),
    Role2 = ?M:grant(mkid(), P1, Role1),
    Role3 = ?M:grant(mkid(), P0, Role2),
    Role4 = ?M:revoke(mkid(), P0, Role3),
    Role5 = ?M:revoke(mkid(), P1, Role3),
    ?assertEqual([P0], ?M:permissions(Role1)),
    ?assertEqual([P0, P1], ?M:permissions(Role2)),
    ?assertEqual([P0, P1], ?M:permissions(Role3)),
    ?assertEqual([P1], ?M:permissions(Role4)),
    ?assertEqual([P0], ?M:permissions(Role5)).
