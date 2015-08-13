-module(ft_org_tests).
-define(M, ft_org).
-include_lib("eunit/include/eunit.hrl").

mkid() ->
    {ft_test_helper:timestamp(), test}.



to_json_test() ->
    Org = ?M:new(mkid()),
    OrgJ = [{<<"metadata">>,[]},
            {<<"name">>,<<>>},
            {<<"triggers">>,[]},
            {<<"uuid">>,<<>>}],
    ?assertEqual(OrgJ, ?M:to_json(Org)).

name_test() ->
    Name0 = <<"Test0">>,
    Org0 = ?M:new(mkid()),
    Org1 = ?M:name(mkid(), Name0, Org0),
    Name1 = <<"Test1">>,
    Org2 = ?M:name(mkid(), Name1, Org1),
    ?assertEqual(Name0, ?M:name(Org1)),
    ?assertEqual(Name1, ?M:name(Org2)).


remove_target_test() ->
    Org0 = ?M:new(mkid()),

    %% Add a grant trigger.
    Target1 = uuid:uuid4s(),
    UUID1 = uuid:uuid4s(),
    Tr1 = {vm_create, {grant, group, Target1, a}},
    Org1 = ?M:add_trigger(mkid(), UUID1, Tr1, Org0),

    %% Add a join trigger
    Target2 = uuid:uuid4s(),
    UUID2 = uuid:uuid4s(),
    Tr2 = {vm_create, {join, group, Target2}},
    Org2 = ?M:add_trigger(mkid(), UUID2, Tr2, Org1),

    %% Test if both triggers are added
    Ts1 = lists:sort([{UUID1, Tr1}, {UUID2, Tr2}]),
    ResTs1 = lists:sort(?M:triggers(Org2)),
    ?assertEqual(Ts1, ResTs1),

    %% Remove the first trigger
    Org3 = ?M:remove_target(mkid(), Target1, Org2),
    Ts2 = lists:sort([{UUID2, Tr2}]),
    ResTs2 = lists:sort(?M:triggers(Org3)),
    ?assertEqual(Ts2, ResTs2),

    %% Remove the seconds trigger
    Org4 = ?M:remove_target(mkid(), Target2, Org3),
    Ts3 = [],
    ResTs3 = lists:sort(?M:triggers(Org4)),
    ?assertEqual(Ts3, ResTs3),

    ok.
