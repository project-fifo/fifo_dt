-module(hostname_state_eqc).

-import(ft_test_helper, [id/1, permission/0, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).
-define(R, ft_hostname).
%% This is larger then and time we ever get in the size, used for ensure setting data
%% in LWW registers.
-define(BIG_TIME, 1000000000).

hostname() ->
    ?SIZED(Size, hostname(Size)).

a() ->
    {non_blank_string(), int()}.

hostname(Size) ->
    ?LAZY(oneof([{call, ?R, new, [id(Size)]} || Size == 0] ++
                    [?LETSHRINK(
                        [R], [hostname(Size - 1)],
                        oneof([
                               {call, ?R, load, [id(Size), R]},
                               {call, ?R, add_a, [id(Size), a(), R]},
                               {call, ?R, remove_a, [id(Size), maybe_oneof(calc_as(R)), R]}
                              ]))
                     || Size > 0])).

calc_as({call, _, add_a, [_, P, R]}) ->
    [P | calc_as(R)];
calc_as({call, _, remove_a, [_, P, R]}) ->
    lists:delete(P, lists:usort(calc_as(R)));
calc_as({call, _, _, R}) ->
    calc_as(lists:last(R));
calc_as(_) ->
    [].

prop_add_a() ->
    ?FORALL({P, R},
            {a(), hostname()},
            begin
                Hostname = eval(R),
                R1 = ?R:add_a(id(?BIG_TIME), P, Hostname),
                M1 = lists:usort([P | ft_hostname:a(Hostname)]),
                M2 = ft_hostname:a(R1),
                ?WHENFAIL(io:format(user, "History: ~p~nHostname: ~p~n"
                                    "Records': ~p~n", [R, Hostname, M2]),
                          M1 =:= M2)
            end).

prop_remove_a() ->
    ?FORALL({R, P}, ?LET(R, hostname(), {R, maybe_oneof(calc_as(R))}),
            begin
                Hostname = eval(R),
                R1 = ?R:remove_a(id(?BIG_TIME), P, Hostname),
                M1 = ft_hostname:a(Hostname) -- [P],
                M2 = ft_hostname:a(R1),
                ?WHENFAIL(io:format(user, "History: ~p~nHostname: ~p~n"
                                    "Records': ~p~n", [R, Hostname, M2]),
                          M1 =:= M2)
            end).
