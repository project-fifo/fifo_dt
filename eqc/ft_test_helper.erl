-module(ft_test_helper).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

id(I) ->
    {I+1, eqc}.

timestamp() ->
    erlang:unique_integer().

id() ->
    id(timestamp()).

atom() ->
    elements([a,b,c,undefined]).

not_empty(G) ->
    ?SUCHTHAT(X, G, X /= [] andalso X /= <<>>).

non_blank_string() ->
    not_empty(?LET(X,list(lower_char()), list_to_binary(X))).

%% Generate a lower 7-bit ACSII character that should not cause any problems
%% with utf8 conversion.
lower_char() ->
    %%choose(16#20, 16#7f).
    choose($a, $z).

maybe_oneof(L) ->
    fqc:maybe_oneof(L, non_blank_string()).

metadata_value() ->
    oneof([delete, non_blank_string()]).

metadata_kvs() ->
    ?SUCHTHAT(L, list({non_blank_string(), metadata_value()}), L /= []
              andalso lists:sort([K || {K, _} <- L]) == lists:usort([K || {K, _} <- L])).

permission() ->
    ?SIZED(Size, permission(Size)).

permission(Size) ->
    ?LAZY(oneof([[oneof([<<"...">>, perm_entry()])] || Size == 0] ++
                    [[perm_entry() | permission(Size -1)] || Size > 0])).

perm_entry() ->
    oneof([<<"_">>, non_blank_string()]).

requirement() ->
    oneof([normal_req(), scale_req(), random_req()]).

normal_req() ->
    oneof([flat_req(), set_req()]).

flat_req() ->
    {req_wight(), flat_cond(), non_blank_string(), int()}.

set_req() ->
    {req_wight(), set_cond(), non_blank_string(), list(int())}.

normal_set_req() ->
    ok.

set_cond() ->
    oneof(['superset', 'disjoint', 'subset']).

flat_cond() ->
    oneof(['>=', '>', '=<', '<', '=:=', '=/=', 'element']).

req_wight() ->
    oneof(['must', 'cant', int()]).

scale_req() ->
    ?SUCHTHAT({_, _, Low, High},
              {scale, non_blank_string(), choose(-100, 100), choose(-100, 100)},
              Low =< High).
random_req() ->
    ?SUCHTHAT({_, Low, High},
              {random, choose(-100, 100), choose(-100, 100)},
              Low =< High).
