-module(ft_test_helper).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include("ft.hrl").

-compile(export_all).

id(I) ->
    {I, eqc}.

id() ->
    {Mega, Sec, Micro} = now(),
    Now = (Mega * 1000000  + Sec) * 1000000 + Micro,
    id(Now).

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

-endif.
