-module(ft_test_helper).

-compile(export_all).

id(I) ->
    {I+1, eqc}.

timestamp() ->
    erlang:system_time().

id() ->
    id(timestamp()).
