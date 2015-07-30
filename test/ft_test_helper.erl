-module(ft_test_helper).

-compile(export_all).

id(I) ->
    {I+1, eqc}.

timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000  + Sec) * 1000000 + Micro.

id() ->
    id(timestamp()).
