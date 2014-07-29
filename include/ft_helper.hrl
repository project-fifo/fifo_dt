-define(G(N, F),
        getter(O, N) ->
               S0 = ft_obj:val(O),
               F(S0)).

-define(G(E),
        E(H) -> riak_dt_lwwreg:value(H#?OBJ.E)).

-define(S(E),
        E({T, _ID}, V, H) ->
               {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?OBJ.E),
               H#?OBJ{E = V1}).

-define(S(N, F),
        set(TID, N, Value, D) ->
               F(TID, Value, D)).
