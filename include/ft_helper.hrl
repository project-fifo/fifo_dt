-define(G(N, F),
        getter(O, N) ->
               S0 = ft_obj:val(O),
               F(S0)).

-define(G_JSX,
        getter(K, O) ->
               lager:warning("[] Accessing unsupported getter ~p,"
                             " reverting to jsxd.", [?MODULE, K]),
               V = ft_obj:value(O),
               jsxd:get(K, to_json(V))).

-define(G(E),
        E(H) -> riak_dt_lwwreg:value(H#?OBJ.E)).



-define(S(E),
        E({T, _ID}, V, H) ->
               {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?OBJ.E),
               H#?OBJ{E = V1}).

-define(S(N, F),
        set(TID, N, Value, D) ->
               F(TID, Value, D)).
