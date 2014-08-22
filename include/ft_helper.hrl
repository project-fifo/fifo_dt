-define(NEW_LWW(V, T), riak_dt_lwwreg:update(
                         {assign, V, T}, none,
                         riak_dt_lwwreg:new())).

-define(CONVERT_VORSET(S),
        riak_dt_orswot:update(
          {add_all, vorsetg:value(S)}, none,
          riak_dt_orswot:new())).

-define(G(N, F),
        getter(O, N) ->
               S0 = ft_obj:val(O),
               F(S0)).

-define(G_JSX,
        getter(O, K) ->
               lager:warning("[~s] Accessing unsupported getter ~p,"
                             " reverting to jsxd.", [?MODULE, K]),
               V = ft_obj:val(O),
               jsxd:get(K, <<>>, to_json(V))).

-define(G(E),
        E(H) -> riak_dt_lwwreg:value(H#?OBJ.E)).

-define(S_BODY(E),
        {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, O#?OBJ.E),
        O#?OBJ{E = V1}).

-define(S_BIN(E),
        E({T, _ID}, V, O) when is_binary(V) ->
               ?S_BODY(E)).

-define(S_PI(E),
        E({T, _ID}, V, O) when is_integer(V), V > 0 ->
               ?S_BODY(E)).

-define(S(E),
        E({T, _ID}, V, O) ->
               ?S_BODY(E)).


-define(S(N, F),
        set(TID, N, Value, D) ->
               F(TID, Value, D)).

-define(IS_A,
        is_a(#?OBJ{}) -> true;
        is_a(_) -> false).
