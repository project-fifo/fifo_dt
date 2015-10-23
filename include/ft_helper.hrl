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

-ifdef(OBJ).
-define(IS_A,
        is_a(#?OBJ{}) -> true;
        is_a(_) -> false).
-else.
-define(IS_A,
        is_a(#{type := ?TYPE}) -> true;
            is_a(_) -> false).
-endif.

%% Registers
-define(REG_GET(Field),
        Field(#{type := ?TYPE, Field := V}) -> riak_dt_lwwreg:value(V)).

-define(REG_SET_BODY(Field),
        {ok, Reg1} = riak_dt_lwwreg:update({assign, V, T}, none, Reg0),
        O#{Field := Reg1}).

-define(REG_SET(Field),
        Field({T, _ID}, V, O = #{type := ?TYPE, Field := Reg0}) ->
               ?REG_SET_BODY(Field)).

-define(REG_SET_BIN(Field),
        Field({T, _ID}, V, O = #{type := ?TYPE, Field := Reg0})
        when is_binary(V) ->
               ?REG_SET_BODY(Field)).

-define(REG_SET_PI(Field),
        Field({T, _ID}, V, O = #{type := ?TYPE, Field := Reg0})
        when is_integer(V), V > 0 ->
               ?REG_SET_BODY(Field)).

%% ORSwot
-define(SET_GET(Name, Field),
        Name(#{type := ?TYPE, Field := V}) -> riak_dt_orswot:value(V)).

-define(SET_GET(Field), ?SET_GET(Field, Field)).

-define(SET_ADD(Name, Field),
        Name({_T, ID}, V, O = #{type := ?TYPE, Field := Vs0}) ->
               {ok, Vs1} = riak_dt_orswot:update({add, V}, ID, Vs0),
               O#{Field := Vs1}).

-define(SET_REM(Name, Field),
        Name({_T, ID}, V, O = #{type := ?TYPE, Field := Vs0}) ->
               case riak_dt_orswot:update({remove, V}, ID, Vs0) of
                   {error,{precondition,{not_present, V}}} ->
                       O;
                   {ok, Vs1} ->
                       O#{Field := Vs1}
               end).
%% Map
-define(MAP_GET(Field),
        Field(#{type := ?TYPE, Field := Map}) ->
          fifo_map:value(Map)).

-define(MAP_SET(Name, Field),
        Name({T, ID}, K, V, O = #{type := ?TYPE , Field := Map}) ->
            {ok, Map1} = fifo_map:set(K, {reg, V}, ID, T, Map),
            O#{Field := Map1}).

-define(MAP_REM(Name, Field),
        Name({_T, ID}, K, O = #{type := ?TYPE , Field := Map}) ->
            {ok, Map1} = fifo_map:remove(K, ID, Map),
            O#{Field := Map1}).

-define(META, ?MAP_GET(metadata)).

-define(SET_META_3,
        set_metadata(ID, [{K, V} | R] , Obj) ->
            set_metadata(ID, R, set_metadata(ID, K, V, Obj));
        set_metadata(_ID, _, Obj) ->
            Obj).

-define(SET_META_4,
        set_metadata({T, ID}, P, V, O) when is_binary(P) ->
            set_metadata({T, ID}, fifo_map:split_path(P), V, O);
        set_metadata({_T, ID}, Attribute, delete,
                     O = #{type := ?TYPE, metadata := Meta}) ->
            {ok, M1} = fifo_map:remove(Attribute, ID, Meta),
            O#{metadata := M1};
        set_metadata({T, ID}, Attribute, Value,
                     O = #{type := ?TYPE, metadata := Meta}) ->
            {ok, M1} = fifo_map:set(Attribute, Value, ID, T, Meta),
            O#{metadata := M1}).
