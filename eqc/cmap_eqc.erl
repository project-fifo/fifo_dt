-module(cmap_eqc).

-import(ft_test_helper, [id/1, permission/0, maybe_oneof/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).
-define(M, ?MODULE).

cmap() ->
    ?SIZED(Size, cmap(Size)).

pos_int() ->
    ?SUCHTHAT(I, int(), I > 0).

cmap(Size) ->
    ?LAZY(
       oneof(
         [ft_cmap:new() || Size == 0] ++
             [?LETSHRINK(
                 [M], [cmap(Size - 1)],
                 ?LET({F, K, V}, {oneof([inc, dec]), non_blank_string(), pos_int()},
                      ?MODULE:F(K, V, M))) || Size > 0])).
cmaps() ->
    ?SIZED(Size, cmaps(Size)).

cmaps(Size) ->
    ?LAZY(
       oneof(
         [{call, ?M, new3, []} || Size == 0] ++
             [?LETSHRINK(
                 [M], [cmaps(Size - 1)],
                 oneof([
                        {call, ?M, op_left,
                         [oneof([inc, dec]), non_blank_string(), pos_int(), M]},
                        {call, ?M, op_right,
                         [oneof([inc, dec]), non_blank_string(), pos_int(), M]},
                        {call, ?M, op_both,
                         [oneof([inc, dec]), non_blank_string(), pos_int(), M]}]))
              || Size > 0])).

new3()  ->
    {ft_cmap:new(), ft_cmap:new(), ft_cmap:new()}.
inc(K, V, M) ->
    {ok, M1} = ft_cmap:inc(eqc, K, V, M),
    M1.

dec(K, V, M) ->
    {ok, M1} = ft_cmap:dec(eqc, K, V, M),
    M1.

op_left(Op, K, V, {L, R, B}) ->
    {ok, L1} = ft_cmap:Op(eqcl, K, V, L),
    B1 = ft_cmap:merge(B, L1),
    {L1, R, B1}.

op_right(Op, K, V, {L, R, B}) ->
    {ok, R1} = ft_cmap:Op(eqcr, K, V, R),
    B1 = ft_cmap:merge(B, R1),
    {L, R1, B1}.

op_both(Op, K, V, {L, R, B}) ->
    {ok, L1} = ft_cmap:Op(eqc, K, V, L),
    {ok, R1} = ft_cmap:Op(eqc, K, V, R),
    B1 = ft_cmap:merge(B, L1),
    B2 = ft_cmap:merge(B1, R1),
    {L1, R1, B2}.


prop_cmap() ->
    ?FORALL(A, cmap(),
            begin
                B = ft_cmap:merge(A, A),
                ?WHENFAIL(io:format("~p =/= ~p~n",
                                    [A, B]),
                          A =:= B)
            end).

prop_cmap_inc() ->
    ?FORALL({K, V, A}, {non_blank_string(), int(), cmap()},
            begin
                {ok, B} = ft_cmap:inc(eqc, K, V, A),
                VA = ft_cmap:get(K, 0, A),
                VB = ft_cmap:get(K, 0, B),
                ?WHENFAIL(io:format("~p =/= ~p~n",
                                    [(VA + K), VB]),
                          (VA + V) =:= VB)
            end).

prop_cmap_dec() ->
    ?FORALL({K, V, A}, {non_blank_string(), int(), cmap()},
            begin
                {ok, B} = ft_cmap:dec(eqc, K, V, A),
                VA = ft_cmap:get(K, 0, A),
                VB = ft_cmap:get(K, 0, B),
                ?WHENFAIL(io:format("~p =/= ~p~n",
                                    [(VA - K), VB]),
                          (VA - V) =:= VB)
            end).

prop_cmap_remove_len() ->
    ?FORALL({K, A}, {non_blank_string(), cmap()},
            begin
                VsA = ft_cmap:value(A),
                {ok, B} = ft_cmap:remove(eqc, K, A),
                VsB = ft_cmap:value(B),
                ?WHENFAIL(io:format("~p < ~p~n",
                                    [VsA, VsB]),
                          length(VsA) >= length(VsB))
            end).

prop_cmap_remove() ->
    ?FORALL({K, A}, {non_blank_string(), cmap()},
            begin
                {ok, B} = ft_cmap:remove(eqc, K, A),
                ?WHENFAIL(io:format("~p~n", [B]),
                          ft_cmap:get(K, B) =:= undefined)
            end).


prop_cmap_merge() ->
    ?FORALL(M, cmaps(),
            begin
                {L, R, B} = eval(M),
                C = ft_cmap:merge(L, R),
                VB = ft_cmap:value(B),
                VC = ft_cmap:value(C),
                ?WHENFAIL(io:format("~p =/= ~p <- (~p + ~p)~n"
                                    "~p~n=/=~n~p~n<-~n~p~n+~n~p~n",
                                    [VB, VC, ft_cmap:value(L), ft_cmap:value(R),
                                     B, C, L, R]),
                          VB =:= VC)
            end).


