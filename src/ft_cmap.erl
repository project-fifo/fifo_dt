-module(ft_cmap).

-export([new/0, value/1, get/2, get/3, merge/2, inc/4, dec/4, remove/3, ctx/1]).

-define(T,riak_dt_emcntr).
-spec new() -> riak_dt_map:map().
new() ->
    riak_dt_map:new().

-spec merge(riak_dt_map:map(), riak_dt_map:map()) -> riak_dt_map:map().
merge(A, B) ->
    riak_dt_map:merge(A, B).

value(Map) ->
    V1 = riak_dt_map:value(Map),
    [{K, V} || {{K,?T},V} <- V1].

get(Key, Map) ->
    V1 = riak_dt_map:value(Map),
    case [{K, V} || {{K,?T},V} <- V1, K =:= Key] of
        [] ->
            undefined;
        [{_, V}] ->
            {ok, V}
    end.

get(Key, Dflt, Map) ->
    case ft_cmap:get(Key, Map) of
        {ok, V} ->
            V;
        _ ->
            Dflt
    end.

inc(Actor, Key, Value, Map) ->
    Update = {update, [{update, {Key, ?T}, {increment, Value}}]},
    riak_dt_map:update(Update, Actor, Map).

dec(Actor, Key, Value, Map) ->
    Update = {update, [{update, {Key, ?T}, {decrement, Value}}]},
    riak_dt_map:update(Update, Actor, Map).

remove(Actor, Key, Map) ->
    Update = {update, [{remove, {Key, ?T}}]},
    case riak_dt_map:update(Update, Actor, Map) of
        {error, {precondition, {not_present,{Key, ?T}}}} ->
            {ok, Map};
        Res ->
            Res
    end.

ctx(Map) ->
    riak_dt_map:precondition_context(Map).
