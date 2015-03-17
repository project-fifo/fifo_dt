%% -------------------------------------------------------------------
%%
%% riak_dt_map: OR-Set schema based multi CRDT container
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc a multi CRDT holder. A Document-ish thing. Consists of two
%% elements, a Schema and a value list. The schema is an OR-Set of
%% {name, type} tuples that identify a field and it's type (type must
%% be a CRDT module, yes, even this one.)  The value list is a dict of
%% {name, type} -> CRDT value mappings. It uses a tombstoneless OR-Set
%% for keys, so we drop deleted values at once.
%%
%%
%% @end

-module(old_map).


%% API
-export([new/0, value/1, value/2, update/3, merge/2,
         equal/2, to_binary/1, from_binary/1, precondition_context/1, stats/1, stat/2]).

-export_type([old_map/0, binary_map/0, map_op/0]).

-type binary_map() :: binary(). %% A binary that from_binary/1 will accept
-type old_map() :: {riak_dt_vclock:vclock(), valuelist()}.
-type field() :: {Name::term(), Type::crdt_mod()}.
-type crdt_mod() :: riak_dt_pncounter | riak_dt_lwwreg |
                    riak_dt_od_flag |
                    old_map | riak_dt_orswot.
-type valuelist() :: [{field(), entry()}].
-type entry() :: {minimal_clock(), crdt()}.

%% a minimal clock is just the dots for the element, each dot being an
%% actor and event counter for when the element was added.
-type minimal_clock() :: [dot()].
-type dot() :: {riak_dt:actor(), Count::pos_integer()}.

-type crdt()  ::  riak_dt_pncounter:pncounter() | riak_dt_od_flag:od_flag() |
                  riak_dt_lwwreg:lwwreg() |
                  riak_dt_orswot:orswot() |
                  old_map:old_map().

-type map_op() :: {update, [map_field_update() | map_field_op()]}.

-type map_field_op() :: {add, field()} | {remove, field()}.
-type map_field_update() :: {update, field(), crdt_op()}.

-type crdt_op() :: riak_dt_pncounter:pncounter_op() |
                   riak_dt_lwwreg:lwwreg_op() |
                   riak_dt_orswot:orswot_op() | riak_dt_od_flag:od_flag_op() |
                   old_map:map_op().

-type map_q() :: size | {get, field()} | {get_crdt, field()} |
                   keyset | {contains, field()}.

-type values() :: [value()].
-type value() :: {field(), old_map:values() | integer() | [term()] | boolean() | term()}.
-type precondition_error() :: {error, {precondition, {not_present, field()}}}.

%% @doc Create a new, empty Map.
-spec new() -> old_map().
new() ->
    {riak_dt_vclock:fresh(), orddict:new()}.

%% @doc get the current set of values for this Map
-spec value(old_map()) -> values().
value({_Clock, Values}) ->
    orddict:fold(fun({_Name, riak_dt_map}=Key, {_Dots, Value}, Acc) ->
                         [{Key, old_map:value(Value)} | Acc];
                    ({_Name, Mod}=Key, {_Dots, Value}, Acc) ->
                         [{Key, Mod:value(Value)} | Acc]
                 end,
                 [],
                 Values).

%% @doc execute the given `map_q()' against the given
%% `old_map()'.
%% @TODO add a query for getting a subset of fields
%% including submap fields (Maybe kvc like?)
-spec value(map_q(), old_map()) -> term().
value(size, {_Clock, Values}) ->
    length(keys(Values));
value({get, {_Name, riak_dt_map}=Field}, Map) ->
    case value({get_crdt, Field}, Map) of
        error -> error;
        CRDT -> old_map:value(CRDT)
    end;
value({get, {_Name, Mod}=Field}, Map) ->
    case value({get_crdt, Field}, Map) of
        error -> error;
        CRDT -> Mod:value(CRDT)
    end;
value({get_crdt, {_Name, riak_dt_map}=Field}, {_Clock, Values}) ->
    get_crdt(orddict:find(Field, Values), old_map);
value({get_crdt, {_Name, Mod}=Field}, {_Clock, Values}) ->
    get_crdt(orddict:find(Field, Values), Mod);
value(keyset, {_Clock, Values}) ->
    keys(Values);
value({contains, Field}, {_Clock, Values}) ->
    lists:member(Field, keys(Values)).

%% @private
-spec get_crdt({ok, entry()} | error, crdt_mod()) -> crdt() | error.
get_crdt(Entry, Mod) ->
    get_crdt(Entry, Mod, false).

%% @private
-spec get_crdt({ok, entry()} | error, crdt_mod(), boolean()) ->
                      crdt() | error.
get_crdt({ok, {_Dot, Value}}, _Mod, _) ->
    Value;
get_crdt(error, _Mod, false) ->
    error;
get_crdt(error, Mod, true) ->
    Mod:new().

-spec keys(orddict:orddict()) -> [field()] | [].
keys(Values) ->
    orddict:fetch_keys(Values).

%% @Doc update the `old_map()' or a field in the `map()' by executing
%% the `map_op()'. `Ops' is a list of one or more of the following
%% ops:
%%
%% `{update, field(), Op} where `Op' is a valid update operation for a
%% CRDT of type `Mod' from the `Key' pair `{Name, Mod}' If there is no
%% local value for `Key' a new CRDT is created, the operation applied
%% and the result inserted otherwise, the operation is applied to the
%% local value.
%%
%% {add, `field()'}' where field is `{name, type}' results in `field'
%% being added to the Map, and a new crdt of `type' being its value.
%% `{remove, `field()'}' where field is `{name, type}', results in the
%% crdt at `field' and the key and value being removed. A concurrent
%% `update' | `add' will win over a remove.
%%
%% Either all of `Ops' are performed successfully, or none are.
%%
%% @see riak_dt_orswot for more details.

-spec update(map_op(), riak_dt:actor(), old_map()) -> {ok, old_map()} | precondition_error().
update({update, Ops}, Actor, {Clock, Values}) ->
    apply_ops(Ops, Actor, Clock, Values).

%% @private
-spec apply_ops([map_field_update() | map_field_op()], riak_dt:actor(),
                riak_dt_vclock:vclock(), orddict:orddict()) ->
                       {ok, old_map()} | precondition_error().
apply_ops([], _Actor, Clock, Values) ->
    {ok, {Clock, Values}};
apply_ops([{update, {_Name, Mod}=Field, Op} | Rest], Actor, Clock, Values) ->
    InitialValue = get_crdt(orddict:find(Field, Values), Mod, true),
    case Mod:update(Op, Actor, InitialValue) of
        {ok, NewValue} ->
            NewClock = riak_dt_vclock:increment(Actor, Clock),
            NewDot = {Actor, riak_dt_vclock:get_counter(Actor, NewClock)},
            NewValues = orddict:store(Field, {[NewDot], NewValue}, Values),
            apply_ops(Rest, Actor, NewClock, NewValues);
        Error ->
            Error
    end;
apply_ops([{remove, Field} | Rest], Actor, Clock, Values) ->
    case orddict:is_key(Field, Values) of
        true->
            apply_ops(Rest, Actor, Clock, orddict:erase(Field, Values));
        false -> {error, {precondition, {not_present, Field}}}
    end;
apply_ops([{add, {_Name, Mod}=Field} | Rest], Actor, Clock, Values) ->
    InitialValue = get_crdt(orddict:find(Field, Values), Mod, true),
    NewClock = riak_dt_vclock:increment(Actor, Clock),
    Dot = {Actor, riak_dt_vclock:get_counter(Actor, NewClock)},
    NewValues = orddict:store(Field, {[Dot], InitialValue}, Values),
    apply_ops(Rest, Actor, NewClock, NewValues).

%% @Doc merge two `old_map()'s.  and then a pairwise merge on all values
%% in the value list.  This is the LUB function.
-spec merge(old_map(), old_map()) -> old_map().
merge({LHSClock, LHSEntries}, {RHSClock, RHSEntries}) ->
    Clock = riak_dt_vclock:merge([LHSClock, RHSClock]),
    %% If an element is in both dicts, merge it. If it occurs in one,
    %% then see if its dots are dominated by the others whole set
    %% clock. If so, then drop it, if not, keep it.
    LHSFields = sets:from_list(orddict:fetch_keys(LHSEntries)),
    RHSFields = sets:from_list(orddict:fetch_keys(RHSEntries)),
    CommonFields = sets:intersection(LHSFields, RHSFields),
    LHSUnique = sets:subtract(LHSFields, CommonFields),
    RHSUnique = sets:subtract(RHSFields, CommonFields),

    Entries00 = merge_common_fields(CommonFields, LHSEntries, RHSEntries),
    Entries0 = merge_disjoint_fields(LHSUnique, LHSEntries, RHSClock, Entries00),
    Entries = merge_disjoint_fields(RHSUnique, RHSEntries, LHSClock, Entries0),

    {Clock, Entries}.

%% @doc check if each element in `Entries' should be in the merged
%% set.
merge_disjoint_fields(Fields, Entries, SetClock, Accumulator) ->
    sets:fold(fun(Field, Acc) ->
                      {Dots, Value} = orddict:fetch(Field, Entries),
                      case riak_dt_vclock:descends(SetClock, Dots) of
                          false ->
                              %% Optimise the set of stored dots to
                              %% include only those unseen
                              NewDots = riak_dt_vclock:subtract_dots(Dots, SetClock),
                              orddict:store(Field, {NewDots, Value}, Acc);
                          true ->
                              Acc
                      end
              end,
              Accumulator,
              Fields).

%% @doc merges the minimal clocks and values for the common entries in
%% both sets.
merge_common_fields(CommonFields, Entries1, Entries2) ->
    sets:fold(fun({_Name, riak_dt_map}=Field, Acc) ->
                      {Dots1, V1} = orddict:fetch(Field, Entries1),
                      {Dots2, V2} = orddict:fetch(Field, Entries2),
                      Dots = riak_dt_vclock:merge([Dots1, Dots2]),
                      V = old_map:merge(V1, V2),
                      orddict:store(Field, {Dots, V}, Acc);
                 ({_Name, Mod}=Field, Acc) ->
                      {Dots1, V1} = orddict:fetch(Field, Entries1),
                      {Dots2, V2} = orddict:fetch(Field, Entries2),
                      Dots = riak_dt_vclock:merge([Dots1, Dots2]),
                      V = Mod:merge(V1, V2),
                      orddict:store(Field, {Dots, V}, Acc)
              end,
              orddict:new(),
              CommonFields).

%% @Doc compare two `old_map()'s for equality of structure Both schemas
%% and value list must be equal. Performs a pariwise equals for all
%% values in the value lists
-spec equal(old_map(), old_map()) -> boolean().
equal({Clock1, Values1}, {Clock2, Values2}) ->
    riak_dt_vclock:equal(Clock1, Clock2) andalso
        keys(Values1) == keys(Values2) andalso
        pairwise_equals(Values1, Values2).

%% @Private Note, only called when we know that 2 sets of fields are
%% equal. Both dicts therefore have the same set of keys.
-spec pairwise_equals(valuelist(), valuelist()) -> boolean().
pairwise_equals(Values1, Values2) ->
    short_circuit_equals(orddict:to_list(Values1), Values2).

%% @Private
%% Compare each value. Return false as soon as any pair are not equal.
-spec short_circuit_equals(valuelist(), valuelist()) -> boolean().
short_circuit_equals([], _Values2) ->
    true;
short_circuit_equals([{{_Name, Mod}=Field, {Dot1,Val1}} | Rest], Values2) ->
    {Dot2, Val2} = orddict:fetch(Field, Values2),
    case {riak_dt_vclock:equal(Dot1, Dot2), Mod:equal(Val1, Val2)} of
        {true, true} ->
            short_circuit_equals(Rest, Values2);
        _ ->
            false
    end.

%% @Doc a "fragment" of the Map that can be used for precondition
%% operations. The schema is just the active Key Set The values are
%% just those values that are present We use either the values
%% precondition_context or the whole CRDT
-spec precondition_context(old_map()) -> old_map().
precondition_context(Map) ->
    Map.

-spec stats(old_map()) -> [{atom(), integer()}].
stats(Map) ->
    [ {S, stat(S, Map)} || S <- [actor_count, field_count, max_dot_length]].

-spec stat(atom(), old_map()) -> number() | undefined.
stat(actor_count, {Clock, _}) ->
    length(Clock);
stat(field_count, {_, Fields}) ->
    length(Fields);
stat(max_dot_length, {_, Fields}) ->
    orddict:fold(fun(_K, {Dots, _}, Acc) ->
                         max(length(Dots), Acc)
                 end, 0, Fields);
stat(_,_) -> undefined.

-include_lib("riak_dt/include/riak_dt_tags.hrl").
-define(TAG, ?DT_MAP_TAG).
-define(V1_VERS, 1).

%% @doc returns a binary representation of the provided `old_map()'. The
%% resulting binary is tagged and versioned for ease of future
%% upgrade. Calling `from_binary/1' with the result of this function
%% will return the original map.  Use the application env var
%% `binary_compression' to turn t2b compression on (`true') and off
%% (`false')
%%
%% @see `from_binary/1'
-spec to_binary(old_map()) -> binary_map().
to_binary(Map) ->
    Opts = case application:get_env(riak_dt, binary_compression, true) of
               true -> [compressed];
               N when N >= 0, N =< 9 -> [{compressed, N}];
               _ -> []
           end,
    <<?TAG:8/integer, ?V1_VERS:8/integer, (term_to_binary(Map, Opts))/binary>>.

%% @doc When the argument is a `binary_map()' produced by
%% `to_binary/1' will return the original `old_map()'.
%%
%% @see `to_binary/1'
-spec from_binary(binary_map()) -> old_map().
from_binary(<<?TAG:8/integer, ?V1_VERS:8/integer, B/binary>>) ->
    binary_to_term(B).
