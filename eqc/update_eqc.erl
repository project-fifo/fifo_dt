-module(update_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).

-define(OM, fifo_old_map).
-define(NM, fifo_map).
-define(OS, old_set).
-define(NS, riak_dt_orswot).

key() ->
    non_blank_string().

terminal() ->
    oneof([int(), ?LET(L, list(int()), lists:usort(L))]).


orddict() ->
    %%?SIZED(Deepth, orddict(Deepth)).
    orddict(3).

orddict(Deepth) ->
    ?SIZED(Size, orddict(Size, Deepth)).

orddict(0, _) ->
    [];

orddict(Size, 0) ->
    ?LAZY(
       ?LETSHRINK(
          [O], [orddict(Size - 1, 0)],
          {call, orddict, store, [key(), terminal(), O]}));

orddict(Size, Deepth) ->
    ?LAZY(
       ?LETSHRINK(
          [O], [orddict(Size - 1, Deepth - 1)],
          {call, orddict, store, [key(), value(), O]})).

set() ->
    ?SIZED(Size, set(Size)).

set(0) ->
    ?OS:new();

set(Size) ->
    ?LAZY(
       ?LETSHRINK(
          [O], [set(Size - 1)],
          {call, ?MODULE, add, [non_blank_string(), O]})).

add(Value, Set) ->
    {ok, Set1} = ?OS:update({add, Value}, eqc, Set),
    Set1.

value() ->
    frequency(
      [{1, orddict()},
       {50, terminal()}]).

prop_from_orddict_old() ->
    ?FORALL(O, orddict(),
            begin
                Dict = eval(O),
                ?WHENFAIL(io:format(user, "History: ~p~nData: ~p~n", [O, Dict]),
                          Dict == ?OM:value(?OM:from_orddict(Dict, test, 1)))
            end).

prop_from_orddict() ->
    ?FORALL(O, orddict(),
            begin
                Dict = eval(O),
                ?WHENFAIL(io:format(user, "History: ~p~nData: ~p~n", [O, Dict]),
                          Dict == ?NM:value(?NM:from_orddict(Dict, test, 1)))
            end).

prop_convert_map() ->
    ?FORALL(O, orddict(),
            begin
                Dict = eval(O),
                Old = ?OM:from_orddict(Dict, test, 1),
                ?WHENFAIL(io:format(user, "History: ~p~nData: ~p~n", [O, Dict]),
                          Dict == ?NM:value(fifo_dt:update_map(Old)))
            end).

prop_convert_set() ->
    ?FORALL(S, set(),
            begin
                Old = eval(S),
                New = fifo_dt:update_set(Old),
                OldV = ?OS:value(Old),
                NewV = ?NS:value(New),
                ?WHENFAIL(io:format(user, "History: ~p~nOld: ~p~nNew: ~p~n",
                                    [S, OldV, NewV]),
                          OldV == NewV)
            end).
