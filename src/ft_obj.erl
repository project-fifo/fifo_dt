%% @doc A suite of functions that operate on the algebraic data type
%% `ft_obj'.
%%
%% TODO Possibly move type/record defs in there and use accessor funs
%% and opaque types.
%%
%% Taken form https://github.com/Licenser/try-try-try/blob/master/2011/
%% riak-core-conflict-resolution/rts/src/rts_obj.erl
-module(ft_obj).

-export([ancestors/1, children/1, equal/1, equal/2, merge/2, unique/1,
         update/3, update/1, is_a/1, new/0, new/2, needs_update/1]).
-export([val/1, vclock/1]).

-ignore_xref([
              ancestors/1,
              equal/1,
              unique/1,
              vclock/1
             ]).

-type obj_val() :: term().

-record(sniffle_obj, {val    :: obj_val(),
                      vclock :: vclock:vclock()}).

-record(snarl_obj, {val    :: obj_val(),
                    vclock :: vclock:vclock()}).

-record(ft_obj, {val    :: obj_val(),
                 vclock = vclock:fresh() :: vclock:vclock()}).


-type obj() :: #ft_obj{} | not_found.
-type any_obj() :: #snarl_obj{} |
                   #sniffle_obj{} | obj().

-export_type([any_obj/0, obj/0]).

-callback reconcile([term()]) ->
    term().

-spec new() -> obj().
new() ->
    #ft_obj{}.

-spec new(Value::obj_val(), Coordinator::atom()) -> obj().
new(Value, Coordinator) ->
    update(Value, Coordinator, new()).

%% @pure
%%
%% @doc Given a list of `obj()' return a list of all the
%% ancestors.  Ancestors are objects that all the other objects in the
%% list have descent from.
-spec ancestors([any_obj()]) -> [obj()].
ancestors(Objs0) ->
    Objs = [update(O) || O <- Objs0, O /= not_found],
    As = [[O2 || O2 <- Objs,
                 ancestor(O2#ft_obj.vclock,
                          O1#ft_obj.vclock)] || O1 <- Objs],
    unique(lists:flatten(As)).

%% @pure
%%
%% @doc Predicate to determine if `Va' is ancestor of `Vb'.
-spec ancestor(vclock:vclock(), vclock:vclock()) -> boolean().
ancestor(Va, Vb) ->
    vclock:descends(Vb, Va) andalso (vclock:descends(Va, Vb) == false).

%% @pure
%%
%% @doc Given a list of `obj()' return a list of the children
%% objects.  Children are the descendants of all others objects.
-spec children([any_obj() | obj()]) -> [obj()].
children(ObjsIn) ->
    Objs = [update(O) || O <- ObjsIn],
    unique(Objs) -- ancestors(Objs).

%% @pure
%%
%% @doc Predeicate to determine if `ObjA' and `ObjB' are equal.
-spec equal(ObjA::any_obj() | obj(), ObjB::any_obj() | obj()) -> boolean().
equal(A, B) ->
    equal1(update(A), update(B)).

-spec equal1(ObjA::obj(), ObjB::obj()) -> boolean().
equal1(#ft_obj{vclock=A}, #ft_obj{vclock=B}) -> vclock:equal(A, B);
equal1(not_found, not_found) -> true;
equal1(_, _) -> false.

%% @pure
%%
%% @doc Closure around `equal/2' for use with HOFs (damn verbose
%% Erlang).
-spec equal(ObjA::any_obj() | obj()) ->
                   fun((ObjB::any_obj() | obj()) ->boolean()).
equal(ObjA) ->
    ObjA1 = update(ObjA),
    fun(ObjB) -> equal1(ObjA1, update(ObjB)) end.

%% @pure
%%
%% @doc Merge the list of `Objs', calling the appropriate reconcile
%% fun if there are siblings.

-spec merge(atom(), [any_obj() | obj()]) -> obj().
merge(FSM, Objs) ->
    merge1(FSM, [update(O) || O <- Objs]).

-spec merge1(atom(), [obj()]) -> obj().
merge1(FSM, [not_found|_]=Objs) ->
    P = fun(X) -> X == not_found end,
    case lists:all(P, Objs) of
        true -> not_found;
        false -> merge1(FSM, lists:dropwhile(P, Objs))
    end;

merge1(FSM, [#ft_obj{}|_]=Objs) ->
    case children(Objs) of
        [] -> not_found;
        [Child] -> Child;
        Chldrn ->
            Val = FSM:reconcile(lists:map(fun val1/1, Chldrn)),
            MergedVC = vclock:merge(lists:map(fun vclock1/1, Chldrn)),
            make(Val, MergedVC)
    end.

%% @pure
%%
%% @doc Given a list of `Objs' return the list of uniques.
-spec unique([any_obj()]) -> [obj()].
unique(Objs) ->
    F = fun(not_found, Acc) ->
                Acc;
           (Obj, Acc) ->
                Obj1 = update(Obj),
                case lists:any(equal(Obj1), Acc) of
                    true -> Acc;
                    false -> [Obj1|Acc]
                end
        end,
    lists:foldl(F, [], Objs).

%% @pure
%%
%% @doc Given a `Val' update the `Obj'.  The `Updater' is the name of
%% the entity performing the update.
-spec update(obj_val(), node(), any_obj() | obj()) -> obj().
update(Val, Updater, O) ->
    update1(Val, Updater, update(O)).

update1(Val, Updater, #ft_obj{vclock=VClock0}) ->
    VClock = vclock:increment(Updater, VClock0),
    make(Val, VClock).


-spec val(any_obj() | obj()) -> any().
val(O) -> val1(update(O)).

-spec val1(obj()) -> any().
val1(#ft_obj{val=Val}) -> Val;
val1(not_found) -> not_found.

%% @pure
%%
%% @doc Given a vclock type `Obj' retrieve the vclock.
-spec vclock(any_obj() | obj()) -> vclock:vclock().
vclock(O) -> vclock1(update(O)).

-spec vclock1(obj()) -> vclock:vclock().
vclock1(#ft_obj{vclock=VC}) -> VC.

-spec update(any_obj() | obj()) -> obj().

update(#ft_obj{} = O) ->
    O;
update(#snarl_obj{val = V, vclock = C}) ->
    make(V, C);
update(#sniffle_obj{val = V, vclock = C}) ->
    make(V, C);
update(not_found) ->
    not_found.

-spec is_a(any_obj() | obj()) -> boolean().

is_a(#ft_obj{}) ->
    true;
is_a(#snarl_obj{}) ->
    true;
is_a(#sniffle_obj{}) ->
    true;
is_a(_) ->
    false.

-spec needs_update(any_obj() | obj()) -> boolean().

needs_update(#ft_obj{}) ->
    false;
needs_update(#snarl_obj{}) ->
    true;
needs_update(#sniffle_obj{}) ->
    true.


make(V, C) ->
    #ft_obj{val = V, vclock = C}.
