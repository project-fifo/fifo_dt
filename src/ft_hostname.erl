%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_hostname).

-include("ft_role.hrl").
-include("ft_helper.hrl").

-export([
         new/1,
         load/2,
         merge/2, empty/1,
         a/1, add_a/3, remove_a/3,
         is_a/1
        ]).

-ignore_xref([
              new/1,
              load/2,
              merge/2,
              a/1, add_a/3, remove_a/3
             ]).

-type role() :: #{
            type        => ?TYPE,
            version     => ?VERSION,
            a           => riak_dt_orswot:orswot()
         }.
-export_type([role/0]).

?IS_A.

new({_T, _ID}) ->
    #{
     type     => ?TYPE,
     version  => ?VERSION,
     a        => riak_dt_orswot:new()
    }.

%%-spec load({integer(), atom()}, any_role()) -> role().

load(_, #{type := ?TYPE, version := ?VERSION} = Hostname) ->
    Hostname.

-spec merge(role(), role()) -> role().

merge(R = #{
        type := ?TYPE,
        a    := A1
       },
      #{
         type := ?TYPE,
         a    := A2
       }) ->
    R#{
      a => riak_dt_orswot:merge(A1, A2)
     }.

empty(H) ->
    L = a(H),
    L =:= [].

?SET_GET(a).
?SET_ADD(add_a, a).
?SET_REM(remove_a, a).
