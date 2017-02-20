-record(dtrace_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          script         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          config         = old_map:new()    :: old_map:old_map(),
          metadata       = old_map:new()    :: old_map:old_map()
        }).

-record(dtrace_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          script         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          config         = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map()
        }).

-define(TYPE, dtrace).
-define(VERSION, 1).
