-record(dtrace_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          script         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          config         = old_map:new()    :: old_map:map(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-define(DTRACE, dtrace_0_1_0).
