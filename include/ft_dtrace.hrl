-record(dtrace_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          script         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          config         = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

-define(DTRACE, dtrace_0_1_0).
