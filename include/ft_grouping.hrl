-record(grouping_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = old_set:new() :: old_set:orswot(),
          elements       = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-record(grouping_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = old_set:new() :: old_set:orswot(),
          elements       = old_set:new() :: old_set:orswot(),
          config         = old_map:new()    :: old_map:map(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-record(grouping_1,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          elements       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          config         = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

-define(GROUPING, grouping_1).
