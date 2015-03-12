-record(grouping_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          elements       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-record(grouping_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          elements       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          config         = old_map:new()    :: old_map:map(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-define(GROUPING, grouping_0).
