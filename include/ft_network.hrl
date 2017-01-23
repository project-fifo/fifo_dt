-record(network_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          ipranges       = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map()
        }).

-record(network_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          ipranges       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map()
        }).

-define(TYPE, network).
-define(VERSION, 0).
