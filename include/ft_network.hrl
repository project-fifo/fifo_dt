-record(network_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          ipranges       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

-define(NETWORK, network_0_1_0).
