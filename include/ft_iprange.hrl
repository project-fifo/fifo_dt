-record(iprange_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          network        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          netmask        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          gateway        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          tag            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          vlan           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          free           = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          used           = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = old_map:new()    :: old_map:map()
        }).

-define(IPRANGE, iprange_0_1_0).
