-record(iprange_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          network        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          netmask        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          gateway        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          tag            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          vlan           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          free           = old_set:new() :: old_set:orswot(),
          used           = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map()
        }).

-record(iprange_0,
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
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map()
        }).

-define(IPRANGE, iprange_0).
