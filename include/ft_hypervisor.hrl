-record(hypervisor_0_1_0,
        {
          characteristics = old_map:new()    :: old_map:old_map(),
          etherstubs      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          host            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          metadata        = old_map:new()    :: old_map:old_map(),
          alias           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          path            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          pools           = old_map:new()    :: old_map:old_map(),
          port            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          resources       = old_map:new()    :: old_map:old_map(),
          services        = old_map:new()    :: old_map:old_map(),
          sysinfo         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          uuid            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          virtualisation  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-record(hypervisor_0,
        {
          characteristics = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          etherstubs      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          host            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          metadata        = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          alias           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          path            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          pools           = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          port            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          resources       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          services        = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          sysinfo         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          uuid            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          virtualisation  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-define(TYPE, hypervisor).
-define(VERSION, 1).
