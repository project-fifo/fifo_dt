-record(hypervisor_0_1_0,
        {
          characteristics = old_map:new()    :: old_map:map(),
          etherstubs      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          host            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          metadata        = old_map:new()    :: old_map:map(),
          alias           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          path            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          pools           = old_map:new()    :: old_map:map(),
          port            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          resources       = old_map:new()    :: old_map:map(),
          services        = old_map:new()    :: old_map:map(),
          sysinfo         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          uuid            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          virtualisation  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-define(HYPERVISOR, hypervisor_0_1_0).
