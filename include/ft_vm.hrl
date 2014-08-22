-record(vm_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          alias          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          owner          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          dataset        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          package        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          hypervisor     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          network_map    = riak_dt_map:new()    :: riak_dt_map:map(),

          config         = riak_dt_map:new()    :: riak_dt_map:map(),
          info           = riak_dt_map:new()    :: riak_dt_map:map(),
          services       = riak_dt_map:new()    :: riak_dt_map:map(),
          backups        = riak_dt_map:new()    :: riak_dt_map:map(),
          snapshots      = riak_dt_map:new()    :: riak_dt_map:map(),

          logs           = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          state          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          metadata       = riak_dt_map:new()    :: riak_dt_map:map()

        }).


-define(VM, vm_0_1_0).
