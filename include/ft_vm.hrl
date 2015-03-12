-record(vm_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          alias          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          owner          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          dataset        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          package        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          hypervisor     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          network_map    = old_map:new()    :: old_map:map(),

          config         = old_map:new()    :: old_map:map(),
          info           = old_map:new()    :: old_map:map(),
          services       = old_map:new()    :: old_map:map(),
          backups        = old_map:new()    :: old_map:map(),
          snapshots      = old_map:new()    :: old_map:map(),

          logs           = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          state          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          metadata       = old_map:new()    :: old_map:map()

        }).


-record(vm_0_1_1,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          alias          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          owner          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          dataset        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          package        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          hypervisor     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          network_map    = old_map:new()    :: old_map:map(),

          config         = old_map:new()    :: old_map:map(),
          info           = old_map:new()    :: old_map:map(),
          services       = old_map:new()    :: old_map:map(),
          backups        = old_map:new()    :: old_map:map(),
          snapshots      = old_map:new()    :: old_map:map(),

          logs           = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          state          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          fw_rules       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = old_map:new()    :: old_map:map()

        }).


-define(VM, vm_0_1_1).
