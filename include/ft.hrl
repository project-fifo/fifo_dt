-record(hypervisor_0_1_0,
        {
          characteristics = riak_dt_map:new()    :: riak_dt_map:map(),
          etherstubs      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:orswot(),
          host            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          metadata        = riak_dt_map:new()    :: riak_dt_map:map(),
          alias           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          path            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:orswot(),
          pools           = riak_dt_map:new()    :: riak_dt_map:map(),
          port            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          resources       = riak_dt_map:new()    :: riak_dt_map:map(),
          services        = riak_dt_map:new()    :: riak_dt_map:map(),
          sysinfo         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          uuid            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          virtualisation  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-record(grouping_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          groupings      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          elements       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

-record(dataset_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map(),

          dataset        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          description    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          disk_driver    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          homepage       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          image_size     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          nic_driver     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          os             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          users          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()

        }).

-record(network_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          ipranges       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

-record(package_0_1_0,
        {
          uuid            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name            = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          blocksize       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          compression     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          cpu_cap         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          cpu_shares      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          max_swap        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          quota           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          ram             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          zfs_io_priority = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          requirements    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),

          metadata        = riak_dt_map:new()    :: riak_dt_map:map()

        }).

-record(dtrace_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          script         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          config         = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

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
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
        }).

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


-define(PACKAGE, package_0_1_0).
-define(DTRACE, dtrace_0_1_0).
-define(IPRANGE, iprange_0_1_0).
-define(VM, vm_0_1_0).
-define(HYPERVISOR, hypervisor_0_1_0).
-define(GROUPING, grouping_0_1_0).
-define(DATASET, dataset_0_1_0).
-define(NETWORK, network_0_1_0).
