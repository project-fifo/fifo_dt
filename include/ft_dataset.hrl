-record(dataset_0_1_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map(),

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

-record(dataset_0_1_1,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map(),

          description    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          disk_driver    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          homepage       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          image_size     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          nic_driver     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          os             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          users          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          sha1           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()

        }).

-record(dataset_0,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          zone_type      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map(),

          description    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          disk_driver    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          homepage       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          image_size     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          networks       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          nic_driver     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          os             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          users          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          sha1           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          kernel_version = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-record(dataset_1,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          zone_type      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = old_set:new() :: old_set:orswot(),
          networks       = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map(),

          description    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          disk_driver    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          homepage       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          image_size     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          nic_driver     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          os             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          users          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          sha1           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          kernel_version = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-record(dataset_2,
        {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          zone_type      = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),

          status         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          imported       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          requirements   = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          networks       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),

          description    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          disk_driver    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          homepage       = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          image_size     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          nic_driver     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          os             = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          users          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          sha1           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          version        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          kernel_version = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg()
        }).

-define(TYPE, dataset).
-define(VERSION, 1).
