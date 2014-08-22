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

-define(PACKAGE, package_0_1_0).

