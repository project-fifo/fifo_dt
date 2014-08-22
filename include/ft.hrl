-define(NEW_LWW(V, T), riak_dt_lwwreg:update(
                         {assign, V, T}, none,
                         riak_dt_lwwreg:new())).

-define(CONVERT_VORSET(S),
        riak_dt_orswot:update(
          {add_all, vorsetg:value(S)}, none,
          riak_dt_orswot:new())).

-type obj_val() :: term().
-record(sniffle_obj, {val    :: obj_val(),
                      vclock :: vclock:vclock()}).

-record(snarl_obj, {val    :: obj_val(),
                    vclock :: vclock:vclock()}).

-record(ft_obj, {val    :: obj_val(),
                 vclock = vclock:fresh() :: vclock:vclock()}).


-type sniffle_obj() :: #sniffle_obj{} | not_found.
-type snarl_obj() :: #ft_obj{} | not_found.
-type ft_obj() :: #ft_obj{} | not_found.
-type any_obj() :: ft_obj() | snarl_obj() | sniffle_obj().

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


-define(DTRACE, dtrace_0_1_0).
-define(IPRANGE, iprange_0_1_0).
-define(VM, vm_0_1_0).
-define(HYPERVISOR, hypervisor_0_1_0).
-define(GROUPING, grouping_0_1_0).
-define(DATASET, dataset_0_1_0).
-define(NETWORK, network_0_1_0).


%% SNARL

-record(user_0_1_0, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          password         :: vlwwregister:vlwwregister(),
          permissions      :: vorsetg:vorsetg(),
          roles            :: vorsetg:vorsetg(),
          metadata
         }).

-record(user_0_1_1, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          password         :: vlwwregister:vlwwregister(),
          permissions      :: vorsetg:vorsetg(),
          roles            :: vorsetg:vorsetg(),
          ssh_keys         :: vorsetg:vorsetg(),
          metadata
         }).

-record(user_0_1_2, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          password         :: vlwwregister:vlwwregister(),
          active_org       :: vlwwregister:vlwwregister(),
          permissions      :: vorsetg:vorsetg(),
          roles            :: vorsetg:vorsetg(),
          ssh_keys         :: vorsetg:vorsetg(),
          orgs             :: vorsetg:vorsetg(),
          metadata
         }).

-record(user_0_1_3, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          password    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          active_org  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          roles       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ssh_keys    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          orgs        = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(user_0_1_4, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          password    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          active_org  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          roles       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ssh_keys    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          orgs        = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          yubikeys    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(user_0_1_5, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          password    = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          active_org  = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          roles       = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ssh_keys    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          orgs        = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          yubikeys    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(group_0_1_0, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          permissions      :: vorsetg:vorsetg(),
          metadata
         }).

-record(group_0_1_1, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).


-record(role_0_1_0, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(organisation_0_1_0, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          triggers         :: vorsetg:vorsetg(),
          metadata
         }).

-record(organisation_0_1_1, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(organisation_0_1_2, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(organisation_0_1_3, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-define(USER, user_0_1_5).
-define(ROLE, role_0_1_0).
-define(ORG, organisation_0_1_3).
