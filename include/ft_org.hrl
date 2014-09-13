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

-record(organisation_0_1_4, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = riak_dt_map:new()    :: riak_dt_map:map(),
          resources      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          s3_id          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          s3_key         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          default_bucket = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          buckets        = riak_dt_map:new()    :: riak_dt_map:map(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-record(organisation_0_1_5, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = riak_dt_map:new()    :: riak_dt_map:map(),
          resources      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-define(ORG, organisation_0_1_5).
