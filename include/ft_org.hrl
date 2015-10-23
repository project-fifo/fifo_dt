-record(organisation_0_1_0, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          triggers         :: vorsetg:vorsetg(),
          metadata
         }).

-record(organisation_0_1_1, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = old_set:new() :: old_set:orswot(),
          metadata    = old_map:new()    :: old_map:old_map()
         }).

-record(organisation_0_1_2, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = old_map:new()    :: old_map:old_map(),
          metadata    = old_map:new()    :: old_map:old_map()
         }).

-record(organisation_0_1_3, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers    = old_map:new()    :: old_map:old_map(),
          metadata    = old_map:new()    :: old_map:old_map()
         }).

-record(organisation_0_1_4, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = old_map:new()    :: old_map:old_map(),
          resources      = old_set:new() :: old_set:orswot(),
          s3_id          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          s3_key         = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          default_bucket = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          buckets        = old_map:new()    :: old_map:old_map(),
          metadata       = old_map:new()    :: old_map:old_map()
         }).

-record(organisation_0_1_5, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = old_map:new()    :: old_map:old_map(),
          resources      = old_set:new() :: old_set:orswot(),
          metadata       = old_map:new()    :: old_map:old_map()
         }).

-record(organisation_0, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          resources      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map()
         }).

-record(organisation_1, {
          uuid           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name           = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          triggers       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map(),
          resources      = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata       = riak_dt_map:new()    :: riak_dt_map:riak_dt_map()
         }).

-define(TYPE, org).
-define(VERSION, 1).
