-record(group_0_1_0, {
          uuid             :: vlwwregister:vlwwregister(),
          name             :: vlwwregister:vlwwregister(),
          permissions      :: vorsetg:vorsetg(),
          metadata
         }).

-record(group_0_1_1, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = old_set:new() :: old_set:orswot(),
          metadata    = old_map:new()    :: old_map:map()
         }).

-record(role_0_1_0, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = old_set:new() :: old_set:orswot(),
          metadata    = old_map:new()    :: old_map:map()
         }).

-record(role_0, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = old_set:new() :: old_set:orswot(),
          ptree       = libsnarlmatch_tree:new(),
          metadata    = old_map:new()    :: old_map:map()
         }).

-record(role_1, {
          uuid        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          permissions = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ptree       = libsnarlmatch_tree:new(),
          metadata    = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-define(ROLE, role_1).
