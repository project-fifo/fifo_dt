-record(client_0, {
          uuid          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          client_id     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          secret        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          redirect_uris = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          permissions   = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          roles         = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata      = old_map:new()    :: old_map:map()
         }).

-record(client_1, {
          uuid          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          client_id     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          secret        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          redirect_uris = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          permissions   = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ptree         = libsnarlmatch_tree:new(),
          roles         = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata      = old_map:new()    :: old_map:map()
         }).

-record(client_2, {
          uuid          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          name          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          client_id     = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          secret        = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          type          = riak_dt_lwwreg:new() :: riak_dt_lwwreg:lwwreg(),
          redirect_uris = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          permissions   = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          ptree         = libsnarlmatch_tree:new(),
          roles         = riak_dt_orswot:new() :: riak_dt_orswot:orswot(),
          metadata      = riak_dt_map:new()    :: riak_dt_map:map()
         }).

-define(CLIENT, client_2).
