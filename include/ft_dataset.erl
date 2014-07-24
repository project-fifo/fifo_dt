%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
-module(ft_dataset).

-include("sniffle.hrl").
-include_lib("ft.hrl").

-define(G(N, F),
        getter(#sniffle_obj{val=S0}, N) ->
               F(S0)).

-define(G(E),
        E(H) -> riak_dt_lwwreg:value(H#?DATASET.E)).

-define(S(E),
        E({T, _ID}, V, H) ->
               {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DATASET.E),
               H#?DATASET{E = V1}).

-define(S(N, F),
        set(TID, N, Value, D) ->
               F(TID, Value, D)).

-export([
         new/1,
         load/2,
         uuid/1, uuid/3,
         type/1, type/3,
         status/1, status/3,
         imported/1, imported/3,
         set/4,
         getter/2,
         to_json/1,
         metadata/1, set_metadata/4,
         requirements/1, add_requirement/3, remove_requirement/3,
         merge/2
        ]).

-ignore_xref([load/2, load/1, set/4, set/3, getter/2, merge/2,
              type/1, type/3,
              uuid/1, uuid/3,
              imported/1, imported/3,
              status/1, status/3,
              requirements/1, add_requirement/3, remove_requirement/3,
              metadata/1, set_metadata/4
             ]).

-export([
         dataset/1,
         dataset/3,
         description/1,
         description/3,
         disk_driver/1,
         disk_driver/3,
         homepage/1,
         homepage/3,
         image_size/1,
         image_size/3,
         name/1,
         name/3,
         networks/1,
         networks/3,
         nic_driver/1,
         nic_driver/3,
         os/1,
         os/3,
         users/1,
         users/3,
         version/1,
         version/3
        ]).

-ignore_xref([
              dataset/1,
              dataset/3,
              description/1,
              description/3,
              disk_driver/1,
              disk_driver/3,
              homepage/1,
              homepage/3,
              image_size/1,
              image_size/3,
              name/1,
              name/3,
              networks/1,
              networks/3,
              nic_driver/1,
              nic_driver/3,
              os/1,
              os/3,
              users/1,
              users/3,
              version/1,
              version/3
             ]).


new({T, _ID}) ->
    {ok, Imported} = ?NEW_LWW(0, T),
    {ok, Status} = ?NEW_LWW(<<"new">>, T),
    #?DATASET{
        imported = Imported,
        status = Status
       }.
?G(<<"uuid">>, uuid);
?G(<<"type">>, type);
?G(<<"status">>, status);
?G(<<"imported">>, imported);

?G(<<"dataset">>, dataset);
?G(<<"description">>, description);
?G(<<"disk_driver">>, disk_driver);
?G(<<"homepage">>, homepage);
?G(<<"image_size">>, image_size);
?G(<<"name">>, name);
?G(<<"networks">>, networks);
?G(<<"nic_driver">>, nic_driver);
?G(<<"os">>, os);
?G(<<"users">>, users);
?G(<<"version">>, version).

?G(uuid).
?S(uuid).

?G(type).
type(ID, <<"kvm">>, H) ->
    type(ID, kvm, H);
type(ID, <<"zone">>, H) ->
    type(ID, zone, H);
type({T, _ID}, V, H) when V =:= kvm;
                          V =:= zone ->
    {ok, V1} = riak_dt_lwwreg:update({assign, V, T}, none, H#?DATASET.type),
    H#?DATASET{type = V1}.

?G(status).
?S(status).
?G(imported).
?S(imported).

?G(dataset).
?S(dataset).
?G(description).
?S(description).
?G(disk_driver).
?S(disk_driver).
?G(homepage).
?S(homepage).
?G(image_size).
?S(image_size).
?G(name).
?S(name).
?G(networks).
?S(networks).
?G(nic_driver).
?S(nic_driver).
?G(os).
?S(os).
?G(users).
?S(users).
?G(version).
?S(version).

requirements(H) ->
    riak_dt_orswot:value(H#?DATASET.requirements).

add_requirement({_T, ID}, V, H) ->
    {ok, O1} = riak_dt_orswot:update({add, V}, ID, H#?DATASET.requirements),
    H#?DATASET{requirements = O1}.

remove_requirement({_T, ID}, V, H) ->
    case riak_dt_orswot:update({remove, V}, ID, H#?DATASET.requirements) of
        {error,{precondition,{not_present,_}}} ->
            H;
        {ok, O1} ->
            H#?DATASET{requirements = O1}
    end.

metadata(H) ->
    fifo_map:value(H#?DATASET.metadata).

set_metadata({T, ID}, P, Value, User) when is_binary(P) ->
    set_metadata({T, ID}, fifo_map:split_path(P), Value, User);

set_metadata({_T, ID}, Attribute, delete, G) ->
    {ok, M1} = fifo_map:remove(Attribute, ID, G#?DATASET.metadata),
    G#?DATASET{metadata = M1};

set_metadata({T, ID}, Attribute, Value, G) ->
    {ok, M1} = fifo_map:set(Attribute, Value, ID, T, G#?DATASET.metadata),
    G#?DATASET{metadata = M1}.

-define(V(R), riak_dt_lwwreg:value(R)).

to_json(D) ->
    Type = case type(D) of
               <<>> -> <<>>;
               kvm -> <<"kvm">>;
               zone -> <<"zone">>
           end,
    Vs = [
          {<<"dataset">>, fun dataset/1},
          {<<"description">>, fun description/1},
          {<<"disk_driver">>, fun disk_driver/1},
          {<<"homepage">>, fun homepage/1},
          {<<"image_size">>, fun image_size/1},
          {<<"imported">>, fun imported/1},
          {<<"metadata">>, fun metadata/1},
          {<<"name">>, fun name/1},
          {<<"networks">>, fun networks/1},
          {<<"nic_driver">>, fun nic_driver/1},
          {<<"os">>, fun os/1},
          {<<"requirements">>, fun requirements/1},
          {<<"status">>, fun status/1},
          {<<"users">>, fun users/1},
          {<<"uuid">>, fun uuid/1},
          {<<"version">>, fun version/1}
         ],
    add(Vs, D, [{<<"type">>, Type}]).

add([], _, D) ->
    D;
add([{<<"type">> = N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        kvm ->
            add(R, In, jsxd:set(N, <<"kvm">>, D));
        zone ->
            add(R, In, jsxd:set(N, <<"zone">>, D))
    end;
add([{N, F} | R], In, D) ->
    case F(In) of
        <<>> ->
            add(R, In, D);
        undefined ->
            add(R, In, D);
        V ->
            add(R, In, jsxd:set(N, V, D))
    end.



load(_, #?DATASET{} = H) ->
    H;

load({T, ID}, Sb) ->
    H = statebox:value(Sb),
    {ok, UUID} = jsxd:get([<<"uuid">>], H),
    {ok, Imported} = jsxd:get([<<"imported">>], H),
    {ok, Status} = jsxd:get([<<"status">>], H),
    {ok, Metadata} = jsxd:get([<<"metadata">>], [], H),
    {ok, UUID1} = ?NEW_LWW(UUID, T),
    {ok, Imported1} = ?NEW_LWW(Imported, T),
    {ok, Status1} = ?NEW_LWW(Status, T),
    Requirements = jsxd:get(<<"requirements">>, [], H),

    Metadata1 = fifo_map:from_orddict(Metadata, ID, T),
    Requirements1 = riak_dt_orswot:update(
                      {add_all, Requirements}, ID,
                      riak_dt_orswot:new()),

    D = #dataset_0_1_0{
           uuid = UUID1,
           imported = Imported1,
           status = Status1,
           requirements    = Requirements1,
           metadata = Metadata1
          },
    D1 = case jsxd:get([<<"dataset">>], H) of
             {ok, Dataset} ->
                 {ok, Dataset1} = ?NEW_LWW(Dataset, T),
                 D#dataset_0_1_0{dataset = Dataset1};
             _->
                 D
         end,
    D2 = case jsxd:get([<<"description">>], H) of
             {ok, Description} ->
                 {ok, Description1} = ?NEW_LWW(Description, T),
                 D1#dataset_0_1_0{description = Description1};
             _->
                 D1
         end,
    D3 = case jsxd:get([<<"disk_driver">>], H) of
             {ok, DiskDriver} ->
                 {ok, DiskDriver1} = ?NEW_LWW(DiskDriver, T),
                 D2#dataset_0_1_0{disk_driver = DiskDriver1};
             _->
                 D2
         end,
    D4 = case jsxd:get([<<"homepage">>], H) of
             {ok, Homepage} ->
                 {ok, Homepage1} = ?NEW_LWW(Homepage, T),
                 D3#dataset_0_1_0{homepage = Homepage1};
             _->
                 D3
         end,
    D5 = case jsxd:get([<<"image_size">>], H) of
             {ok, ImageSize} ->
                 {ok, ImageSize1} = ?NEW_LWW(ImageSize, T),
                 D4#dataset_0_1_0{image_size = ImageSize1};
             _->
                 D4
         end,
    D6 = case jsxd:get([<<"name">>], H) of
             {ok, Name} ->
                 {ok, Name1} = ?NEW_LWW(Name, T),
                 D5#dataset_0_1_0{name = Name1};
             _->
                 D5
         end,
    D7 = case jsxd:get([<<"networks">>], H) of
             {ok, Networks} ->
                 {ok, Networks1} = ?NEW_LWW(Networks, T),
                 D6#dataset_0_1_0{networks = Networks1};
             _->
                 D6
         end,
    D7 = case jsxd:get([<<"nic_driver">>], H) of
             {ok, NicDriver} ->
                 {ok, NicDriver1} = ?NEW_LWW(NicDriver, T),
                 D6#dataset_0_1_0{nic_driver = NicDriver1};
             _->
                 D6
         end,
    D8 = case jsxd:get([<<"os">>], H) of
             {ok, OS} ->
                 {ok, OS1} = ?NEW_LWW(OS, T),
                 D7#dataset_0_1_0{os = OS1};
             _->
                 D7
         end,
    D9 = case jsxd:get([<<"users">>], H) of
             {ok, Users} ->
                 {ok, Users1} = ?NEW_LWW(Users, T),
                 D8#dataset_0_1_0{users = Users1};
             _->
                 D8
         end,
    D10 = case jsxd:get([<<"version">>], H) of
              {ok, Version} ->
                  {ok, Version1} = ?NEW_LWW(Version, T),
                  D9#dataset_0_1_0{version = Version1};
              _->
                  D9
          end,
    load({T, ID}, D10).

?S(<<"uuid">>, uuid);
?S(<<"status">>, status);
?S(<<"type">>, type);
?S(<<"imported">>, imported);

?S(<<"dataset">>, dataset);
?S(<<"description">>, description);
?S(<<"disk_driver">>, disk_driver);
?S(<<"homepage">>, homepage);
?S(<<"image_size">>, image_size);
?S(<<"name">>, name);
?S(<<"networks">>, networks);
?S(<<"nic_driver">>, nic_driver);
?S(<<"os">>, os);
?S(<<"users">>, users);
?S(<<"version">>, version);

set(ID, K = <<"metadata.", _/binary>>, V, H) ->
    set(ID, re:split(K, "\\."), V, H);
set(ID, [<<"metadata">> | R], V, H) ->
    set_metadata(ID, R, V, H).

merge(#?DATASET{
          dataset        = Dataset1,
          description    = Desc1,
          disk_driver    = DiskD1,
          homepage       = Homepage1,
          image_size     = ImageSize1,
          imported       = Imported1,
          metadata       = Metadata1,
          name           = Name1,
          networks       = Networks1,
          nic_driver     = NicD1,
          os             = OS1,
          requirements   = Reqs1,
          status         = Status1,
          type           = Type1,
          users          = Users1,
          uuid           = UUID1,
          version        = Version1
         },
      #?DATASET{
          dataset        = Dataset2,
          description    = Desc2,
          disk_driver    = DiskD2,
          homepage       = Homepage2,
          image_size     = ImageSize2,
          imported       = Imported2,
          metadata       = Metadata2,
          name           = Name2,
          networks       = Networks2,
          nic_driver     = NicD2,
          os             = OS2,
          requirements   = Reqs2,
          status         = Status2,
          type           = Type2,
          users          = Users2,
          uuid           = UUID2,
          version        = Version2
         }) ->
    #?DATASET{
        dataset        = riak_dt_lwwreg:merge(Dataset1, Dataset2),
        description    = riak_dt_lwwreg:merge(Desc1, Desc2),
        disk_driver    = riak_dt_lwwreg:merge(DiskD1, DiskD2),
        homepage       = riak_dt_lwwreg:merge(Homepage1, Homepage2),
        image_size     = riak_dt_lwwreg:merge(ImageSize1, ImageSize2),
        imported       = riak_dt_lwwreg:merge(Imported1, Imported2),
        metadata       = fifo_map:merge(Metadata1, Metadata2),
        name           = riak_dt_lwwreg:merge(Name1, Name2),
        networks       = riak_dt_lwwreg:merge(Networks1, Networks2),
        nic_driver     = riak_dt_lwwreg:merge(NicD1, NicD2),
        os             = riak_dt_lwwreg:merge(OS1, OS2),
        requirements   = riak_dt_orswot:merge(Reqs1, Reqs2),
        status         = riak_dt_lwwreg:merge(Status1, Status2),
        type           = riak_dt_lwwreg:merge(Type1, Type2),
        users          = riak_dt_lwwreg:merge(Users1, Users2),
        uuid           = riak_dt_lwwreg:merge(UUID1, UUID2),
        version        = riak_dt_lwwreg:merge(Version1, Version2)
       }.
