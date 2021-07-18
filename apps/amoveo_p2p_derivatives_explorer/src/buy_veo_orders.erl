-module(buy_veo_orders).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/1, read_contract/1, clean/0, 
         keys/0, sync/2, cron/0]).

-define(LOC, "buy_veo_orders.db").
-record(contract, 
        {cid, source = <<0:256>>, 
         source_type = 0, choose_address_timeout,
         oracle_start_height, blockchain,
         amount, ticker, date, trade_id, now
        }).

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("buy veo orders died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, C1}, X) ->
    Now = erlang:timestamp(),
    C = C1#contract{
          now = Now
         },
    CID = C#contract.cid,
    CID2 = cid_maker(C),
    %CID2 = CID,
    if 
        (not(CID == CID2)) ->
            io:fwrite("buy veo orders, cids don't match\n"),
            {noreply, X};
        true ->
            
            X2 = case dict:find(CID, X) of
                     error ->
                         io:fwrite("adding contract\n"),
                         io:fwrite(packer:pack(CID)),
                         io:fwrite("\n"),
                         dict:store(CID, C, X);
                     _ -> 
                         %io:fwrite(dict:find(CID, X)),
                         %io:fwrite("\n"),
                         io:fwrite("not adding contract, id taken\n"),
                         X
                 end,
            {noreply, X2}
    end;
handle_cast(backup, X) -> 
    db:save(?LOC, X),
    {noreply, X};
handle_cast({remove, CID}, X) ->
    %unused
    {noreply, dict:erase(CID, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call({read_contract, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(keys, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call(_, _From, X) -> {reply, X, X}.

cid_maker(Contract) ->
    #contract{
           source = Source,
           source_type = SourceType,
           choose_address_timeout = 
               ChooseAddressTimeout,
           oracle_start_height = OracleStartHeight,
           blockchain = Blockchain,
           amount = Amount,
           ticker = Ticker,
           date = Date,
           trade_id = TID
          } = Contract,
    PrivDir = "../../../../../amoveo/apps/amoveo_core/priv",
    {ok, CodeStatic} = file:read_file(PrivDir ++ "/buy_veo.fs"),
    {ok, CodeStatic2} = file:read_file(PrivDir ++ "/buy_veo_part2.fs"),

    OSHS = integer_to_binary(OracleStartHeight),
    ReusableSettings = 
        <<" int4 ", 
          OSHS/binary, 
          " .\" ",
          Blockchain/binary,
          "\" .\" ",
          Amount/binary,
          "\" .\" ",
          Ticker/binary,
          "\" .\" ",
          Date/binary, 
          "\" ">>,
    Settings = <<" int ",
                 (integer_to_binary(ChooseAddressTimeout))/binary,
                 " ", 
                 ReusableSettings/binary,
                 " int 1 binary 32 ", 
                 (base64:encode(TID))/binary>>,
    %the 1 is the trade nonce.
    Gas = 100000,
    HashStatic2 = base64:encode(hd(chalang:stack(chalang:test(compiler_chalang:doit(<<CodeStatic2/binary, " part2 ">>), Gas, Gas, Gas, Gas, [])))), %contract hash of the static part.
    HashStatic2def = 
          <<" macro part2 binary 32 ", 
            HashStatic2/binary,
          " ; ">>,
    StaticBytes = compiler_chalang:doit(<<HashStatic2def/binary, CodeStatic/binary>>),
    SettingsBytes = compiler_chalang:doit(Settings),
    FullContract = <<SettingsBytes/binary, StaticBytes/binary>>,
    CH = hash:doit(FullContract),
    CID = hash:doit(<<CH/binary,
                      Source/binary,
                      2:16,
                      SourceType:16>>),
    CID.



clean() ->
    %TODO
    %if the offer expired, or was canceled.
    ok.
sync(IP, Port) ->
    %TODO
    %add instructions to readme.

%doit({contracts, 2}) ->
    {ok, CIDS} = 
        talker:talk(
          {contracts, 2},
          {IP, Port}),
    %sync_contracts([hd(CIDS)], {IP, Port}).
    sync_contracts(CIDS, {IP, Port}).
sync_contracts([], _) -> ok;
sync_contracts([CID|T], Peer) -> 
    {ok, Contract} = 
        talker:talk({read, 3, CID}, Peer),
    add(Contract),
    sync_contracts(T, Peer).
   
backup() ->
    gen_server:cast(?MODULE, backup).
keys() ->
    gen_server:call(?MODULE, keys).
add(0) -> ok;
add(Contract) when is_record(Contract, contract) ->
    %we can trust them to give the correct CID, because 256 bytes is too much space to find a collision, and because the light node can detect an incorrect cid from the contract data.
    %TODO, check the contract data is reasonably sized, and of the correct format.
    gen_server:cast(?MODULE, {add, Contract}).
read_contract(CID) ->
    gen_server:call(?MODULE, {read_contract, CID}).

cron() ->
    timer:sleep(60000),
    backup(),
    spawn(fun() -> clean() end),
    cron().
