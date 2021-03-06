-module(scalar_contracts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/3, add/5, read_contract/1,
         clean/0, keys/0, sync/2,
         cron/0]).

%-record(c, {string, max_price, oracle_start_height}).

%this is for storing the data needed to enforce the outcome for any scalar type contract
%so if you want to post a swap tx, the subcurrencies being swapped need to have enforcement data.
-define(LOC, "scalar_contracts.db").
-record(scalar, {text, height, max_price, now, source = <<0:256>>, source_type = 0}).

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
    io:format("scalar contracts died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, CID, Text, Height, MaxPrice, Source, SourceType}, X) -> 
    Now = erlang:timestamp(),
    C = #scalar{text = Text, 
                height = Height, 
                max_price = MaxPrice, 
                now = Now,
                source = Source,
                source_type = SourceType},
    X2 = dict:store(CID, C, X),
    {noreply, X2};
handle_cast({add, CID, Text, Height, MaxPrice}, X) -> 
    Now = erlang:timestamp(),
    C = #scalar{text = Text, 
                height = Height, 
                max_price = MaxPrice, 
                now = Now},
    X2 = dict:store(CID, C, X),
    {noreply, X2};
handle_cast({remove, CID}, X) ->
    {noreply, dict:erase(CID, X)};
handle_cast(backup, X) -> 
    db:save(?LOC, X),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(keys, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call({check, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

sync(IP, Port) ->
    {ok, CIDS} = 
        talker:talk(
          {contracts}, 
          {IP, Port}),
    sync_contracts(CIDS, {IP, Port}).
sync_contracts([], _) -> ok;
sync_contracts([CID|T], Peer) -> 
    {ok, Contract} = 
        talker:talk({read, 3, CID}, Peer),
    #scalar
        {text = Text,
         height = Height,
         max_price = MaxPrice,
         source = Source,
         source_type = SourceType
        } = Contract,
    add(Text, Height, MaxPrice, 
        Source, SourceType),
    sync_contracts(T, Peer).

clean() ->
    IN = utils:server_url(internal),
    {ok, H1} = talker:talk({height}, IN),
    {ok, H2} = talker:talk({height, 1}, IN),
    if
        H1 < 130000 -> 
            io:fwrite("need to sync the full node first before cleaning scalar contracts");
        not(H1 == H2) ->
            io:fwrite("need to sync the blocks in the full node first before syncing scalar contracts.");
        true ->
            D = gen_server:call(?MODULE, read_dict),
            clean_internal(D)
    end.
clean_internal(D) ->
    Ks = dict:fetch_keys(D),
    clean_internal2(D, Ks).
clean_internal2(_, []) -> ok;
clean_internal2(D, [K|T]) ->
    A = dict:fetch(K, D),
    #scalar{
             text = Question,
             now = Then,
             height = Start
           } = A,
    FN = utils:server_url(external),
    IN = utils:server_url(internal),
    {ok, Contract} = talker:talk({contracts, K}, FN),
    %and this contract was not created on-chain
    B1 = (Contract == 0),
    Now = erlang:timestamp(),
    DiffSeconds = 
        timer:now_diff(Now, Then) / 1000000,
    %if more than 100 minutes passed,
    B2 = DiffSeconds > (60*100),
    %and we aren't storing any open offer for this.
    B3 = not(swap_books:using_contract(K)),
    QH = hash:doit(Question),
    OID = hash:doit(<<Start:32,0:32,0:32,QH/binary>>),
    {ok, Oracle} = talker:talk({oracle, OID}, IN),
    %if this oracle was already posted on-chain.
    B4 = not(Oracle == 0),
    if
        (B1 and B2 and B3) or
        (not(B1) and B4) -> 
            gen_server:cast(?MODULE, {remove, K});
        true -> ok
    end,
    clean_internal2(D, T).

    

cid_maker(Text, Height, MaxPrice) ->
    cid_maker(Text, Height, MaxPrice, <<0:256>>, 0).
cid_maker(Text, _Height, MaxPrice, Source, SourceType) ->
    true = is_binary(Text),
    StaticContract = base64:decode(
        "bpYZNRc5AzAyj4cUGIYWjDpGFBRHFHBxSG8AAAAAAXgAAAAAAngWAAAAAAN4gxSDFhSDFhSDFKyHAAAAAAJ5jBWGhgAAAAABeQAAAAADeYw6RhQUAgAAAAEwRxSQjIcWFBYCAAAAIGRuan/EdSKkhbAp0OEF6cQDv9x9li1vx5O6vqNMm3KlcUiGKIYoO0ZHDUiNhxYUAgAAAAEBO0ZHDUiEAAAAAAN5FoIA/////wAAAAADeTMWgoiMBAPo"),

%"bpYZNRc5AzAyj4cUGIYWjDpGFBRHFHBxSG8AAAAAAXgAAAAAAngWAAAAAAN4gxSDFhSDFhSDFKyHAAAAAAF5jBWGhgAAAAACeQAAAAADeYw6RhQUAgAAAAEwRxSQjIcWFBYCAAAAIGRuan/EdSKkhbAp0OEF6cQDv9x9li1vx5O6vqNMm3KlcUiGKIYoO0ZHDUiNhxYUAgAAAAEBO0ZHDUiEAAAAAAN5FoIA/////wAAAAADeTMWgoiMBAPo"),
    OracleTextPart = "MaxPrice = " ++ integer_to_list(MaxPrice) ++ "; MaxVal = 4294967295; B = " ++ binary_to_list(Text) ++ " from $0 to $MaxPrice; max(0, min(MaxVal, (B * MaxVal / MaxPrice)) is ",
    L = length(OracleTextPart),
    %FullContract = <<0, Height:32, 2, L:32, (list_to_binary(OracleTextPart))/binary, StaticContract/binary>>,
    FullContract = <<22, 2, L:32, (list_to_binary(OracleTextPart))/binary, StaticContract/binary>>,
    CH = hash:doit(FullContract),
    CID = hash:doit(<<CH/binary,
                      Source/binary,
                      2:16,
                      SourceType:16>>),
    CID.

add(Text, Height, MaxPrice, Source, SourceType) ->
    CID = cid_maker(Text, Height, MaxPrice, Source, SourceType),
    gen_server:cast(?MODULE, {add, CID, Text, Height, MaxPrice, Source, SourceType}),
    CID.
add(Text, Height, MaxPrice) ->
    CID = cid_maker(Text, Height, MaxPrice),
    gen_server:cast(?MODULE, {add, CID, Text, Height, MaxPrice}),
    CID.
backup() ->
    gen_server:cast(?MODULE, backup).
read_contract(<<0:256>>) -> 
    true;
read_contract(CID) ->
    gen_server:call(?MODULE, {check, CID}).
keys() ->
    gen_server:call(?MODULE, keys).
   
cron() ->
    timer:sleep(60000),
    backup(),
    spawn(fun() -> clean() end),
    cron().
