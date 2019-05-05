-module(oracles).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1, remove/1, read/1,
buys/1, sells/1, new/1, add_trade/2,
clean/0, cron/0,
test/0]).
-define(LOC, "oracles.db").
-define(clean_period, 300).
-record(oracle, {oid, 
                 buys,%pointers to channel offers to buy, ordered by price
                 sells, question, expiration
                }).
buys(X) -> X#oracle.buys.
sells(X) -> X#oracle.sells.
new(OID) ->
    FNL = "http://127.0.0.1:8081",
    FN = "http://127.0.0.1:8080",
    {ok, Oracle} = talker:talk({oracle, OID}, FNL),
    {ok, {_, Q}} = talker:talk({oracle, OID}, FN),
    E = element(5, Oracle),
    #oracle{oid = OID, buys = [], sells = [],
            question = Q, expiration = E}.
add_trade(Oracle, Trade) ->
    OID = Oracle#oracle.oid,
    OID = channel_offers_ram:oid(Trade),
    D = channel_offers_ram:direction(Trade),
    case D of
        1 -> %add to buys
            NewBuys = add(Trade, Oracle#oracle.buys),
            Oracle#oracle{buys = NewBuys};
        2 -> 
            NewSells = add(Trade, Oracle#oracle.sells),
            Oracle#oracle{sells = NewSells}
    end.
add(Trade, []) ->
    CID = channel_offers_ram:cid(Trade),
    [CID];
add(Trade, [H|T]) ->
    Trade2 = hd(channel_offers_ram:read([H])),
    B = channel_offers_ram:price(Trade) > 
        channel_offers_ram:price(Trade2),
    if
        B -> [channel_offers_ram:cid(Trade)|[H|T]];
        true -> [H|add(Trade, T)]
    end.
            
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
    io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Y}, X) -> 
    K = Y#oracle.oid,
    X2 = dict:store(K, Y, X),
    {noreply, X2};
handle_cast({remove, CID}, X) -> 
    X2 = dict:erase(CID, X),
    {noreply, X2};
handle_cast(clean, X) -> 
    K = dict:fetch_keys(X),
    X2 = clean_internal(K, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, OID}, _From, X) -> 
    Y = dict:find(OID, X),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(X) -> 
    gen_server:cast(?MODULE, {add, X}).
remove(OID) -> gen_server:cast(?MODULE, {remove, OID}).
read(OID) -> gen_server:call(?MODULE, {read, OID}).

clean() -> gen_server:cast(?MODULE, clean).
clean_internal([], D) -> D;
clean_internal([H|T], D) ->
    X = dict:fetch(H, D),
    Buys = X#oracle.buys,
    Sells = X#oracle.sells,
    Buys2 = clean_trades(Buys),
    Sells2 = clean_trades(Sells),
    X2 = X#oracle{buys = Buys2, sells = Sells2},
    D2 = dict:store(H, X2, D),
    clean_internal(T, D2).
clean_trades([]) -> [];
clean_trades([H|T]) ->
    X = channel_offers_ram:read([H]),
    case X of
        [] -> clean_trades(T);
        _ -> [H|clean_trades(T)]
    end.
            
cron() -> utils:cron_job(?clean_period, fun() -> clean() end).

    
test() ->
    OID = <<>>,
    X = #oracle{oid = OID, buys = [], sells = [], question = <<"">>, expiration = 1000}, 
    add(X),
    {ok, X} = read(OID),
    remove(OID),
    error = read(OID),
    success.
    
