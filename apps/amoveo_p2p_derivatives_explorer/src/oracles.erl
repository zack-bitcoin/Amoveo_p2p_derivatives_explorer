-module(oracles).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1, remove/1, read/1,
test/0]).
-record(oracle, {oid, 
                 buys,%pointers to channel offers to buy, ordered by price
                 sells, question, expiration
                }).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Y}, X) -> 
    K = Y#oracle.oid,
    X2 = dict:store(K, Y, X),
    {noreply, X2};
handle_cast({remove, CID}, X) -> 
    X2 = dict:erase(CID, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, OID}, _From, X) -> 
    Y = dict:find(OID, X),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(X) -> gen_server:cast(?MODULE, {add, X}).
remove(OID) -> gen_server:cast(?MODULE, {remove, OID}).
read(OID) -> gen_server:call(?MODULE, {read, OID}).
    
test() ->
    OID = <<>>,
    X = #oracle{oid = OID, buys = [], sells = [], question = <<"">>, expiration = 1000}, 
    add(X),
    {ok, X} = read(OID),
    remove(OID),
    error = read(OID),
    success.
    
