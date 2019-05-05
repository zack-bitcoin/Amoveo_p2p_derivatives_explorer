%eventually this will belong on the hard drive. For now we have a simple all-ram implementation to save time in development.

-module(channel_offers_hd).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/2,remove/1,read/1,
test/0]).
-define(LOC, "channel_offers_hd.db").
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
    io:format("channel offers hd died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Key, Value}, X) -> 
    X2 = dict:store(Key, Value, X),
    {noreply, X2};
handle_cast({remove, K}, X) -> 
    X2 = dict:erase(K, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, K}, _From, X) -> 
    Y = dict:find(K, X),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(CID, X) -> gen_server:cast(?MODULE, {add, CID, X}).
remove(OID) -> gen_server:cast(?MODULE, {remove, OID}).
read(OID) -> gen_server:call(?MODULE, {read, OID}).

test() ->
    CID = <<>>,
    X = <<>>,
    add(CID, X),
    {ok, X} = read(CID),
    remove(CID),
    error = read(CID),
    success.
    
