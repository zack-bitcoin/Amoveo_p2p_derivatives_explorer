-module(channel_offers_ram).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1, remove/1, read/1,
test/0]).
-record(channel_offer, {cid, oid, price, direction, hd_location}).

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, C}, X) -> 
    K = C#channel_offer.cid,
    X2 = dict:store(K, C, X),
    {noreply, X2};
handle_cast({remove, CID}, X) -> 
    X2 = dict:erase(CID, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, L}, _From, X) -> 
    Y = read_loop(L, X),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

read_loop([], _) -> [];
read_loop([H|T], D) ->
    case dict:find(H, D) of
        error -> read_loop(T, D);
        {ok, X} -> [X|read_loop(T, D)]
    end.
                                 
add(C) ->
    %cid is the key for storing in a dict.
    gen_server:cast(?MODULE, {add, C}).
remove(CID) ->
    gen_server:cast(?MODULE, {remove, CID}).
read(L) when is_list(L) ->%list of cids
    gen_server:call(?MODULE, {read, L}).

test() ->
    CID = <<>>,
    C = #channel_offer{cid = CID},
    add(C),
    [C] = read([CID]),
    remove(CID),
    [] = read([CID]),
    success.
