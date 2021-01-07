%store a copy of data about the current set of markets here.

%every few minutes we want to rescan the open trades, and make a new version of data to store here.

-module(swap_markets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/0, refresh/0,
        cron/0]).
-record(x, {id, cid1, type1, cid2, type2}).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(refresh, _) -> 
    %scan all open trades
    X = swap_books:markets(),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(read, _From, X) -> 
    {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

read() -> gen_server:call(?MODULE, read).
refresh() -> gen_server:cast(?MODULE, refresh).

cron() ->
    timer:sleep(5000),
    spawn(fun() ->
                  refresh()
          end),
    spawn(fun() ->
                  cron()
          end).
    
    
    
    












