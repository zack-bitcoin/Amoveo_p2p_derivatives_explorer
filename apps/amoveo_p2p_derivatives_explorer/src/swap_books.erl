-module(swap_books).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/1, read/1
]).

%this is for storing the current order books for all the markets of swap offers.


-record(swap_tx, {from, offer, fee}).
-record(market, 
        {book = []
        }).

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

add(S) ->
    gen_server:cast(?MODULE, {add, S}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).

