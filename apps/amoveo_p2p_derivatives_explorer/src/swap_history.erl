-module(swap_history).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/1, remove/1, read/2
]).

%this is for storing a history of how the swap order books were created. That way it is easy to follow along with the current state of the market by downloading the parts of the history that you missed.


-record(swap_tx, {from, offer, fee}).
-record(swap_offer, {
          acc1, start_limit, end_limit, salt,
          amount1, cid1, type1, %this is what acc1 gives.
          amount2, cid2, type2, %this is what acc2 gives.
          fee1, %what acc1 pays in fees
          fee2}).

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({garbage, Market, NewHistory}, X) -> 
    X2 = dict:store(Market, NewHistory, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(read_all, _From, X) -> 
    {reply, X, X};
handle_call({update, Trade}, _From, X) -> 
    case dict:find(Market, X) of
        error ->
            %create the market
            ok;
        H ->
            %add this trade to the history
            ok
    end,
    {reply, Nonce, X2};
handle_call({read, MarketID, Nonce}, _From, X) -> 
    R = case dict:find(MarketID, X) of
            error  -> [];
            A ->
                %take the slice of history between Nonce and now.
                ok
        end,
    {reply, R, X};
handle_call(_, _From, X) -> {reply, X, X}.



add(Trade) ->
    %returns nonce
    gen_server:call(?MODULE, {update, add, Trade}).
remove(Trade) ->
    %returns nonce
    gen_server:call(?MODULE, {update, remove, Trade}).
read(Market, Nonce) ->
    %returns history since Nonce.
    gen_server:call(?MODULE, {read, Market, Nonce}).

garbage() ->
    A = gen_server:call(?MODULE, read_all),
    %for every market
    %for every trade
    %if it is more than 5 minutes old, remove that part of the history.

    %gen_server:cast(?MODULE, {garbage, MarketID, NewHistory}),
    ok.
