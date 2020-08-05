-module(swap_history).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/4, remove/2, read/2
]).

%this is for storing a history of how the swap order books were created. That way it is easy to follow along with the current state of the market by downloading the parts of the history that you missed.

-include("records.hrl").
-record(sh, {nonce = 1, l}).

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({garbage, Market, NewHistory}, X) -> 
    X2 = dict:store(Market, NewHistory, X),
    {noreply, X2};
handle_cast({remove, MID, TID}, X) ->
    case dict:find(MID, X) of
        empty -> 
            io:fwrite("can't remove from a non-existant market"),
            {noreply, X};
        #sh{nonce = N1,
            l = L} ->
            SH2 = #sh{
              nonce = N1 + 1,
              l = [{remove, TID}|L]
             },
            X2 = dict:store(MID, SH2, X),
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({add, MID, TID, A1, A2}, _, X) ->
    TS = erlang:timestamp(),
    E = {add, TID, A1, A2, TS},
    Y = case dict:find(MID, X) of
            error -> 
                #sh{l = [E]};
            {ok, #sh{nonce = N1,
                     l = L}} -> 
                #sh{
              nonce = N1 + 1,
              l = [E|L]
             }
        end,
    X2 = dict:store(MID, Y, X),
    N = Y#sh.nonce,
    {reply, N, X2};
handle_call(read_all, _From, X) -> 
    {reply, X, X};
handle_call({read, MarketID, Nonce}, _From, X) -> 
    R = case dict:find(MarketID, X) of
            error  -> [];
            A ->
                #sh{
              nonce = N,
              l = L
             } = A,
                Many = min(length(L), N - Nonce),
                {L2, _} = lists:split(Many, L),
                L2
        end,
    {reply, R, X};
handle_call(_, _From, X) -> {reply, X, X}.


add(MarketID, TradeID, Amount1, Amount2) ->
    gen_server:call(?MODULE, {add, MarketID, TradeID, Amount1, Amount2}).
remove(MarketID, TradeID) ->
    gen_server:cast(?MODULE, {remove, MarketID, TradeID}).
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
