-module(swap_books).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/9, read/1, markets/0, garbage_cron/0
]).

%this is for storing the current order books for all the markets of swap offers.

-include("records.hrl").

-record(market, {nonce = 1, mid, cid1, type1, cid2, type2, orders}).
-record(order, {price, amount, tid}).

new_order(TID, Price, Amount) ->
    #order{tid = TID,
           price = Price,
           amount = Amount}.

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, MID, S, Nonce, CID1, Type1, CID2, Type2}, X) -> 
    M2 = case dict:find(MID, X) of
             error -> #market{orders = [S], mid = MID,
                              cid1 = CID1, type1 = Type1,
                              cid2 = CID2, type2 = Type2
                             };
             {ok, M} -> 
                 L2 = merge(S, M#market.orders),
                 M#market{orders = L2,
                          nonce = Nonce}
         end,
    X2 = dict:store(MID, M2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, MID}, _From, X) -> 
    Z = dict:find(MID, X),
    {reply, Z, X};
handle_call(markets, _From, X) ->
    Y = lists:map(
          fun(Z) ->
                  {ok, Market} = dict:find(Z, X),
                  Market#market{orders = 0}
          end, 
          dict:fetch_keys(X)),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(MID, TID, Price, Amount, Nonce, CID1, Type1, CID2, Type2) ->
    S = new_order(TID, Price, Amount),
    gen_server:cast(?MODULE, {add, MID, S, Nonce, CID1, Type1, CID2, Type2}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
markets() ->
    gen_server:call(?MODULE, markets).

garbage_cron() ->
    ok.


merge(S, []) -> [S];
merge(S, [H|T]) -> 
    if
        S#order.price > H#order.price ->
            [S|[H|T]];
        true ->
            [H|merge(S, T)]
    end.
    
