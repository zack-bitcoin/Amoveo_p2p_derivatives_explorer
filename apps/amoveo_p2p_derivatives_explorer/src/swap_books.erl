-module(swap_books).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/9, read/1, markets/0, garbage_cron/0
]).

%this is for storing the current order books for all the markets of swap offers.

-define(LOC, "swap_books.db").
-include("records.hrl").

-record(market, {nonce = 1, mid, cid1, type1, cid2, type2, orders}).
-record(order, {price, amount, tid}).

new_order(TID, Price, Amount) ->
    #order{tid = TID,
           price = Price,
           amount = Amount}.

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
    io:format("swap books died!"), 
    ok.
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
handle_cast(garbage, X) -> 
    X2 = garbage(X),
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
garbage() ->
    gen_server:cast(?MODULE, garbage).

garbage(D) ->
    K = dict:fetch_keys(D),
    garbage2(K, D).
garbage2([], D) -> D;
garbage2([MID|T], D) -> 
    Market = dict:fetch(MID, D),
    Orders = Market#market.orders,
    Orders2 = garbage_orders(Orders, MID),
    Market2 = Market#market{orders = Orders2},
    D2 = dict:store(MID, Market2, D),
    garbage2(T, D2).
garbage_orders([], _) -> [];
garbage_orders([H|T], MID) -> 
    TID = H#order.tid,
    F = case swap_full:read(TID) of
            error -> [];
            {ok, S} ->
                FN = utils:server_url(external),
                {ok, Height} = talker:talk({height}, FN),
                Offer = element(2, S),
                B = swap_verify:keep_longer(Offer, Height, TID),
                if 
                    B -> [H];
                    true -> 
                        swap_history:remove(MID, TID),
                        swap_full:remove(TID),
                        []
                end
        end,
    F ++ garbage_orders(T, MID).


merge(S, []) -> [S];
merge(S, [H|T]) -> 
    if
        S#order.price > H#order.price ->
            [S|[H|T]];
        true ->
            [H|merge(S, T)]
    end.
    

garbage_cron() ->
    spawn(fun() ->
                  timer:sleep(20000),
                  garbage(),
                  garbage_cron()
          end).
