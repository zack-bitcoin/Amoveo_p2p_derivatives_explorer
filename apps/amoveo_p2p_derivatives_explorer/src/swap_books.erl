-module(swap_books).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/9, read/1, markets/0, markets2/0, garbage_cron/0, using_contract/1,
         garbage/0
]).

%this is for storing the current order books for all the markets of swap offers.

-define(LOC, "swap_books.db").
-define(cron, 1000).%for production we probably want this higher than 1 second.
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
handle_cast({garbage, Height}, X) -> 
    X2 = garbage(X, Height),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, MID}, _From, X) -> 
    Z = dict:find(MID, X),
    {reply, Z, X};
handle_call(markets2, _From, X) ->
    Y = lists:map(
          fun(Z) ->
                  {ok, Market} = dict:find(Z, X),
                  Market#market{orders = 0},
                  #market{cid1 = CID1, cid2 = CID2} = Market,
                  T1 = scalar_contracts:read_contract(CID1),
                  T2 = scalar_contracts:read_contract(CID2),
                  R1 = case T1 of
                           {ok, Text1} -> Text1;
                           _ -> ""
                       end,
                  R2 = case T2 of
                           {ok, Text2} -> Text2;
                           _ -> ""
                       end,
                  {Market, R1, R2}
          end, 
          dict:fetch_keys(X)),
    {reply, Y, X};
handle_call(markets, _From, X) ->
    Y = lists:map(
          fun(Z) ->
                  {ok, Market} = dict:find(Z, X),
                  Market#market{orders = 0}
          end, 
          dict:fetch_keys(X)),
    {reply, Y, X};
handle_call({using_contract, CID}, _From, X) ->
    %checks if we are using this contract. scans all the markets.
    K = dict:fetch_keys(X),
    B = using_contract_internal(CID, X, K),
    {reply, B, X};
handle_call(_, _From, X) -> {reply, X, X}.

using_contract_internal(_, _, []) ->
    false;
using_contract_internal(CID, X, [H|T]) ->
    M = dict:fetch(H, X),
    if
        M#market.cid1 == CID -> true;
        M#market.cid2 == CID -> true;
        true -> 
            using_contract_internal(CID, X, T)
    end.

using_contract(X) ->
    gen_server:call(?MODULE, {using_contract, X}).

add(MID, TID, Price, Amount, Nonce, CID1, Type1, CID2, Type2) ->
    S = new_order(TID, Price, Amount),
    gen_server:cast(?MODULE, {add, MID, S, Nonce, CID1, Type1, CID2, Type2}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
markets() ->
    gen_server:call(?MODULE, markets).
markets2() ->
    gen_server:call(?MODULE, markets2).
garbage() ->
    FN = utils:server_url(external),
    X = talker:talk({height}, FN),
    case X of
        {ok, Height} ->
            gen_server:cast(?MODULE, {garbage, Height});
        _ -> ok
    end.

garbage(D, Height) ->
%    FN = utils:server_url(external),
%    X = talker:talk({height}, FN),
%    case X of
%        {ok, Height} ->
    K = dict:fetch_keys(D),
    garbage2(K, D, Height).
%        _ -> D
%    end.
garbage2([], D, _) -> D;
garbage2([MID|T], D, Height) -> 
    Market = dict:fetch(MID, D),
    Orders = Market#market.orders,
    Orders2 = garbage_orders(Orders, MID, Height),
    D2 = if
             ([] == Orders2) ->
                 dict:erase(MID, D);
             true ->
                 Market2 = 
                     Market#market{
                       orders = Orders2},
                 dict:store(MID, Market2, D)
         end,
    garbage2(T, D2, Height).
garbage_orders([], _, _) -> [];
garbage_orders([H|T], MID, Height) -> 
    TID = H#order.tid,
    F = case swap_full:read(TID) of
            error -> [];
            {ok, {S, Second}} ->
                Offer = element(2, S),
                B = swap_verify:keep_longer(Offer, Height, TID),
                if 
                    B -> [H];
                    true -> 
                        swap_history:remove(MID, TID),
                        case Second of
                            0 -> ok;
                            _ -> re_absorb_cron(Second)
                        end,
                        swap_full:remove(TID),
                        []
                end
        end,
    F ++ garbage_orders(T, MID, Height).


merge(S, []) -> [S];
merge(S, [H|T]) -> 
    if
        S#order.price > H#order.price ->
            [S|[H|T]];
        true ->
            [H|merge(S, T)]
    end.
    

garbage_cron() ->
    timer:sleep(?cron),
    spawn(fun() ->
                  garbage()
          end),
    spawn(fun() ->
                  garbage_cron()
          end).
    


re_absorb_cron(SignedOffer) ->
    FN = utils:server_url(external),
    {ok, Height} = talker:talk({height}, FN),
    spawn(fun()->
                  re_absorb_cron2(SignedOffer, Height)
          end).
re_absorb_cron2(SignedOffer, Height1) ->
    %for 2 blocks, keep trying to re-add this offer to the order book.
    io:fwrite("re absorb cron\n"),
    FN = utils:server_url(external),
    {ok, Height2} = talker:talk({height}, FN),
    Offer = element(2, SignedOffer),
    TID = utils:trade_id(Offer),
    B = swap_verify:keep_longer(Offer, Height2, TID),%TODO we actually only need to check if their account/sub_account can afford the trade, and we only need to do this once per block.
    if
        Height2 > (Height1 + 2) -> 
            io:fwrite("re absorb cron deleted\n"),
            %deleting the 2nd offer.
            ok;
        B -> 
            io:fwrite("re absorb cron succeeded\n"),
            http_handler:doit({add, SignedOffer, 0});
        true ->
            timer:sleep(?cron),
            re_absorb_cron2(SignedOffer, Height1)
    end.
            
