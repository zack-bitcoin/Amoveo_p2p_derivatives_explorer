-module(swap_history).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/4, remove/2, read/2, garbage_cron/0
]).

%this is for storing a history of how the swap order books were created. That way it is easy to follow along with the current state of the market by downloading the parts of the history that you missed.

-include("records.hrl").
-define(LOC, "swap_history.db").
-record(sh, {nonce = 1, l}).
-record(add, {tid, a1, a2, ts}).
-record(remove, {tid, ts}).

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
    io:format("swap history died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) -> 
    Keys = dict:fetch_keys(X),
    Now = erlang:timestamp(),
    X2 = garbage2(X, Now, Keys),
    {noreply, X2};
handle_cast({remove, MID, TID}, X) ->
    TS = erlang:timestamp(),
    case dict:find(MID, X) of
        empty -> 
            io:fwrite("can't remove from a non-existant market"),
            {noreply, X};
        {ok, #sh{nonce = N1,
                 l = L}} ->
            SH2 = #sh{
              nonce = N1 + 1,
              l = [#remove{tid = TID, ts = TS}|L]
             },
            X2 = dict:store(MID, SH2, X),
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({add, MID, TID, A1, A2}, _, X) ->
    TS = erlang:timestamp(),
    E = #add{tid = TID, a1 = A1, a2 = A2, ts = TS},
    %E = {add, TID, A1, A2, TS},
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
    gen_server:cast(?MODULE, garbage).

garbage2(Dict, _, []) -> Dict;
garbage2(Dict, Now, [M|T]) ->
    SH = dict:fetch(M, Dict),
    L = SH#sh.l,
    L2 = remove_old(L, Now),
    SH2 = SH#sh{l = L2},
    Dict2 = dict:store(M, SH2, Dict),
    garbage2(Dict2, Now, T).
remove_old([], _) -> [];
remove_old([A|T], Now) when is_record(A, add) -> 
    #add{ts = TS} = A,
    remove_old2(TS, A, T, Now);
remove_old([R|T], Now) -> 
    #remove{ts = TS} = R,
    remove_old2(TS, R, T, Now).

remove_old2(TS, A, T, Now) ->
    Seconds = timer:now_diff(Now, TS) div 1000000,
    if
        Seconds > 600 -> %600 seconds is 10 minutes
            [];
        true -> [A|remove_old(T, Now)]
    end.
    

garbage_cron() ->
    spawn(fun() ->
                  timer:sleep(60000),
                  garbage(),
                  garbage_cron()
          end).
