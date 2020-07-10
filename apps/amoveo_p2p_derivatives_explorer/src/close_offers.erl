-module(close_offers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1,read/1,test/0]).
-define(LOC, "close_offers.db").
init(ok) -> 
    case ets:info(?MODULE) of
        undefined ->
            case ets:file2tab(?LOC) of
                {ok, ?MODULE} -> ok;
                {error, _} ->
                    ets:new(?MODULE, [bag, named_table])
            end;
        _ -> ok
    end,
    {ok, 0}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    ets:tab2file(?MODULE, ?LOC, [{sync, true}]),
    io:format("close offers died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, CloseOffer}, N) -> 
    [_, _, _, Stx] = CloseOffer,
    Tx = element(2, Stx),
    Pubkey = element(2, Tx),
    TS = erlang:timestamp(),
    ets:insert(?MODULE, {Pubkey, CloseOffer, TS}),
    {noreply, N};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pubkey}, _From, X) -> 
    L = ets:lookup(?MODULE, Pubkey),
    L2 = offers_only(L),
    {reply, L2, X};
handle_call(_, _From, X) -> {reply, X, X}.

offers_only([]) -> [];
offers_only([{_, X, _}|T]) -> 
    [X|offers_only(T)].


read(Pubkey) ->
    gen_server:call(?MODULE, {read, Pubkey}).
add(X) ->
    gen_server:cast(?MODULE, {add, X}).


test() ->    
    Pubkey = 300,
    add([1,2,3,{4,{5, 300}}]),
    read(Pubkey).
    
