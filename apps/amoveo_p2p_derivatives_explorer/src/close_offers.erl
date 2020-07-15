-module(close_offers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1,read/1,clean/0,cron/0,test/0]).
-define(LOC, "close_offers.db").
-define(clean_period, 20).%how often to check if close offers can be removed because they have become invalid.
init(ok) -> 
    process_flag(trap_exit, true),
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
handle_cast({add, Pubkey, CloseOffer}, N) -> 
    TS = erlang:timestamp(),
    ets:insert(?MODULE, {Pubkey, CloseOffer, TS}),
    {noreply, N};
handle_cast({delete, Object}, X) -> 
    ets:delete_object(?MODULE, Object),
    {noreply, X};
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
add(CloseOffer) ->
    [_, _, _, Stx] = CloseOffer,
    Tx = element(2, Stx),
    Pubkey = element(2, Tx),
    gen_server:cast(?MODULE, {add, Pubkey, CloseOffer}).

cron() -> utils:cron_job(?clean_period, fun() -> clean() end).

clean() ->
    ets:foldl(fun(X, _) ->
                      spawn(fun() -> clean2(X) end),
                      0
              end, 
              success, ?MODULE).
clean2(X = {Pubkey, CloseOffer, TS}) ->
    io:fwrite("attempt to clean "),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    [_, _, _, Stx] = CloseOffer,
    Tx = element(2, Stx),
%-record(ctc2, {aid1 = 0, aid2 = 0, fee = 0,
%               id = 0,
%               amount1 = 0, amount2 = 0,
%               upper_limit, lower_limit}).
    CID = element(5, Tx),
    FN = utils:server_url(external),
    FNL = utils:server_url(internal),
    COC = talker:talk({channel, CID}, FNL),
    B = case COC of
            {ok, 0} -> %channel does not exist in the consensus state.
                Expiration = element(8, ctc2),
                {ok, Height} = talker:talk({height}, FN),
                Now = erlang:timestamp(),
                Age = timer:now_diff(Now, TS) div 1000000,%in seconds
                ThreeHours = 3*60*60,
                if
                    (Height > Expiration) -> true;
                    (Age > ThreeHours) -> true;
                    true -> false
                end;
            _ ->
                Closed = element(11, COC),%a flag for if the channel is closed already.
                case Closed of
                    1 -> %the channel is already closed
                        true;
                    0 -> false
                end
        end,
    if
        B -> 
            io:fwrite("removed\n"),
            gen_server:cast(?MODULE, {delete, X});
        true -> ok
    end,
    io:fwrite("clean success \n");
clean2(X) ->
    io:fwrite("close offers failure"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n").
    


    
    


test() ->    
    Pubkey = 300,
    add([1,2,3,{4,{5, 300}}]),
    read(Pubkey).
    
