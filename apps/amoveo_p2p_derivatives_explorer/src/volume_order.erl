%lists of all the oracles that we are storing bets for. is ordered by volume of available channel offers in each oracle.

-module(volume_order).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
read/0, add/1, sort/0,
test/0]).
-record(v, {oid, volume}).
-define(LOC, "volume_order").
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> [];
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(sort, X) -> 
    X2 = sort_internal(X),
    {noreply, X2};
handle_cast({add, V}, X) -> 
    X2 = insert(V, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

read() -> gen_server:call(?MODULE, read).
sort() -> gen_server:cast(?MODULE, sort).
add(V) when is_record(V, v) -> 
    gen_server:cast(?MODULE, {add, V}).

insert(V, []) -> [V];
insert(V, [H|T]) ->
    B = (V#v.volume > H#v.volume),
    if
        B -> [V|[H|T]];
        true -> [H|insert(V, T)]
    end.
            
sort_internal(X) ->
    %for every element of X, look up the oracle to calculate the 
    X.

test() ->
    V1 = #v{oid = 1, volume = 1},
    V2 = #v{oid = 2, volume = 2},
    V3 = #v{oid = 3, volume = 3},
    SR = [V3, V2, V1],
    add(V2),
    add(V1),
    add(V3),
    SR = read(),
    sort(),
    SR = read(),
    success.
    
    
