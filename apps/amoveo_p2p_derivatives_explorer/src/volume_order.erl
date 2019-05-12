%lists of all the oracles that we are storing bets for. is ordered by volume of available channel offers in each oracle.

-module(volume_order).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
read/0, add/2, sort/0, cron/0,
test/0]).
-record(v, {oid, volume = 0}).
-define(LOC, "volume_order.db").
-define(sort_period, 300).%how often to sort orders by volume. 
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
add(OID, A) ->
    R = read(),
    B = is_in(OID, R),
    if
        B -> ok;
        true ->
            V = #v{oid = OID, volume = A},
            gen_server:cast(?MODULE, {add, V})
    end.
is_in(_, []) -> false;
is_in(OID, [H|T]) ->
    if
        (OID == H#v.oid) -> true;
        true -> is_in(OID, T)
    end.
    

insert(V, []) -> [V];
insert(V, [H|T]) ->
    B = (V#v.volume > H#v.volume),
    if
        B -> [V|[H|T]];
        true -> [H|insert(V, T)]
    end.
            
sort_internal(X) ->
    %for every element of X, look up the oracle to calculate the 
    Volumes = sr_volumes(X), 
    %io:fwrite("volumes are \n"),
    %io:fwrite(Volumes),
    %io:fwrite("volumes are \n"),
    SV = sort_by_volume(Volumes),
    OIDS = grab_oids(SV),
    OIDS.
listify([]) -> [];
listify([H|T]) -> [[H]|T].
sort_by_volume(L) ->
    L2 = listify(L),
    L3 = merge_sort(L2),
    case L3 of
        [] -> [];
        [X] -> X
    end.
merge_sort([]) -> [];
merge_sort([X]) -> [X];
merge_sort(X) ->
    X2 = merge_sort2(X),
    merge_sort(X2).
merge_sort2([]) -> [];
merge_sort2([X]) -> [X];
merge_sort2([H|[H2|T]]) ->
    H3 = merge(H, H2),
    [H3|merge_sort2(T)].
merge([], X) -> X;
merge(X, []) -> X;
merge([{V1, OID1}|T1], E2 = [{V2, _}|_]) when V1 > V2->
    [{V1, OID1}|merge(T1, E2)];
merge(E1, [{V2, OID2}|T2]) ->
    [{V2, OID2}|merge(E1, T2)].
grab_oids([]) -> [];
grab_oids([{_, OID}|T]) ->
    [OID|grab_oids(T)].
sr_volumes([]) -> [];
sr_volumes([H|T]) ->
    %io:fwrite(H),
    %io:fwrite(oracles:read(H)),
    {ok, O} = oracles:read(H#v.oid),
    Trades = oracles:buys(O) ++ oracles:sells(O),
    %io:fwrite(Trades),%[OID1]
    %io:fwrite(" trades\n"),
    Cs = channel_offers_ram:read(Trades),
    %io:fwrite(Cs),
    %io:fwrite(" cs \n"),
    TV = sr_trades_volume(Cs),
    if
        (TV == 0) ->
            oracles:remove(H#v.oid),
            sr_volumes(T);
        true ->
            [{TV, H#v.oid}|sr_volumes(T)]
    end.
sr_trades_volume([]) -> 0;
sr_trades_volume([C|T]) ->
    %C = channel_offers_ram:read(H),
    A = channel_offers_ram:amount1(C) +
        channel_offers_ram:amount2(C),
    A + sr_trades_volume(T).
cron() -> utils:cron_job(?sort_period, fun() -> sort() end).
                  
                  

test() ->
    V1 = #v{oid = 1, volume = 1},
    V2 = #v{oid = 2, volume = 2},
    V3 = #v{oid = 3, volume = 3},
    SR = [V3, V2, V1],
    add(V2, 0),
    add(V1, 0),
    add(V3, 0),
    SR = read(),
    sort(),
    SR = read(),
    success.
    
    
