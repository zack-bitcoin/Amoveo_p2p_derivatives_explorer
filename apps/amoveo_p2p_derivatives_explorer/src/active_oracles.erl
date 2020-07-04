-module(active_oracles).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
refresh/0, read/0, 
cron/0,
test/0]).
-define(refresh_period, 20).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(refresh, _) -> 
    X = get_active_oracles(),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

get_active_oracles() ->
    {ok, X} = utils:talk({oracles}),
    {ok, Height} = utils:talk({height}),
    X2 = result0(X), %only keep ones where the result is 0
    X4 = live(X2, Height),
    % [{<<"oracle question text, {oracle, ...}}|_]
    % {oracle, id, result, question, starts, type, orders, orders_hash, creator, done_timer, governance, governance_amount}
    %if several oids are consecutive, only keep the zeroth bit one.
    X3 = consecutive_oids(X4),
    X3.
live([], _) -> [];
live([{Q, O}|T], Height) ->
    S = element(5, O),
    if
        S < Height -> live(T, Height);
        true -> [{Q, O}|live(T, Height)]
    end.
            
consecutive_oids(X2) ->
    X3 = listify(X2),
    X4 = merge_sort(X3),
    remove_consecutive(X4).
remove_consecutive([]) -> [];
remove_consecutive([X]) -> [X];
remove_consecutive([{Q1, O1}|[{Q2, O2}|T]]) ->
    ID1 = element(2, O1),
    ID2 = element(2, O2),
    <<N1:256>> = ID1,
    <<N2:256>> = ID2,
    if
        (N2 == (N1 + 1)) -> %remove_consecutive2;
            [{Q1, O1}|remove_consecutive2([{Q2, O2}|T])];
        true -> %remove_consecutive
            [{Q1, O1}|remove_consecutive([{Q2, O2}|T])]
    end.
%[{Q1, O1}|F([{Q2, O2}|T])].
remove_consecutive2([]) -> [];
remove_consecutive2([X]) -> [];
remove_consecutive2([{_Q1, O1}|[{Q2, O2}|T]]) ->
    ID1 = element(2, O1),
    ID2 = element(2, O2),
    <<N1:256>> = ID1,
    <<N2:256>> = ID2,
    if
        (N2 == (N1 + 1)) -> remove_consecutive2([{Q2, O2}|T]);
            %true -> [{Q1, O1}|remove_consecutive([{Q2, O2}|T])]
        true -> remove_consecutive([{Q2, O2}|T])
    end.
            
    
listify([]) -> [];
listify([H|T]) -> [[H]|listify(T)].
merge_sort([]) -> [];
merge_sort([X]) -> X;
merge_sort(L) ->
    merge_sort(merge_improve(L)).
merge_improve([]) -> [];
merge_improve([X]) -> [X];
merge_improve([H|[H2|T]]) ->
    [merge(H, H2)|merge_improve(T)].
merge([], X) -> X;
merge(X, []) -> X;
merge([{Q1, O1}|T1], [{Q2, O2}|T2]) ->
    ID1 = element(2, O1),
    ID2 = element(2, O2),
    <<N1:256>> = ID1,
    <<N2:256>> = ID2,
    if
        (N1 < N2) -> [{Q1, O1}|merge(T1, [{Q2, O2}|T2])];
        true -> [{Q2, O2}|merge([{Q1, O1}|T1], T2)]
    end.
            
    
result0([]) -> [];
result0([{Q, Oracle}|T]) ->
    case element(3, Oracle) of
        0 -> [{Q, Oracle}|result0(T)];
        _ -> result0(T)
    end.
test() ->
    get_active_oracles().
    

read() ->
    gen_server:call(?MODULE, read).
refresh() ->
    gen_server:cast(?MODULE, refresh).


cron() -> utils:cron_job(?refresh_period, fun() -> refresh() end).
    
