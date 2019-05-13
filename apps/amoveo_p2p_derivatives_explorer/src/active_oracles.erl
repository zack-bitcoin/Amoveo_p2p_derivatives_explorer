-module(active_oracles).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
refresh/0, read/0, 
test/0]).
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
    {ok, X} = talker:talk({oracles}, {{127,0,0,1}, 8081}),
    % [{<<"oracle question text, {oracle, ...}}|_]
    % {oracle, id, result, question, starts, type, orders, orders_hash, creator, done_timer, governance, governance_amount}
    %only keep ones where the result is 0
    %if several oids are consecutive, only keep the zeroth bit one.
    X.
test() ->
    get_active_oracles().
    

read() ->
    gen_server:call(?MODULE, read).
refresh() ->
    gen_server:cast(?MODULE, refresh).
