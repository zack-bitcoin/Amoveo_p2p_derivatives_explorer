-module(swap_full).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([read/1, add/2, remove/1]).

%for storing the entire data of swap offers, and oracles used to settle them.

%-record(swap_offer, {
%          acc1, start_limit, end_limit, salt,
%          amount1, cid1, type1, %this is what acc1 gives.
%          amount2, cid2, type2, %this is what acc2 gives.
%          fee1, %what acc1 pays in fees
%          fee2}).


init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, ID, S}, X) -> 
    X2 = dict:store(ID, S, X),
    {noreply, X2};
handle_cast({remove, ID}, X) -> 
    X2 = dict:erase(ID, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    Z = dict:find(ID, X),
    {reply, Z, X};
handle_call(_, _From, X) -> {reply, X, X}.

read(ID) -> gen_server:call(?MODULE, {read, ID}).
add(ID, S) -> gen_server:cast(?MODULE, {add, ID, S}).
remove(ID) -> gen_server:cast(?MODULE, {remove, ID}).

