-module(amoveo_p2p_derivatives_explorer_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(keys, [oracles, channel_offers_ram, channel_offers_hd, volume_order]).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(testnet_sup, H),
    child_killer(T).
stop() -> child_killer(?keys).
child_maker([]) -> [];
child_maker([H|T]) -> [?CHILD(H, worker)|child_maker(T)].
init([]) ->
    Children = child_maker(?keys),
    {ok, { {one_for_all, 50000, 1}, Children} }.
