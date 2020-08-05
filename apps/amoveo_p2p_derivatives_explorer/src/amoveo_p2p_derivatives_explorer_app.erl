-module(amoveo_p2p_derivatives_explorer_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    swap_history:garbage_cron(),
    swap_books:garbage_cron(),
    %active_oracles:cron(),
    %volume_order:cron(),
    %oracles:cron(),
    %channel_offers_ram:cron(),
    %close_offers:cron(),
    amoveo_p2p_derivatives_explorer_sup:start_link().
stop(_State) ->
    ok.
start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [
		  {"/:file", file_handler, []},
		  {"/", http_handler, []}
		 ]}]),
    %{ok, Port} = application:get_env(amoveo_mining_pool, port),
    Port = 8090,
    {ok, _} = cowboy:start_http(
                http, 100,
                [{ip, {0, 0, 0, 0}}, {port, Port}],
                [{env, [{dispatch, Dispatch}]}]),
    ok.
    
