-module(amoveo_p2p_derivatives_explorer_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    swap_history:garbage_cron(),
    swap_books:garbage_cron(),
    spawn(fun() -> scalar_contracts:cron() end),
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
    IP = {0,0,0,0},
    {ok, _} = cowboy:start_clear(
                http,
                [{ip, IP}, {port, Port}],
                #{env => #{dispatch => Dispatch}}),
    ok.
    
