-module(utils).
-export([cron_job/2, off/0]).

cron_job(Period, F) ->
    spawn(fun() -> cron2(F, Period) end).
cron2(F, P) ->
    spawn(fun() -> F end),
    timer:sleep(P * 1000),
    cron2(F, P).

off() ->
    amoveo_p2p_derivatives_explorer_sup:stop(),
    ok = application:stop(amoveo_p2p_derivatives_explorer).
    
