-module(utils).
-export([cron_job/2]).

cron_job(Period, F) ->
    spawn(fun() -> cron2(F, Period) end).
cron2(F, P) ->
    timer:sleep(P * 1000),
    spawn(fun() -> F end),
    cron2(F, P).
    
