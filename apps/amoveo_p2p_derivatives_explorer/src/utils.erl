-module(utils).
-export([cron_job/2, off/0, server_url/1, talk/1]).

-define(TestMode, false).

server_url(T) ->
    L = case T of
          internal -> "1";
          external -> "0"
      end,
    if 
        ?TestMode -> "http://127.0.0.1:301"++L;
        true -> "http://127.0.0.1:808"++L
    end.
talk(X) ->
    Port = 
        if
            ?TestMode -> 3011;
            true -> 8081
        end,
    talker:talk(X, {{127,0,0,1}, Port}).
            

cron_job(Period, F) ->
    spawn(fun() -> cron2(F, Period) end).
cron2(F, P) ->
    spawn(fun() -> 
                  timer:sleep(1000),
                  F() end),
    timer:sleep(P * 1000),
    cron2(F, P).

off() ->
    amoveo_p2p_derivatives_explorer_sup:stop(),
    ok = application:stop(amoveo_p2p_derivatives_explorer).
    
