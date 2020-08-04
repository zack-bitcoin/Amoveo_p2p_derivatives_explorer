-module(utils).
-export([cron_job/2, off/0, server_url/1, talk/1,
         trade_id/2, trade_id/1]).

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
    spawn(fun() -> 
                  timer:sleep(1000),
                  cron2(F, Period) end).
cron2(F, P) ->
    spawn(F),
    timer:sleep(P * 1000),
    cron2(F, P).

off() ->
    amoveo_p2p_derivatives_explorer_sup:stop(),
    ok = application:stop(amoveo_p2p_derivatives_explorer).
    
trade_id(Salt, Pub) ->
    hash:doit(<<Pub/binary,
                Salt/binary>>).
-record(swap_offer, {
          acc1, start_limit, end_limit, salt,
          amount1, cid1, type1, %this is what acc1 gives.
          amount2, cid2, type2, %this is what acc2 gives.
          fee1, %what acc1 pays in fees
          fee2}).
trade_id(SO) ->
    #swap_offer{
          salt = Salt,
          acc1 = Acc
         } = SO,
    trade_id(Salt, Acc).
             

