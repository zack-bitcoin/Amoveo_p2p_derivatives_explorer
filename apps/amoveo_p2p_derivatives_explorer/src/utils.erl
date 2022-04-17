-module(utils).
-export([cron_job/2, off/0, server_url/1, talk/1,
         trade_id/1, market_id/1]).

-include("records.hrl").

test_mode() ->
    case application:get_env(amoveo_p2p_derivatives_explorer, test_mode) of
        {ok, B} -> B;
        _ -> false
    end.

server_url(T) ->
    L = case T of
          internal -> "1";
          external -> "0"
      end,
    TM = test_mode(),
    if 
        TM -> "http://127.0.0.1:301"++L;
        true -> "http://127.0.0.1:808"++L
    end.
talk(X) ->
    TM = test_mode(),
    Port = 
        if
            TM -> 3011;
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
trade_id(SO) when is_record(SO, swap_offer2)->
    #swap_offer2{
          salt = Salt,
          acc1 = Acc
         } = SO,
    trade_id(Salt, Acc);
trade_id(SO) when is_record(SO, swap_offer)->
    #swap_offer{
          salt = Salt,
          acc1 = Acc
         } = SO,
    trade_id(Salt, Acc).
             
market_id(S) when is_record(S, swap_offer2) ->
    #swap_offer2{
    cid1 = CID1,
    type1 = T1,
    cid2 = CID2,
    type2 = T2
   } = S,
    make_id(CID1, T1, CID2, T2);
%    hash:doit(<<CID1/binary,
%                T1:32,
%                CID2/binary,
%                T2:32>>);
market_id(S) when is_record(S, swap_offer) ->
    #swap_offer{
    cid1 = CID1,
    type1 = T1,
    cid2 = CID2,
    type2 = T2
   } = S,
    make_id(CID1, T1, CID2, T2).
%    hash:doit(<<CID1/binary,
%                T1:32,
%                CID2/binary,
%                T2:32>>).

%make_id(CID1, Type1, CID2, Type2) ->
%    hash:doit(<<CID1/binary,
%                T1:32,
%                CID2/binary,
%                T2:32>>);
make_id(CID1, Type1, CID2, Type2) ->
    <<N1:256>> = CID1,
    <<N2:256>> = CID2,
    if
        ((N1+Type1) =< (N2+Type2)) ->
            X = <<CID1/binary,
                  CID2/binary,
                  Type1:16,
                  Type2:16>>,
            %io:fwrite(base64:encode(X)),
           hash:doit(X);
        true ->
            make_id(CID2, Type2, CID1, Type1)
    end.
