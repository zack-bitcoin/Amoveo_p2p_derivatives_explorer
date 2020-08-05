-module(swap_verify).
-export([doit/2]).

-include("records.hrl").
doit(TID, S) ->
    FN = utils:server_url(external),
    {ok, Height} = talker:talk({height}, FN),
    doit(TID, S, Height).
doit(TID, S, Height) ->
    FN = utils:server_url(external),
    FNL = utils:server_url(internal),

    Offer = element(2, S),
    Pub = element(2, Offer),
    true = sign:verify_sig(Offer, element(3, S), Pub),
    #swap_offer{
                 end_limit = EL,
                 start_limit = SL,
                 fee1 = Fee1,
                 fee2 = Fee2,
                 amount1 = Amount1,
                 amount2 = Amount2,
                 salt  = Salt,
                 acc1 = Acc1,
                 cid1 = CID1,
                 nonce = Nonce
               } = Offer,

    %check the nonce.
    case CID1 of
        <<0:256>> ->
            {ok, Acc} = talker:talk({account, Pub}, FNL),
            true = Acc#acc.nonce =< Nonce;
        _ ->
       %TODO, if the offer is sending subcurrency, then the nonce should be for a sub-account.
            1=2
    end,

    %check acc1 is putting some minimum amount into it.
    true = Fee1 + Amount1 > 1000000,

    %check that acc2's fee is reasonable
    true = Fee2 < 200000,

    %check that it isn't expired.
    B1 = (Height >= SL),
    B2 = (Height =< EL),

    %check that we aren't already storing a trade with this id
    B3 = error == swap_full:read(TID),

    %check that the trade id isn't already consumed
    B4 = {ok, 0} == talker:talk({trade, TID}, FNL),

    %TODO, check that the swap isn't already in the tx pool.
    {ok, Txs} = talker:talk({txs}, FN),
    B5 = no_repeats(Acc1, Salt, Txs),

    B1 and B2 and B3 and B4 and B5.
    
no_repeats(_, _, []) -> true;
no_repeats(Acc, Salt, 
           [{signed, 
             #swap_tx{
               offer = 
                   {signed,
                    #swap_offer{
                      acc1 = Acc,
                      salt = Salt
                     },
                    _, _}
              }, _, _}|_]) -> 
    false;
no_repeats(Acc, Salt, [_|T]) -> 
    no_repeats(Acc, Salt, T).
    
