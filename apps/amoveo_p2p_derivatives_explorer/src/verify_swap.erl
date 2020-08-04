-module(verify_swap).
-export([doit/1]).

-include("records.hrl").
doit(S) ->
    FN = utils:server_url(external),
    {ok, Height} = talker:talk({height}, FN),
    doit(S, Height).
doit(S, Height) ->
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

    %check that it isn't expired.
    true = (Height >= SL),
    true = (Height =< EL),

    %check acc1 is putting some minimum amount into it.
    true = Fee1 + Amount1 > 1000000,

    %check that the trade id is not already consumed.
    TID = hash:doit(<<Acc1/binary, Salt/binary>>),
    %{ok, [_, TopHash]} = talker:talk({top, 1}, FNL),
    %{ok, empty} = talker:talk({proof, "trades", TID, TopHash}, FN),
    {ok, 0} = talker:talk({trade, TID}, FNL),

    %TODO, check that the swap isn't already in the tx pool.
    {ok, Txs} = talker:talk({txs}, FN),
    true = no_repeats(Acc1, Salt, Txs),

    true.
    
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
    
