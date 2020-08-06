-module(swap_verify).
-export([doit/2, keep_longer/3]).

-include("records.hrl").
doit(TID, S) ->
    FN = utils:server_url(external),
    {ok, Height} = talker:talk({height}, FN),
    doit(TID, S, Height).
doit(TID, S, Height) ->
    FN = utils:server_url(external),
    FNL = utils:server_url(internal),

    Offer = element(2, S),
    #swap_offer{
                 start_limit = SL,
                 fee1 = Fee1,
                 fee2 = Fee2,
                 amount1 = Amount1,
                 cid1 = CID1,
                 cid2 = CID2,
                 acc1 = Acc1
               } = Offer,
    true = sign:verify_sig(Offer, element(3, S), Acc1),

    %check acc1 is putting some minimum amount into it.
    true = Fee1 + Amount1 > 1000000,

    %check that acc2's fee is reasonable
    true = Fee2 < 200000,

    %check that we aren't already storing a trade with this id
    true = error == swap_full:read(TID),

    %check that the start height limit has already occured.
    true = (Height >= SL),

    %TODO verify that all the contract ids are in the binary_contracts database. `binary_contracts:read(CID)`
    false = (error == binary_contracts:read_contract(CID1)),
    false = (error == binary_contracts:read_contract(CID2)),

    keep_longer(Offer, Height, TID).

keep_longer(Offer, Height, TID) ->
    FNL = utils:server_url(internal),
    FN = utils:server_url(external),
    #swap_offer{
             end_limit = EL,
             amount2 = Amount2,
             salt  = Salt,
             acc1 = Acc1,
             cid1 = CID1,
             nonce = Nonce
          } = Offer,

    %check that it isn't expired.
    B2 = (Height =< EL),

    %check the nonce.
    B3 = case CID1 of
             <<0:256>> ->
                 {ok, Acc} = talker:talk({account, Acc1}, FNL),
                 Acc#acc.nonce =< Nonce;
             _ ->
       %TODO, if the offer is sending subcurrency, then the nonce should be for a sub-account.
                 false
         end,

    %check that the trade id isn't already consumed
    B4 = {ok, 0} == talker:talk({trade, TID}, FNL),

    %TODO, check that the swap isn't already in the tx pool.
    {ok, Txs} = talker:talk({txs}, FN),
    B5 = no_repeats(Acc1, Salt, Txs),

    B6 = B2 and B3 and B4 and B5,
    Verbose = false,
    if
        (Verbose and not(B6)) ->
            io:fwrite(packer:pack(swap_full:read(TID))),
            io:fwrite("\n"),
            io:fwrite(packer:pack([B2, B3, B4, B5])),
            io:fwrite("\n"),
            ok;
        true -> ok
    end,
    B6.
    
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
    
