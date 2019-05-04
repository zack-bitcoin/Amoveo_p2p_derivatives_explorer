-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1]).
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    %io:fwrite("http handler got message: "),
    %io:fwrite(Data0),
    %io:fwrite("\n"),
    Data1 = jiffy:decode(Data0),
    Data = packer:unpack_helper(Data1),
    D = packer:pack(doit(Data)),
    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
	     {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
doit({test}) -> {ok, "success"};
doit({oracle_list}) ->
    {ok, volume_order:read()};
doit({oracle, OID}) ->
    {ok, oracles:read(OID)};
doit({get_offers, L}) ->%list of CIDs
    {ok, channel_offers_ram:read(L)};
doit({get_offer_contract, CID}) ->
    {ok, channel_offers_hd:read(CID)};
doit({add, C}) ->
    %check that the oracle exists.
    ML = hd(C),
    SCO = hd(tl(C)),
    NCO = element(2, SCO),
    CID = element(9, NCO),
    OID = lists:nth(9, ML),
    %imsg = [-6, db.bet_direction_val, bet_expires, maxprice, keys.pub(), db.their_address_val, period, db.our_amount_val, db.their_amount_val, oid, height, db.delay, contract_sig, signedPD, spk_nonce, db.oracle_type_val, db.cid, db.bits_val, db.upper_limit, db.lower_limit, db.payment];
    %look up the range the oracle measures.
    %use both limits and the amounts of veo locked up to calculate what price is being traded at.
    %if binary, we can calculate the price from the ratio of moneys locked up.
    Price = ok,
    Type = lists:nth(15, ML),
    io:fwrite(Type),
    Expires = element(4, NCO),
    Nonce = element(3, NCO),
    Creator = element(2, NCO),
    Direction = lists:nth(1, ML),
    Amount1 = 0,
    Amount2 = 0,
    1=2,
    NCO = channel_offers_ram:new(CID, OID, Price, Direction, Expires, Type, Nonce, Creator, Amount1, Amount2),
    true = channel_offers_ram:valid(NCO),
    channel_offers_hd:add(CID, C),
    channel_offers_ram:add(NCO),
    %If the oracle does not exist, then create it.
    {ok, "success"}.
    

