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
    %verify that it is a valid channel offer
    true = channel_offers_ram:valid(C),
    %check that the oracle exists.
    CID = ok,
    OID = ok,
    Price = ok,
    Direction = ok,
    channel_offers_hd:add(CID, C),
    NCO = channel_offers_ram:new(CID, OID, Price, Direction),
    channel_offers_ram:add(NCO),
    {ok, "success"}.
    

