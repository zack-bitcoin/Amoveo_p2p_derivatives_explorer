-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1,
        test/0]).
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
doit({oracle_list, 2}) ->
    {ok, active_oracles:read()};
doit({oracle_list}) ->
    {ok, volume_order:read()};
doit({oracle, OID}) ->
    {ok, oracles:read(OID)};
doit({get_offers, L}) ->%list of CIDs
    io:fwrite("get offers api call\n"),
    {ok, channel_offers_ram:read(L)};
doit({get_offer_contract, CID}) ->
    {ok, channel_offers_hd:read(CID)};
doit({add, C}) ->
    %check that the oracle exists.
    %C = packer:unpack(C0),
    ML = hd(C),%message list
    SCO = hd(tl(C)),%signed channel offer
    %Rest = tl(tl(C)),
    NCO = element(2, SCO),
    CID = element(9, NCO),
    OID = lists:nth(9, ML),
    Question = lists:nth(23, ML),
    OracleStarts = lists:nth(22, ML),
    %look up the range the oracle measures.
    %use both limits and the amounts of veo locked up to calculate what price is being traded at.
    %if binary, we can calculate the price from the ratio of moneys locked up.
    Type = lists:nth(15, ML),
    %io:fwrite(Type),%2
    Amount1 = lists:nth(7, ML),
    Amount2 = lists:nth(8, ML),
    Direction = lists:nth(1, ML),
    Price = case Type of
                1 -> %binary
                    Amount2 / Amount1;
                2 -> %scalar
                    UL = lists:nth(18, ML),
                    LL = lists:nth(19, ML),
                    true = ((LL == 0) or (UL == 1023)),%only accept leverage of 1 for now.
                    Price0 = (UL + LL) div 2,
                    case Direction of
                        1 -> Price0;
                        2 -> 1023 - Price0
                    end
            end,
    Expires = element(4, NCO),
    Nonce = element(3, NCO),
    Creator = element(2, NCO),
    NCO2 = channel_offers_ram:new(CID, OID, Price, Direction, Expires, Type, Creator, Amount1, Amount2),
    true = channel_offers_ram:valid(NCO2),
    channel_offers_hd:add(CID, C),
    channel_offers_ram:add(NCO2),
    Oracle0 = oracles:read(OID),
    Oracle = case Oracle0 of
                 error -> 
                     %NewOracle = oracles:new(OID, Rest);
                     NewOracle = oracles:new(OID, Question, OracleStarts);
                 {ok, X} -> X
             end,
    Oracle2 = oracles:add_trade(Oracle, NCO2),
    oracles:add(Oracle2),
    %io:fwrite([Amount1, Amount2]),
    C_OIDs = volume_order:read(),
    B = is_in(OID, C_OIDs),
    if 
        B -> ok;
        true ->
            %if the oracle doesn't exist, create it.
            volume_order:add(OID, Amount1+Amount2)
    end,
    {ok, "success"};
doit({add, 2, X}) -> {ok, close_offers:add(X)};
doit({read, 2, X}) -> {ok, close_offers:read(X)};
doit(X) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X).
   
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) ->
    is_in(X, T).

test() ->
    C = <<"[-6,[-6,2,3000,5000,\"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=\",0,1000000,10000,10000,\"6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=\",64239,1000,[-7,2,\"MEQCIEvjwRnANgJrhLfKiPyd3YHSvFXL7XA098Acw9fXrS46AiByuwStQoVjBetI2+GNhmCHA569JSjSxqoAhhAoL+ZRgg==\"],\"AAD67xOHJw/qyEfgU7cTeZnuAErBa/vU3FPQW9ROppWOFnvKAyd8IjBFAiEA8Y2dZkonQU4QXfm6LZqK3les3GP3HlkXRXoJxbiIDY0CIEVn/yOB7CazFCHLeFGUjhk3XkTUVsWYQFkw4Pz2xCPy\",1,2,\"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=\",10,818,0,0],[\"signed\",[\"nc_offer\",\"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=\",10,64339,10000,10000,1000,1000,\"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=\",\"nPwN6mIWS4JUIo2neplltEEifucqc43ytORXToDFtco=\"],\"MEUCIQCwxOaubh3Y7yuPBWZUKJy1jnhqYhLy+U1vRLZNO/pU1AIgX81qJ8HVp0r/Ac48tqG6F7yyYC7gKhcEka4qVk18G9U=\",[-6]]]">>,
    doit({add, packer:unpack(C)}),
    http_handler:doit({oracle_list}).
