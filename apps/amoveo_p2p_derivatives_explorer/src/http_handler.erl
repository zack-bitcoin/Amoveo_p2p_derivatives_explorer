-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1,
        test/0]).
-include("records.hrl").
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

doit({add, SwapOffer}) ->
    %gives the server a new swap offer.
    true = verify_swap:doit(SwapOffer),
    swap_books:add(SwapOffer),%order book of meta data
    swap_full:add(SwapOffer),%full swap data
    swap_history:add(SwapOffer),%history order. so syncing is fast

    %TODO, possibly add this to this list of active markets.
    {ok, 0};
doit({history, ID, Nonce}) ->
    %returns the history of updates to market ID since Nonce.
    %if Nonce is up to date, it waits a while before responding.
    X = swap_history:read(ID, Nonce),
    {ok, S};
doit({get, ID}) ->
    %returns the current state of market ID
    X = swap_books:read(ID),
    {ok, X};
doit({markets}) ->
    %list of active markets.
    %TODO New gen server
    {ok, "unimplemented"};

%doit({oracle_list, 2}) ->
%    {ok, active_oracles:read()};
%doit({oracle_list}) ->
%    {ok, volume_order:read()};
%doit({oracle, OID}) ->
%    {ok, oracles:read(OID)};
%doit({get_offers, L}) ->%list of CIDs
%    FN = utils:server_url(external),
%    L2 = case talker:talk({txs}, FN) of
%        bad_peer -> L;
%        {ok, Txs} ->
%            BadCIDs = read_filter(Txs),
%            list_subtract(L, BadCIDs)
%         end,
%    {ok, channel_offers_ram:read(L2)};
%doit({get_offers, 2, L}) ->%list of Oracle IDs
%    S1 = lists:map(fun(X) -> oracles:read(X) end,
%                   L),
%    R = get_offers_loop(S1, L, erlang:timestamp(), 60),
    %grab starting state of L.
    %keep checking to see if it changes.
    %if R is 0, nothing changed. if it is 1, then something changed.
%    {ok, R};
%doit({get_offer_contract, CID}) ->
%    {ok, channel_offers_hd:read(CID)};
%doit({add, 2, X}) -> {ok, close_offers:add(X)};
%doit({read, 2, X}) -> {ok, close_offers:read(X)};
doit(X) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X).
   
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) ->
    is_in(X, T).

%test() ->
%    C = <<"[-6,[-6,2,3000,5000,\"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=\",0,1000000,10000,10000,\"6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=\",64239,1000,[-7,2,\"MEQCIEvjwRnANgJrhLfKiPyd3YHSvFXL7XA098Acw9fXrS46AiByuwStQoVjBetI2+GNhmCHA569JSjSxqoAhhAoL+ZRgg==\"],\"AAD67xOHJw/qyEfgU7cTeZnuAErBa/vU3FPQW9ROppWOFnvKAyd8IjBFAiEA8Y2dZkonQU4QXfm6LZqK3les3GP3HlkXRXoJxbiIDY0CIEVn/yOB7CazFCHLeFGUjhk3XkTUVsWYQFkw4Pz2xCPy\",1,2,\"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=\",10,818,0,0],[\"signed\",[\"nc_offer\",\"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=\",10,64339,10000,10000,1000,1000,\"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=\",\"nPwN6mIWS4JUIo2neplltEEifucqc43ytORXToDFtco=\"],\"MEUCIQCwxOaubh3Y7yuPBWZUKJy1jnhqYhLy+U1vRLZNO/pU1AIgX81qJ8HVp0r/Ac48tqG6F7yyYC7gKhcEka4qVk18G9U=\",[-6]]]">>,
%    doit({add, packer:unpack(C)}),
%    http_handler:doit({oracle_list}).

%read_filter([]) -> [];
%read_filter([{signed, T, _, _}|R]) when (element(1, T) == nc_accept) -> 
%    SOffer = element(3, T),
%    {signed, Offer, _, _} = SOffer,
%    CID = element(9, Offer),
%    [CID|read_filter(R)];
%read_filter([_|T]) -> 
%    read_filter(T).
%list_subtract([], _) -> [];
%list_subtract([H|T], L) -> 
%    B = is_in(H, L),
%    if
%        B -> list_subtract(T, L);
%        true -> [H|list_subtract(T, L)]
%    end.


%get_offers_loop(S1, L, TS, TimeLimit) ->
%    Now = erlang:timestamp(),
%    D = timer:now_diff(Now, TS) div 1000000,%in seconds
%    S2 = lists:map(fun(X) -> oracles:read(X) end,
%                   L),%we could use a different database that keeps a record of thetimes when each oracle is changed. That way we don't need to read so much data out of a slow dictionary.
%    if
%        D > TimeLimit -> 0;
%        S1 == S2 ->
%            timer:sleep(100),%miliseconds
%            get_offers_loop(S1, L, TS, TimeLimit);
%        true -> 1
%    end.
            
            
    
    
