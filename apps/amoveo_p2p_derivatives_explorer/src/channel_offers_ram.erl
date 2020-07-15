-module(channel_offers_ram).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
add/1, remove/1, read/1, clean/0, new/9, cron/0,
amount1/1, amount2/1, oid/1, cid/1, direction/1, price/1,
valid/1, all/0,
test/0]).
-record(channel_offer, {cid, oid, price, direction, expires, nonce, amount1, amount2,
                       type,%can be binary or scalar.
                        creator
                       }).
amount1(X) -> X#channel_offer.amount1.
amount2(X) -> X#channel_offer.amount2.
oid(X) -> X#channel_offer.oid.
cid(X) -> X#channel_offer.cid.
direction(X) -> X#channel_offer.direction.
price(X) -> X#channel_offer.price.
              
-define(LOC, "channel_offers_ram.db").
-define(clean_period, 20).%how often to check if channel offers can be removed because they have become invalid.

new(CID, OID, Price, Direction, Expires, Type, Creator, Amount1, Amount2) ->
    #channel_offer{cid = CID, oid = OID, price = Price, direction = Direction, expires = Expires, type = Type, nonce = 0, creator = Creator, amount1 = Amount1, amount2 = Amount2}.

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("channel offers ram died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, C}, X) -> 
    K = C#channel_offer.cid,
    X2 = dict:store(K, C, X),
    {noreply, X2};
handle_cast({remove, CID}, X) -> 
    X2 = dict:erase(CID, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, L}, _From, X) -> 
    Y = read_loop(L, X),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

read_loop([], _) -> [];
read_loop([H|T], D) ->
    case dict:find(H, D) of
        error -> read_loop(T, D);
        {ok, X} -> [X|read_loop(T, D)]
    end.
                                 
add(C) ->
    %cid is the key for storing in a dict.
    gen_server:cast(?MODULE, {add, C}).
remove(CID) ->
    gen_server:cast(?MODULE, {remove, CID}).
read(L) when is_list(L) ->%list of cids
    %TODO, we should filter this list to remove anything that is in the mempool:
    %io:fwrite(packer:pack([L, L2])),
    %io:fwrite("\n"),
    %remove these cids from L.
    gen_server:call(?MODULE, {read, L}).
all() -> gen_server:call(?MODULE, all).

clean() ->
    D = all(),
    K = dict:fetch_keys(D),
    clean2(K, D).
clean2([], _) -> ok;
clean2([H|T], D) ->
    V = dict:fetch(H, D),
    B = valid(V),
    if
        B -> ok;
        true -> 
            remove(H),
            channel_offers_hd:remove(H)
    end,
    clean2(T, D).

cron() -> utils:cron_job(?clean_period, fun() -> clean() end).
    

valid(C) ->
    %#channel_offer{cid = CID, oid = OID, price = Price, direction = Direction, expires, type = Type, creator}.
    FN = utils:server_url(external),
    FNL = utils:server_url(internal),
    {ok, Height} = talker:talk({height}, FN),
    {ok, COC} = talker:talk({channel, C#channel_offer.cid}, FNL),
    if
        not(COC == 0) ->
            if
                (element(11, C) == 1) ->
                    %channel was closed, we don't need this offer any more.
                    false;
                true ->
                    %channel is active, keep this offer.
                    true
            end;
        ((Height + 1) >= C#channel_offer.expires) -> 
            io:fwrite("ran out of time"),
            false; %offer is expired without the channel being created. so we can delete it.
        true ->
            %check if the account can afford to pay this offer.
            COA = C#channel_offer.amount1,
            {ok, Acc} = talker:talk({account, C#channel_offer.creator}, FNL),
            if 
                ((Acc == "empty") or ((Acc == empty) or (Acc == 0))) -> 
                    io:fwrite("account does not exist \n"),
                    io:fwrite(FNL),
                    io:fwrite("\n"),
                    io:fwrite(packer:pack([C#channel_offer.creator, Acc])),
                    io:fwrite("\n"),
                    false;
                true -> 
                    AB = element(2, Acc),
                    AB > COA
            end
    end.

test() ->
    CID = <<>>,
    C = #channel_offer{cid = CID},
    add(C),
    [C] = read([CID]),
    remove(CID),
    [] = read([CID]),
    success.
