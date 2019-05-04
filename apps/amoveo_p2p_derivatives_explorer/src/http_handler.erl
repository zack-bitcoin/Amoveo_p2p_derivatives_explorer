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
    {ok, 0};
doit({oracle, OID}) ->
    {ok, 0};
doit({get_offers, L}) ->%list of CIDs
    {ok, 0};
doit({get_offer_contract, CID}) ->
    {ok, 0};
doit({add, C}) ->
    {ok, 0}.
%doit({work, Nonce, Pubkey}) ->
    %io:fwrite("attempted work \n"),
%    mining_pool_server:receive_work(Nonce, Pubkey, IP).
    

pub_split(<<Pubkey:704>>) ->
    {<<Pubkey:704>>, 0};
pub_split(PubkeyWithWorkerID) ->
    <<Pubkey:704, _, ID/binary>> = 
	PubkeyWithWorkerID,
    {<<Pubkey:704>>, base64:encode(ID)}.
