-module(binary_contracts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        add/2, exists/2, read_oracle/1, read_contract/1]).

%this is for storing the data needed to enforce the outcome for any binary contract type subcurrency being used
%so if you want to post a swap tx, the subcurrencies being swapped need to have enforcement data.
-define(LOC, "binary_contracts.db").

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
    io:format("binary contracts died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, CID, Text, Height}, X) -> 
    Now = erlang:timestamp(),
    X2 = dict:store(CID, {Text, Height, Now}, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

add(OracleText, OracleHeight) ->
    CID = cid_maker(OracleText, OracleHeight),
    gen_server:cast(?MODULE, {add, CID, OracleText, OracleHeight}). 

exists(OracleText, OracleHeight) ->
    CID = cid_maker(OracleText, OracleHeight),
    case gen_server:call(?MODULE, {check, CID}) of
        error -> false;
        _ -> true
    end.

read_contract(<<0:256>>) -> 
    true;
read_contract(CID) -> 
    gen_server:call(?MODULE, {check, CID}). 

read_oracle(OracleID) ->
    %return oracle text and oracle height
    CID = cid_maker(OracleID),
    read_contract(CID).

cid_maker(OracleText, OracleHeight) ->
    true = is_binary(OracleText),
    QuestionHash = hash:doit(OracleText),
    OID = hash:doit(<<OracleHeight:32,
                      0:32,
                      0:32,
                      QuestionHash/binary>>),
    cid_maker(OID).
cid_maker(OID) ->
    BinaryCodeStatic = <<" OID ! \
macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
macro even_split [ int 2147483648 , int 2147483647 , int 0 ] ; \
macro maximum int 4294967295 ; \
 car drop car swap drop car swap drop car drop \
int 32 split \
 OID @ \
 == if else fail then \
drop drop int 1 split swap drop binary 3 AAAA swap ++ \
int 3 == if \
  [ int 0 , int 0 , maximum ] int 0 int 1000 \
else drop \
  int 1 == if \
      [ maximum , int 0 , int 0 ] int 0 int 1000 \
    else drop \
    int 2 == if \
      [ int 0 , maximum, int 0 ] int 0 int 1000 \
    else drop drop \
        even_split int 5000 int 10 \
    then \
  then \
then ">>,
    BinaryCodeInner = <<" binary 32 ",
                        (base64:encode(OID))/binary, 
                        BinaryCodeStatic/binary
                      >>,
    ContractBytecode = compiler_chalang:doit(BinaryCodeInner),
    BinaryHash = hash:doit(ContractBytecode),
    %BinaryCode = <<" def ",
    %               BinaryCodeInner/binary,
    %               " ; ">>,
    %BinaryDerivative = compiler_chalang:doit(BinaryCode),
    %BinaryHash = hd(vm(BinaryDerivative)),
    hash:doit(<<BinaryHash/binary,
                          0:256,
                          3:16,
                          0:16>>).
%vm(Code) ->
%    ExampleData = chalang:data_maker(1000000,1000000,1000,1000,<<>>,<<>>,chalang:new_state(0,0,0),32,2,false),
%    chalang:stack(chalang:run5(Code, ExampleData)).

