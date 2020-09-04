-module(binary_contracts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        add/2, add/4, read_contract/1]).

%this is for storing the data needed to enforce the outcome for any binary contract type subcurrency being used
%so if you want to post a swap tx, the subcurrencies being swapped need to have enforcement data.
-define(LOC, "binary_contracts.db").

-record(binary, {text, height, now, source = <<0:256>>, source_type = 0}).

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
handle_cast({add, CID, Text, Height, Source, SourceType}, X) -> 
    Now = erlang:timestamp(),
    C = #binary{text = Text, 
                height = Height, 
                now = Now, 
                source = Source, 
                source_type = SourceType},
    X2 = dict:store(CID, C, X),
    {noreply, X2};
handle_cast({add, CID, Text, Height}, X) -> 
    Now = erlang:timestamp(),
    C = #binary{text = Text, height = Height, now = Now},
    X2 = dict:store(CID, C, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

add(OracleText, OracleHeight, Source, SourceType) ->
    CID = cid_maker(OracleText, OracleHeight, Source, SourceType),
    gen_server:cast(?MODULE, {add, CID, OracleText, OracleHeight, Source, SourceType}),
    CID.
add(OracleText, OracleHeight) ->
    CID = cid_maker(OracleText, OracleHeight),
    gen_server:cast(?MODULE, {add, CID, OracleText, OracleHeight}),
    CID.

read_contract(<<0:256>>) -> 
    true;
read_contract(CID) -> 
    gen_server:call(?MODULE, {check, CID}). 


cid_maker(OracleText, OracleHeight) ->
    cid_maker(OracleText, OracleHeight, <<0:256>>, 0).
cid_maker(OracleText, OracleHeight, Source, SourceType) ->
    true = is_binary(OracleText),
    QuestionHash = hash:doit(OracleText),
    OID = hash:doit(<<OracleHeight:32,
                      0:32,
                      0:32,
                      QuestionHash/binary>>),
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
    ToHash = <<BinaryHash/binary,
               Source/binary,
               3:16,
               SourceType:16>>,
    CID = hash:doit(ToHash),
    io:fwrite("binary contracts tohash is "),
    io:fwrite(packer:pack(ToHash)),
    io:fwrite("\n"),
    io:fwrite("cid is \n"),
    io:fwrite(packer:pack(CID)),
    io:fwrite("\n"),
    CID.
%vm(Code) ->
%    ExampleData = chalang:data_maker(1000000,1000000,1000,1000,<<>>,<<>>,chalang:new_state(0,0,0),32,2,false),
%    chalang:stack(chalang:run5(Code, ExampleData)).

