%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2019 19:48
%%%-------------------------------------------------------------------
-module(masterFunction).
-author("amir").
-behavior(gen_server).
%% API
%% API
-export([start/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, callFunc/1, castFunc/1]).
-export([count/0, timerSupervisor/0, getTimeStamp/0, test/0, terminatorLoop/0, initCustomer/1, waitForCustomerToLeave/0]).
-define(DEPARTMENT_LIST, [dairy, meat, bakery]).
-define(LOGGER_FILE_PATH, "../Logger-masterFunction.txt").
-define(NUMBER_OF_ITERATIONS, 1000000).
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-define(SECURITY1, security1).
-define(SECURITY2, security2).
-define(SUPPLIER, supplierProc).
-export([getNumberOfCustomers/0]).
%%-export([periodicallyRestockInventory/0]).


start()->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().


initErlMarketFunctionality() ->
  put(numberOfCustomers,0),
  global:register_name(?TERMINATOR, spawn(?MODULE, terminatorLoop, [])),
  global:register_name(?TIMER, spawn(?MODULE, timerSupervisor, [])),
  globalRegisterMasterFunction(),
  writeToLogger("strating initialization"),
  initDepartments(?DEPARTMENT_LIST),
  initPurchaseDepartment(),
  initCashiers(),
  %%global:register_name(?SUPPLIER, spawn(?MODULE, periodicallyRestockInventory, [])),
  global:register_name(?SECURITY1, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 100) ])),
  %global:register_name(?SECURITY2, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 100) ])),
  {ok, normal}.

globalRegisterMasterFunction() ->
  global:register_name(masterFunction, self()).

handle_call(getTimeStamp, _From, State) ->
  global:send(?TIMER, {getTimeStamp, self()}),
  receive
    {timeStamp, TimeStamp} -> {TimeStamp, ok, State}
  end;


handle_call(getNumberOfCustomers, _From, State) ->
  writeToLogger("getNumberofCustomers reached"),
  Reply = getNumberOfCustomers(),
  {reply, Reply, State}.

handle_cast(createCustomer, State) ->
  updateNumberOfCustomers("create"),
  {noreply, State};

handle_cast(closeShop, State) ->
  writeToLogger("im at close shop"),
  global:send(?SECURITY1, {terminate}),
  writeToLogger("im at close shop"),
  %global:send(?SECURITY2, {terminate}),
  writeToLogger("im at close shop"),
  spawn(?MODULE, waitForCustomerToLeave, []),
  writeToLogger("im at close shop"),
  writeToLogger("exiting close shop"),
  {noreply, State};

handle_cast(customerOut, State) ->
  updateNumberOfCustomers("terminate"),
  {noreply, State};

handle_cast(terminate, State) ->
  %terminate(0,0),
  {stop, normal, State}.

handle_info(Info, State) ->
  writeToLogger("masterFunction recieved: ", Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  writeToLogger("Master Function says Bye Bye"),
  global:send(?TIMER, {terminate}),
  ok.

%% @doc interface function for using gen_server call
callFunc(Message) ->
  gen_server:call({global,?MODULE}, Message).

%% @doc interface function for using gen_server cast
castFunc(Message) ->
  gen_server:cast({global, ?MODULE}, Message).


initErlMarketDataBase() ->
  inventory:initInventory(node()).

initDepartments(DepartmentList) ->
  writeToLogger("Initializaing Departments"),
  lists:foreach(fun(DepartmentName) ->
    department:start(DepartmentName)
                end, DepartmentList).

terminateDepartments(DepartmentList) ->
  writeToLogger("terminating Departments"),
  lists:foreach(fun(DepartmentName) ->
                 department:castFunc(DepartmentName, terminate)
                end, DepartmentList).

waitForCustomerToLeave()->
  timer:sleep(500),
  NumberOfCustomers = callFunc(getNumberOfCustomers),
  writeToLogger(variable, "Shop is closed: ~p  Customer remain ~n",[NumberOfCustomers]),
  if
    NumberOfCustomers == 0 ->  writeToLogger("Shop is closed: all customers left~n"),
                               castFunc(terminate),
                               exit(normal);
    true -> waitForCustomerToLeave()
  end.

%%periodicallyRestockInventory()->
%%  % TODO delete this functio and the supplier Process
%%  receive
%%    {terminate} -> writeToLogger("Supplier Out"),
%%                   exit(normal);
%%          MSG -> writeToLogger(variable,"Got Msg: ~p~n",[MSG])
%%    after 2000 ->
%%      writeToLogger("Restocking inventory"),
%%      spawn(inventory, fillInventory, []),
%%      periodicallyRestockInventory()
%%  end.



initPurchaseDepartment() ->
  writeToLogger("Initializaing Purchase Departments"),
  purchaseDepartment:start().

initCashiers() ->
  writeToLogger("Initializaing CahsierServer"),
  cashierServer:start().


initCustomer(DelayQ) ->
  receive
    {terminate} ->
      writeToLogger("Store is Closed: no new customers~n"),
      exit(normal)

    after DelayQ ->
      customer:initCustomer(),
      castFunc(createCustomer),
      customer:initCustomer(),
      castFunc(createCustomer),
      initCustomer(round(rand:uniform() * 10))
  end.
  %writeToLogger("Initializaing Customer"),


updateNumberOfCustomers(TypeOfAction) ->
  %writeToLogger("updateNumberOfCustomers:OldStatus ", [TypeOfAction, getNumberOfCustomers()]),
  case TypeOfAction of
    "create" -> put(numberOfCustomers, getNumberOfCustomers() + 1 );
    "terminate" -> put(numberOfCustomers, getNumberOfCustomers() - 1);
    _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
end,
  writeToLogger("updateNumberOfCustomers:UpdatedStatus ", [TypeOfAction, getNumberOfCustomers()]).

getNumberOfCustomers() ->
  %writeToLogger("getNumberOfCustomers:Status ", get(numberOfCustomers)),
  get(numberOfCustomers).


count()->
  timer:sleep(1000),
  CurrTimestamp = ets:update_counter(timer, timestamp, {2, 1}),
  global:send(?TERMINATOR, {running}),
  if
    CurrTimestamp == ?NUMBER_OF_ITERATIONS -> castFunc(closeShop);
    true -> nothing
  end,
  count().


timerSupervisor()->
  writeToLogger("timerSupervisor"),
  ets:new(timer,[public, named_table]),
  ets:insert(timer,{timestamp, 0}),
  {Pid, _} = spawn_monitor(?MODULE, count, []),
  put(timerPid, Pid),
  timerSupervisorLoop().

timerSupervisorLoop()->
  writeToLogger("timerSupervisorLoop"),
  receive
    {'DOWN', _, process, _, _} ->  put(timerPid, spawn_monitor(count)),
      timerSupervisorLoop();
   {terminate} ->
     exit(get(timerPid), kill),
     ets:delete(timer),
     writeToLogger("Timer is terminated");
    {getTimeStamp, Pid} ->
                             [{timestamp, TimeStamp}] = ets:lookup(timer, timestamp),
                             Pid ! {timeStamp, TimeStamp},
                             timerSupervisorLoop()
  end.

getTimeStamp()->
  TimeStamp = callFunc(getTimeStamp).


terminatorLoop()->
  receive
    {running} -> terminatorLoop()
  after 5000 ->
    writeToLogger("Terminator decided program is dead"),
    gen_server:cast({global,purchaseDepartment},terminate),
    cashierServer:castFunc(terminate),
    terminateDepartments(inventory:getDepartments()),
    global:send(?SUPPLIER, {terminate}),
    deleteMnesia(),
    %global:send(?TIMER, "terminate"),
    exit(normal)
  end.

deleteMnesia()->
  mnesia:delete_table(product),
  mnesia:delete_table(department),
  mnesia:delete_table(dairy),
  mnesia:delete_table(meat),
  mnesia:delete_table(bakery).


%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment
writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).

writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).
writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).

test()-> start().
  %NumberOfCustomers = callFunc(getNumberOfCustomers),
  %A = 5.
