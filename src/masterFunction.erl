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

%% API
-export([main/0, count/0, timerSuperviser/0, getTimeStamp/0, testTimer/0, test/0, terminatorLoop/0]).
-define(DEPARTMENT_LIST, inventory:getDepartments()).
-define(LOGGER_FILE_PATH, "../Logger-masterFunction.txt").
-define(NUMBER_OF_ITERATIONS, 10).
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-export([getNumberOfCustomers/0]).


main() ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().
  %testDep().
 % register(cleaner, spawn(cleaner,[])).

initErlMarketFunctionality() ->
  process_flag(trap_exit, true),
  global:register_name(?TERMINATOR, spawn(?MODULE, terminatorLoop, [])),
  global:register_name(?TIMER, spawn(?MODULE, timerSuperviser, [])),
  globalRegisterMasterFunction(),
  writeToLogger("strating initialization"),
  put(numberOfCustomers,0),
  initDepartments(?DEPARTMENT_LIST),
  initPurchaseDepartment(),
  initCashiers(),
  %initSuppliers(),
  initCustomer(),
  waitingLoop().

globalRegisterMasterFunction() ->
  global:register_name(masterFunction, self()).


waitingLoop() ->
  receive
    {"customerOut"} -> updateNumberOfCustomers("terminate"), waitingLoop();
    {"createCustomer"} -> updateNumberOfCustomers("create"), waitingLoop();
    {"terminateMaster"} -> global:send(?TIMER, {terminate}),  exit(normal);
    {"getNumberOfCustomers"} -> global:send(purchaseDepartment,{"numberOfCustomers",getNumberOfCustomers()}), waitingLoop();
    Msg -> writeToLogger("masterFunction recieved: ", Msg), waitingLoop()

    after 50 ->
    CurrentIteration =  getTimeStamp(),
    if CurrentIteration < ?NUMBER_OF_ITERATIONS ->
      initCustomer(),
      waitingLoop();
      true -> global:send(masterFunction, {"terminateMaster"}), waitingLoop()
    end

  end.


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

initPurchaseDepartment() ->
  writeToLogger("Initializaing Purchase Departments"),
  %register(purchasedepartment, spawn(purchaseDepartment,initPurchaseDepartment,[])). % why should we double register purchase department
  purchaseDepartment:initPurchaseDepartment().

initCashiers() ->
  writeToLogger("Initializaing CahsierServer"),
  cashierServer:start().


initCustomer() ->
  writeToLogger("Initializaing Customer"),
  customer:initCustomer(),
  updateNumberOfCustomers("create").

updateNumberOfCustomers(TypeOfAction) ->
  writeToLogger("updateNumberOfCustomers:OldStatus ", [TypeOfAction, getNumberOfCustomers()]),
  case TypeOfAction of
    "create" -> put(numberOfCustomers, getNumberOfCustomers() + 1 );
    "terminate" -> put(numberOfCustomers, getNumberOfCustomers() - 1);
    _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
end,
  writeToLogger("updateNumberOfCustomers:UpdatedStatus ", [TypeOfAction, getNumberOfCustomers()]).

getNumberOfCustomers() ->
  writeToLogger("getNumberOfCustomers:Status ", get(numberOfCustomers)),
  get(numberOfCustomers).


count()->
  timer:sleep(1000),
  ets:update_counter(timer, timestamp, {2, 1}),
  global:send(?TERMINATOR, {running}),
  count().


timerSuperviser()->
  ets:new(timer,[public, named_table]),
  ets:insert(timer,{timestamp, 0}),
  {Pid, _} = spawn_monitor(?MODULE, count, []),
  put(timerPid, Pid),
  timerSuperviserLoop().

timerSuperviserLoop()->
  receive
    {'DOWN', _, process, _, _} ->  put(timerPid, spawn_monitor(count)),
      timerSuperviserLoop();
   {terminate} ->
     io:fwrite("timer out ~n"),
     exit(get(timerPid), kill),
     ets:delete(timer),
     writeToLogger("timer is terminated ~n");
    {getTimeStamp, Pid} ->
                             [{timestamp, TimeStamp}] = ets:lookup(timer, timestamp),
                             Pid ! {timeStamp, TimeStamp},
                             timerSuperviserLoop()
  end.

getTimeStamp()->
  global:send(?TIMER, {getTimeStamp, self()}),
  receive
    {timeStamp, TimeStamp} -> TimeStamp
  end.

terminatorLoop()->
  receive
    {running} -> terminatorLoop()
  after 10000 ->
    writeToLogger("terminator decided program is dead"),
    global:send(purchaseDepartment,"terminate"),
    cashierServer:castFunc(terminate),
    terminateDepartments(inventory:getDepartments()),
    global:send(?TIMER, "terminate"),
    exit(normal)
  end.
%%initSuppliers() ->
%%  DepartmentList = inventory:getDeparments(),
%%  lists:foreach(fun(DepartmentName) ->
%%    supplier:raiseSupplier(DepartmentName)
%%                end, DepartmentList).



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


test()-> main().

testTimer()->
  main(),
  TimeStamp = getTimeStamp(),
  timer:sleep(1500),
  TimeStamp1 = getTimeStamp(),
  timer:sleep(1500),
  TimeStamp2 = getTimeStamp(),
  A = 5.

testDep() ->
  department:start(dairy),
  List = department:callFunc(dairy, getTotalAmountOfValidProduct),
  ListFormattedRight = purchaseDepartment:sumAmount(List, dairy),

  purchaseDepartment:setInitialBudget(),
  ErlMarketBudget =  10000,

  RatioedList = purchaseDepartment:getRatio(ListFormattedRight, 1000),

  file:write("../Log.txt",RatioedList),

  purchaseDepartment:ratioToReserve(RatioedList, 1000, ErlMarketBudget),

    A = 5.