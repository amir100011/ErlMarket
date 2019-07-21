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
-export([main/0, count/0, timerSuperviser/0, getTimeStamp/0, testTimer/0, test/0, terminatorLoop/0, initCustomer/1]).
-define(DEPARTMENT_LIST, inventory:getDepartments()).
-define(LOGGER_FILE_PATH, "../Logger-masterFunction.txt").
-define(NUMBER_OF_ITERATIONS, 2).
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-define(SECURITY1, security1).
-define(SECURITY2, security2).
-export([getNumberOfCustomers/0]).


main() ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().
  %testDep().
 % register(cleaner, spawn(cleaner,[])).

initErlMarketFunctionality() ->
  global:register_name(?TERMINATOR, spawn(?MODULE, terminatorLoop, [])),
  global:register_name(?TIMER, spawn(?MODULE, timerSuperviser, [])),
  globalRegisterMasterFunction(),
  writeToLogger("strating initialization"),
  put(numberOfCustomers,0),
  put(open, 1),
  initDepartments(?DEPARTMENT_LIST),
  initPurchaseDepartment(),
  initCashiers(),
  %initSuppliers(),
  global:register_name(?SECURITY1, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 10) ])),
  global:register_name(?SECURITY2, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 10) ])),
  waitingLoop().

globalRegisterMasterFunction() ->
  global:register_name(masterFunction, self()).


waitingLoop() ->
  receive
    {"customerOut"} -> updateNumberOfCustomers("terminate");
    {"createCustomer"} -> updateNumberOfCustomers("create");
    {"terminateMaster"} -> global:send(?TIMER, {terminate}),  exit(normal);
    {"getNumberOfCustomers"} -> global:send(purchaseDepartment,{"numberOfCustomers", getNumberOfCustomers()});
    Msg -> writeToLogger("masterFunction recieved: ", Msg)
  end,
  CurrentIteration =  getTimeStamp(),
  if
    CurrentIteration < ?NUMBER_OF_ITERATIONS ->
      %initCustomer(),
      waitingLoop();

    true ->
      NumOfCustomers = getNumberOfCustomers(),
      if
        NumOfCustomers == 0 -> global:send(masterFunction, {"terminateMaster"}), waitingLoop();
      % waiting for customers to leave the store before closing it
        true ->
          Open = get(open),
          if
            Open == 1 -> put(open, 0) ,global:send(?SECURITY1, {terminate}), global:send(?SECURITY1, {terminate});
            true -> proceed
          end,
          waitingLoop()
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


initCustomer(DelayQ) ->
  receive
    {terminate} ->
      writeToLogger("Store is Closed: no new customers~n"),
      exit(normal)
    after DelayQ ->
      customer:initCustomer(),
      global:send(masterFunction, {"createCustomer"}),
      customer:initCustomer(),
      global:send(masterFunction, {"createCustomer"}),
      customer:initCustomer(),
      global:send(masterFunction, {"createCustomer"}),
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
     exit(get(timerPid), kill),
     ets:delete(timer),
     writeToLogger("Timer is terminated ~n");
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
    writeToLogger("Terminator decided program is dead"),
    global:send(purchaseDepartment,"terminate"),
    cashierServer:castFunc(terminate),
    terminateDepartments(inventory:getDepartments()),
    global:send(?TIMER, "terminate"),
    deleteMnesia(),
    exit(normal)
  end.

deleteMnesia()->
  mnesia:delete_table(product),
  mnesia:delete_table(department),
  mnesia:delete_table(dairy),
  mnesia:delete_table(meat),
  mnesia:delete_table(bakery).
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