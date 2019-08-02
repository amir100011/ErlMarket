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
-define(PURCHASE_DEPARTMENT_NODE, 'amir@amir-Inspiron-5559').
-define(CASHIER_SERVER_NODE, 'amir@amir-Inspiron-5559').
-define(DEPARTMENTS_NODE, 'amir@amir-Inspiron-5559').
-behavior(gen_server).
%% API
%% API
-export([start/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, callFunc/1, castFunc/1]).
-export([count/0, timerSupervisor/0, getTimeStamp/0, test/0, terminatorLoop/0, initCustomer/2, waitForCustomerToLeave/0]).
-include_lib("records.hrl").
-define(LOGGER_FILE_PATH, "../Logger-masterFunction.txt").
-define(NUMBER_OF_ITERATIONS, 1000000).
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-define(SECURITY1, security1).
-define(SECURITY2, security2).
-export([getNumberOfCustomers/0]).
%%-export([periodicallyRestockInventory/0]).


start()->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().


initErlMarketFunctionality() ->
  put(numberOfCustomers,0),
  global:register_name(?TERMINATOR, spawn(?MODULE, terminatorLoop, [])),%TODO add monitoring for this
  global:register_name(?TIMER, spawn(?MODULE, timerSupervisor, [])),%TODO add monitoring for this
  globalRegisterMasterFunction(),
  writeToLogger("strating initialization"),
  ListOfModulesToNodes = buildListForWatchDogToInitialize(),
  watchdog:start(ListOfModulesToNodes),
  global:register_name(?SECURITY1, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 50), 0 ])),
  global:register_name(?SECURITY2, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 50), 0 ])),
  {ok, normal}.

globalRegisterMasterFunction() ->
  global:register_name(masterFunction, self()).

buildListForWatchDogToInitialize() ->
  DepartmentListOfModulesToNodes = initDepartments(?DEPARTMENT_LIST,[]),
  DepartmentListOfModulesToNodes ++ [[?PURCHASE_DEPARTMENT_NODE,purchaseDepartment,[]]] ++ [[?CASHIER_SERVER_NODE,cashierServer,[]]].

handle_call(getTimeStamp, _From, State) ->
  TimeStamp = getTimeStampFromClock(),
  {reply, TimeStamp, State};

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
  global:send(?SECURITY2, {terminate}),
  spawn(?MODULE, waitForCustomerToLeave, []),
  writeToLogger("exiting close shop"),
  {noreply, State};

handle_cast(customerOut, State) ->
  updateNumberOfCustomers("terminate"),
  {noreply, State};

handle_cast(terminate, State) ->
  %terminate(0,0),
  writeToLogger("master function reached terminate"),
  {stop, normal, State}.

handle_info(Info, State) ->
  writeToLogger("masterFunction recieved: ", Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  writeToLogger("Master Function says Bye Bye"),
  %global:send(?TIMER, {terminate}),
  ok.

%% @doc interface function for using gen_server call
callFunc(Message) ->
  gen_server:call({global,?MODULE}, Message).

%% @doc interface function for using gen_server cast
castFunc(Message) ->
  gen_server:cast({global, ?MODULE}, Message).


initErlMarketDataBase() ->
  inventory:initInventory(node()).

initDepartments([H|T],List) ->
  initDepartments(T,List) ++ [initDepartmentsInternal(H,List)];
initDepartments([],List) -> List.

initDepartmentsInternal(DepartmentName,List) ->
  List ++ [?DEPARTMENTS_NODE, department, DepartmentName].

terminateDepartments(DepartmentList) ->
  writeToLogger("terminating Departments"),
  lists:foreach(fun(DepartmentName) ->
                 department:castFunc(DepartmentName, terminate)
                end, DepartmentList).

waitForCustomerToLeave()->
  timer:sleep(2500),
  NumberOfCustomers = callFunc(getNumberOfCustomers),
  writeToLogger(variable, "Shop is closed: ~p  Customer remain ~n",[NumberOfCustomers]),
  if
    NumberOfCustomers == 0 ->  writeToLogger("Shop is closed: all customers left~n"),
                               global:send(?TIMER, {terminate}),
                               %castFunc(terminate),
                               exit(normal);
    true -> waitForCustomerToLeave()
  end.


initPurchaseDepartment() ->
  writeToLogger("Initializaing Purchase Departments"),
  purchaseDepartment:start().

initCashiers() ->
  writeToLogger("Initializaing CahsierServer"),
  cashierServer:start().


initCustomer(DelayQ, TimeStamp) ->
  receive
    {terminate} ->
      writeToLogger("Store is Closed: no new customers"),
      exit(normal);
    {updateTime, CurrTimestamp} -> initCustomer(round(rand:uniform() * 20), CurrTimestamp)
    after DelayQ ->
      customer:initCustomer(TimeStamp),
      castFunc(createCustomer),
      customer:initCustomer(TimeStamp),
      castFunc(createCustomer),
      initCustomer(round(rand:uniform() * 20), TimeStamp)
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
  gen_server:cast({global,purchaseDepartment}, {updateTime, CurrTimestamp}),
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
  callFunc(getTimeStamp).

waitForPurchaseDepartment() ->
  case global:whereis_name(purchaseDepartment) of
    undefined -> continue;
    _ -> waitForPurchaseDepartment()
  end.

terminatorLoop()->
  receive
    {running} -> terminatorLoop()
  after 5000 ->
    writeToLogger("Terminator decided program is dead"),
    gen_server:cast({global, purchaseDepartment},terminate),
    waitForPurchaseDepartment(),
    cashierServer:castFunc(terminate),
    terminateDepartments(inventory:getDepartments()),
    castFunc(terminate),
    deleteMnesia(),
    exit(normal)
  end.

deleteMnesia()->
  mnesia:delete_table(product),
  mnesia:delete_table(department),
  mnesia:delete_table(dairy),
  mnesia:delete_table(meat),
  mnesia:delete_table(bakery).

getTimeStampFromClock()->
  global:send(?TIMER, {getTimeStamp, self()}),
  receive
    {timeStamp, TimeStamp} -> TimeStamp
  end.

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

test()-> watchdog:start([[?CASHIER_SERVER_NODE,cashierServer,[]]]).
 % initDepartments([dairy,meat,bakery],[]).
  %NumberOfCustomers = callFunc(getNumberOfCustomers),
  %A = 5.
