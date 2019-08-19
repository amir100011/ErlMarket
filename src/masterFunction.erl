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
-export([count/0, timerSupervisor/0, getTimeStamp/0, terminatorLoop/0, initCustomer/2, waitForCustomerToLeave/0]).
-include_lib("records.hrl").
-define(LOGGER_FILE_PATH, "../Logger-masterFunction.txt").
-define(NUMBER_OF_ITERATIONS, 1000000).
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-define(SECURITY1, security1).
-define(SECURITY2, security2).
-define(CAP, 200).
-record(state, {dairy, meat, bakery}).
-export([getNumberOfCustomers/0]).
%%-export([periodicallyRestockInventory/0]).


start()->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  %initErlMarketDataBase(),
  initErlMarketFunctionality().

%% @doc initialize s the masterfunction gen_Server, initializing the watchdog and monitors the departments
initErlMarketFunctionality() ->
  process_flag(trap_exit, true),  % to block the option of department process destroying this process
  put(numberOfCustomers,0),
  global:register_name(?TERMINATOR, spawn(?MODULE, terminatorLoop, [])),
  global:register_name(?TIMER, spawn(?MODULE, timerSupervisor, [])),
  globalRegisterMasterFunction(),
  writeToLogger("strating initialization"),
  IsWatchDogAlredayRegisterd = global:whereis_name(watchdog),
  if IsWatchDogAlredayRegisterd =:= undefined ->
    writeToLogger("Starting watchdog"),
    startWatchdog();
    true -> skip
  end,
  [DairyMonitor, MeatMonitor, BakeryMonitor] = initDepartments(?DEPARTMENT_LIST),
  global:register_name(?SECURITY1, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 50), 0 ])),
  global:register_name(?SECURITY2, spawn(?MODULE, initCustomer, [ round(rand:uniform() * 50), 0 ])),
  State = #state{dairy = DairyMonitor, meat = MeatMonitor, bakery = BakeryMonitor},
  {ok, State}.

globalRegisterMasterFunction() ->
  global:register_name(masterFunction, self()).

startWatchdog() ->
  ListOfModulesToNodes = buildListForWatchDogToInitialize(),
  watchdog:start(ListOfModulesToNodes).

buildListForWatchDogToInitialize() ->
  [[?PURCHASE_DEPARTMENT_NODE, purchaseDepartment,[]]] ++ [[?CASHIER_SERVER_NODE, cashierServer,[]]].

handle_call(getTimeStamp, _From, State) ->
  TimeStamp = getTimeStampFromClock(),
  {reply, TimeStamp, State};

handle_call(getNumberOfCustomers, _From, State) ->
  writeToLogger("getNumberofCustomers reached"),
  Reply = getNumberOfCustomers(),
  {reply, Reply, State};

handle_call(pid, _From, State) ->
  Reply= self(),
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
  writeToLogger("master function reached terminate"),
  {stop, normal, State}.

% not pretty but effective, for each department we create a handle info block according to the monitorRef
handle_info({'DOWN', MonitorRef, _Type, _Object, normal}, State) ->
  writeToLogger(variable, "RECEIVED normal termination of ~p ~n",[MonitorRef]),
  {noreply, State};
handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, #state{bakery = MonitorRef} = State) ->
  writeToLogger(variable, "bakery department has fallen from ~p , reinstalling it ~n",[Info]),
  {noreply, State#state{bakery = newMonitor(bakery)}};

handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, #state{dairy = MonitorRef} = State) ->
  writeToLogger(variable, "dairy department has fallen from ~p , reinstalling it ~n",[Info]),
  {noreply, State#state{dairy = newMonitor(dairy)}};

handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, #state{meat = MonitorRef} = State) ->
  writeToLogger(variable, "meat department has fallen from ~p , reinstalling it ~n",[Info]),
  {noreply, State#state{meat = newMonitor(meat)}};

handle_info(Info, State) ->
  writeToLogger("masterFunction recieved: ", Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  writeToLogger("Master Function says Bye Bye"),
  ok.


%% @doc interface function for using gen_server call
callFunc(getNumberOfCustomers) ->
  gen_server:call({global,?MODULE}, getNumberOfCustomers);
callFunc(Message) ->
  try gen_server:call({global,?MODULE}, Message) of
    AnsFromServer -> AnsFromServer
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable ," ~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      Ans = callFunc(Message),
      Ans
  end.


%% @doc interface function for using gen_server cast
castFunc(Message) ->
  try   gen_server:cast({global, ?MODULE}, Message) of
    AnsFromServer-> AnsFromServer  % usually no reply just ok or some atom
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      castFunc(Message)
  end.



initDepartments([]) -> [];
initDepartments([H|T]) ->  % H is a department
  {ok, DepartmentPid} = department:start(H),
  link(DepartmentPid),
  [monitor(process, DepartmentPid)] ++ initDepartments(T).
% we link because we want to eliminate the possibility of departments working without a masterFunction
% we assume that the masterFunction is protected by the watchdog module


terminateDepartments(DepartmentList) ->
  writeToLogger("terminating Departments"),
  lists:foreach(fun(DepartmentName) ->
                 department:castFunc(DepartmentName, terminate)
                end, DepartmentList).


% we want to have no customers in the shop before shutting down so we wait for them to complete their shopping spree
waitForCustomerToLeave()->
  timer:sleep(1500),
  try callFunc(getNumberOfCustomers) of
    NumberOfCustomers -> writeToLogger(variable, "Shop is closed: ~p  Customer remain ~n",[NumberOfCustomers]),
      if
        NumberOfCustomers =/= 0 ->
          waitForCustomerToLeave();
        true ->
          writeToLogger("Shop is closed: all customers left~n"),
          global:send(?TIMER, {terminate}),
          exit(normal)
      end
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"FROM WaitingForCustomersToleave: MasterFunction is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      waitForCustomerToLeave()
  end.



%% initialize customers is a function used by the Security guards where they spawn customers periodically
initCustomer(DelayQ, TimeStamp) ->
  receive
    {terminate} ->
      writeToLogger("Store is Closed: no new customers"),
      exit(normal);
    {updateTime, CurrTimestamp} -> initCustomer(round(rand:uniform() * 100), CurrTimestamp)
    after DelayQ ->
     try callFunc(getNumberOfCustomers) of
       NumOfCustomers ->
         if NumOfCustomers < ?CAP ->
               customer:initCustomer(TimeStamp),
               castFunc(createCustomer),
               customer:initCustomer(TimeStamp),
               castFunc(createCustomer),
               initCustomer(round(rand:uniform() * 100), TimeStamp);
         true ->
                timer:sleep(500),
                initCustomer(round(rand:uniform() * 100), TimeStamp)
         end
    catch
      exit:Error -> timer:sleep(2500),
                    writeToLogger(variable,"FROM Security: MasterFunction is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
                    initCustomer(round(rand:uniform() * 100), TimeStamp)
    end
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
  purchaseDepartment:castFunc( {updateTime, CurrTimestamp}),
  interface:castFunc({updateTime,CurrTimestamp}),
  count().

%% @doc timer is used for maintating a unified time in all nodes, this is mandatory when removing expired products
%% and also sends an "im alive" signal to the terminator
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
     writeToLogger("Timer is terminated"),
    io:fwrite("Timer is terminated");
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
%% @doc  the terminator receives a signal from the timer indicating that the system is alive, once the timer is dead the
%% terminator shuts down all the Market servers
terminatorLoop()->
  receive
    {running} -> terminatorLoop()
  after 5000 ->
    writeToLogger("Terminator decided program is dead"),
    io:fwrite("Terminator decided program is dead"),
    purchaseDepartment:castFunc(terminate),
    %gen_server:cast({global, purchaseDepartment},terminate),
    waitForPurchaseDepartment(),
    cashierServer:castFunc(terminate),
    terminateDepartments(?DEPARTMENT_LIST),
    timer:sleep(500), % TODO delete!!!
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


newMonitor(DepartmentName) ->
  {ok, DepartmentPid} = department:start(DepartmentName),
  link(DepartmentPid),
  MonitorRefNew = monitor(process, DepartmentPid),
  MonitorRefNew.
%%------------------WRITING TO LOGGER------------------

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
