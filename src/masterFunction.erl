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
-export([main/0]).
-define(DEPARTMENT_LIST, inventory:getDepartments()).
-export([getNumberOfCustomers/0]).


main() ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().
  %testDep().

 % register(cleaner, spawn(cleaner,[])).
%%  initErlMarketDataBase(),
%%  initErlMarketFunctionality().


initErlMarketFunctionality() ->
  writeToLogger("strating initialization"),
  put(numberOfCustomers,0),
  initDepartments(?DEPARTMENT_LIST),
  initPurchaseDepartment(),
  initCashiers(),
  %initSuppliers(),
  initCustomer().


initErlMarketDataBase() ->
  inventory:initInventory(node()).

initDepartments(DepartmentList) ->
  writeToLogger("Initializaing Departments"),
  lists:foreach(fun(DepartmentName) ->
    department:start(DepartmentName)
                end, DepartmentList).

initPurchaseDepartment() ->
  writeToLogger("Initializaing Purchase Departments"),
  register(purchasedepartment,spawn(purchaseDepartment,initPurchaseDepartment,[])).

initCashiers() ->
  writeToLogger("Initializaing CahsierServer"),
  cashierServer:start().


initCustomer() ->
  writeToLogger("Initializaing Customer"),
  customer:initCustomer(),
  updateNumberOfCustomers("create").

updateNumberOfCustomers(TypeOfAction) ->
  writeToLogger("updateNumberOfCustomers:OldStatus ", [TypeOfAction,getNumberOfCustomers()]),
  case TypeOfAction of
    "create" -> put(numberOfCustomers, getNumberOfCustomers() + 1 );
    "terminate" -> put(numberOfCustomers, getNumberOfCustomers() - 1);
    _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
end,
  writeToLogger("updateNumberOfCustomers:UpdatedStatus ", [TypeOfAction,getNumberOfCustomers()]).

getNumberOfCustomers() ->
  writeToLogger("getNumberOfCustomers:Status ", get(numberOfCustomers)),
  get(numberOfCustomers).


%%count()->
%%  Fun = fun() ->
%%    CurrentTime = ets:take(timer, timestamp),
%%    ets:insert(timer,CurrentTime + 1)
%%        end,
%%  timer:apply_after(1000, ?MODULE, Fun, []),
%%  count().
%%
%%
%%cleaner()->
%%  ets:new(timer,[public,named_table]),
%%  ets:insert(timer,{timestamp, 0}),
%%  spawn_monitor(count),
%%  cleaner_loop().
%%
%%cleaner_loop()->
%%  receive
%%    {'DOWN', _, process, _, _} ->  put(timerPid, spawn_monitor(count)),
%%      cleaner_loop();
%%    {terminate} ->  exit(get(timerPid)),
%%      ets:delete(timer),
%%      io:fwrite("timer is terminated ~n")
%%  end.



%%initSuppliers() ->
%%  DepartmentList = inventory:getDeparments(),
%%  lists:foreach(fun(DepartmentName) ->
%%    supplier:raiseSupplier(DepartmentName)
%%                end, DepartmentList).



%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment
writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file("../Log.txt", io_lib:format("~p.~n", [List]), [append]).

writeToLogger(String) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).



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
