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

count()->
  Fun = fun() ->
          CurrentTime = ets:take(timer, timestamp),
          ets:insert(timer,CurrentTime + 1)
        end,
  timer:apply_after(1000, ?MODULE, Fun, []),
  count().


cleaner()->
  ets:new(timer,[public,named_table]),
  ets:insert(timer,{timestamp, 0}),
  spawn_monitor(count),
  cleaner_loop().

cleaner_loop()->
  receive
    {'DOWN', _, process, _, _} ->  put(timerPid, spawn_monitor(count)),
                                   cleaner_loop();
    {terminate} ->  exit(get(timerPid)),
                    ets:delete(timer),
                    io:fwrite("timer is terminated ~n")
  end.

main() ->
  testDep().

 % register(cleaner, spawn(cleaner,[])).
%%  initErlMarketDataBase(),
%%  initErlMarketFunctionality().

%%
%%initErlMarketFunctionality() ->
%%  put(numberOfCostumers,0),
%%  initDepartments(),
%%  initPurchaseDepartment(),
%%  initCashiers(),
%%  initSuppliers(),
%%  initCustomer().
%%
%%
%%initErlMarketDataBase() ->
%%  inventory:initInventory().
%%
%%initDepartments() ->
%%  DepartmentList = inventory:getDepartments(),
%%  lists:foreach(fun(DepartmentName) ->
%%    department:raiseDepartment(DepartmentName)
%%                end, DepartmentList).
%%
%%initPurchaseDepartment() ->
%%  register(purchaseDepartment,spawn(fun() -> purchaseDepartment:initPurchaseDepartment end)).
%%
%%initCashiers() ->
%%  NumberOfCashiersNeeded = getNumberOfCustomers() * 0.25 + 1,
%%  cashier:raiseCashier(NumberOfCashiersNeeded).
%%
%%initSuppliers() ->
%%  DepartmentList = inventory:getDeparments(),
%%  lists:foreach(fun(DepartmentName) ->
%%    supplier:raiseSupplier(DepartmentName)
%%                end, DepartmentList).
%%
%%initCustomer() ->
%%  spawn(fun() -> costumer:initCustomer end),
%%  updateNumberOfCustomers(1).
%%
%%updateNumberOfCustomers(NumberToAdd) ->
%%  put(numberOfCustomers,get(numberOfCustomers) + NumberToAdd).
%%
%%getNumberOfCustomers() ->
%%  get(numberOfCustomers).

testDep() ->
  %inventory:initInventory(node()),
  %Pid = department2:start(),
  Pid2 = department:start(dairy),
  %ResponseC = department2:put(color, "red"),
  %ResponseD = department:callFunc(diary).
  ListA = department:callFunc(dairy, getProducts),
  department:castFunc(dairy, {sale, 0.2}),
  ListB = department:callFunc(dairy, getProducts),
  department:castFunc(dairy, cancelSale),
  ListC = department:callFunc(dairy, getProducts),
  A = 5.
