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


main() ->
  initErlMarketDataBase(),
  initErlMarketFunctionality().


initErlMarketFunctionality() ->
  put(numberOfCostumers,0),
  initDepartments(),
  initPurchaseDepartment(),
  initCashiers(),
  initSuppliers(),
  initCostumer().


initErlMarketDataBase() ->
  inventory:initInventory().

initDepartments() ->
  DepartmentList = inventory:getDeparments(),
  lists:foreach(fun(DepartmentName) ->
    department:raiseDepartment(DepartmentName)
                end, DepartmentList).

initPurchaseDepartment() ->
  register(purchaseDepartment,spawn(fun() -> purchaseDepartment:initPurchaseDepartment end)).

initCashiers() ->
  NumberOfCashiersNeeded = inventory:getNumOfCustomers() * 0.25 + 1,
  cashier:raiseCashier(NumberOfCashiersNeeded).

initSuppliers() ->
  DepartmentList = inventory:getDeparments(),
  lists:foreach(fun(DepartmentName) ->
    supplier:raiseSupplier(DepartmentName)
                end, DepartmentList).

initCostumer() ->
  spawn(fun() -> costumer:initCostumer end),
  updateNumberOfCostumers(1).

updateNumberOfCostumers(NumberToAdd) ->
  put(numberOfCostumers,get(numberOfCostumers) + NumberToAdd).