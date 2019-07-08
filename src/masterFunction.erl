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
-export([updateNumberOfCustomers/1,getNumberOfCustomers/0]).


main() ->
  testDep().
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
updateNumberOfCustomers(NumberToAdd) ->
  put(numberOfCustomers,get(numberOfCustomers) + NumberToAdd).

getNumberOfCustomers() ->
  get(numberOfCustomers).


testDep() ->
  %inventory:initInventory(node()),
  %Pid = department2:start(),
  Pid2 = department:start(dairy),
  Pid3 = dairy,
  io:fwrite("whereAMI~n"),
  %ResponseC = department2:put(color, "red"),
  %ResponseD = department:callFunc(diary).
  X = gen_server:call({global,Pid3}, getTotalAmountOfValidProduct),
  X.