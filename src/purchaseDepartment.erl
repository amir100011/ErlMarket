%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 11:37
%%%-------------------------------------------------------------------
-module(purchaseDepartment).
-include_lib("records.hrl").
-author("amir").

%% API
-export([initPurchaseDepartment/0]).


initPurchaseDepartment() ->
  setInitialBudget(),
  purchaseDepartmentRecursiveLoop().


setInitialBudget() ->
  put(erlMarketBudget,10000).

purchaseDepartmentRecursiveLoop() ->
  DepartmentList = getDepartments(),
  NumberOfCustomers = getNumberOfCustomers(),
  purchaseDepartmentRecursiveLoop(DepartmentList, [], NumberOfCustomers).


purchaseDepartmentRecursiveLoop([H|T], ListOfProductsToReserve, NumberOfCustomers) ->
  ListOfProductsToReserve = ListOfProductsToReserve ++ checkProductStatus(H, NumberOfCustomers),
  purchaseDepartmentRecursiveLoop(T, ListOfProductsToReserve, NumberOfCustomers);
purchaseDepartmentRecursiveLoop([], ListOfProductsToReserve, NumberOfCustomers) ->
  reserve(ListOfProductsToReserve, NumberOfCustomers),
  purchaseDepartmentRecursiveLoop().

getDepartments() ->
  inventory:getDepartments().


checkProductStatus(DepartmentName, NumberOfCustomers) ->
  ListOfValidProducts = gen_server:call(DepartmentName,getTotalAmountOfValidProduct), %List [{product_name,amount,price}]
  shouldReserve(ListOfValidProducts, NumberOfCustomers).



shouldReserve(ListOfValidProducts , NumberOfCustomers) ->
  %TODO: add check list of products to reserve
  %FIXME: returns a list of tuples [{product Name,price,ratio}]
  [{milk,5,0.1}].

reserve(ListOfProductsToReserve, NumberOfCustomers) ->
  {ListOfProductsToReserve, Ratio} = optimizeReservation(ListOfProductsToReserve, NumberOfCustomers),
  gen_server:call(supplierServer,reserveation,  {ListOfProductsToReserve, Ratio}). %List [{product_name,amount,price}]


optimizeReservation(ListOfProductsToReserve, NumberOfCustomers) ->
  {ListOfProductsToReserve,0.5}.

