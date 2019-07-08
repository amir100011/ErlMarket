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
-export([updateBalance/1]).
-export([setInitialBudget/0]).


initPurchaseDepartment() ->
  setInitialBudget(),
  purchaseDepartmentRecursiveLoop().

%TODO add os:systemTime to the log
setInitialBudget() ->
  put(erlMarketBudget,10000),
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s ~n",["setInitialBudget 10000"]),
  file:close(S),
  get(erlMarketBudget).

updateBalance(AmountToAdd) ->
  OldBalance = integer_to_list(get(erlMarketBudget)),
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s ~s ~n",["updateBalance: OldBalance:",OldBalance]),
  put(erlMarketBudget, get(erlMarketBudget) + AmountToAdd),
  NewBalance = integer_to_list(get(erlMarketBudget)),
  io:format(S,"~s ~s ~n",["updateBalance: NewBalance:",NewBalance]),
  file:close(S).

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
  ListOfValidProducts = gen_server:call({global,DepartmentName},getTotalAmountOfValidProduct), %List [{product_name,amount,price}]
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w ~n",["Payment:Add to balance - ",ListOfValidProducts]),
  file:close(S).
  %TODO: shouldReserve(ListOfValidProducts, NumberOfCustomers).



shouldReserve(ListOfValidProducts , NumberOfCustomers) ->
  %TODO: add check list of products to reserve
  %FIXME: returns a list of tuples [{product Name,price,ratio}]
  [{milk,5,0.1}].

reserve(ListOfProductsToReserve, NumberOfCustomers) ->
  {ListOfProductsToReserve, Ratio} = optimizeReservation(ListOfProductsToReserve, NumberOfCustomers),
  gen_server:call(supplierServer,reserveation,  {ListOfProductsToReserve, Ratio}). %List [{product_name,amount,price}]


optimizeReservation(ListOfProductsToReserve, NumberOfCustomers) ->
  {ListOfProductsToReserve,0.5}.

getNumberOfCustomers() ->
  masterFunction:getNumberOfCustomers().


