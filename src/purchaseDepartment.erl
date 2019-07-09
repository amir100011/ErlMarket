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
-define(DESIRED_RATIO, 0.5).
-define(SAVING_RATIO, 0.9).
-author("amir").

%% API
-export([initPurchaseDepartment/0]).
-export([updateBalance/1]).
-export([setInitialBudget/0]).
-export([testReservation/0]).


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
purchaseDepartmentRecursiveLoop([], ListOfProductsToReserve, _NumberOfCustomers) ->
  reserve(ListOfProductsToReserve),
  purchaseDepartmentRecursiveLoop().

getDepartments() ->
  inventory:getDepartments().


checkProductStatus(DepartmentName, NumberOfCustomers) ->
  ListOfValidProducts = gen_server:call({global,DepartmentName},getTotalAmountOfValidProduct), %record(departmentProduct,{department, product_name, price, expiry_time, amount}).
  ListOfValidProductsWithRatio = getRatio(ListOfValidProducts, NumberOfCustomers),
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w ~n",["checkProductStatus:ListOfValidProducts  - ",ListOfValidProducts]),
  file:close(S),
  ErlMarketBudget = get(erlMarketBudget),
  ratioToReserve(ListOfValidProductsWithRatio, NumberOfCustomers, ErlMarketBudget).


getRatio([H|T],NumberOfCustomers)->
  [getRatioSingleElement(H,NumberOfCustomers)] ++ getRatio(T,NumberOfCustomers);
getRatio([],_NumberOfCustomers)->[].

getRatioSingleElement({departmentProduct,{Department, Product_name, Price, _Expiry_time, Amount}},NumberOfCustomers) ->
  NumberOfProductsToOrder = (NumberOfCustomers * ?DESIRED_RATIO) - Amount,
  if  NumberOfProductsToOrder =< 0 -> AmountToOrder = 0;
    true -> AmountToOrder = NumberOfProductsToOrder
  end,
  [Department,Product_name,Price,AmountToOrder].



ratioToReserve(ListOfValidProductsWithRatio, NumberOfCustomers, ErlMarketBudget) ->
  if NumberOfCustomers > 0 ->
    PriceOfReservation = priceOfReservation(ListOfValidProductsWithRatio),
    if PriceOfReservation =< ErlMarketBudget ->
      writeToLogger("ratioToReserve Success: PriceOfReservation - " , PriceOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
      reserve(ListOfValidProductsWithRatio);
      true ->
        writeToLogger("ratioToReserve Failed: PriceOfReservation - " , PriceOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
        DeltaRatio = (?SAVING_RATIO * ErlMarketBudget) / PriceOfReservation,
        ListOfValidProductsWithNewRatio =  editRatio(ListOfValidProductsWithRatio,DeltaRatio),
        ratioToReserve(ListOfValidProductsWithNewRatio, NumberOfCustomers, ErlMarketBudget)
    end;
    true ->
      purchaseDepartmentRecursiveLoop()
  end.


editRatio([H|T],DeltaRatio) ->
  [editRatio(H,DeltaRatio,1)] ++ editRatio(T,DeltaRatio);
editRatio([],_DeltaRatio) -> [].

editRatio([Department,Product_name,Price,AmountToOrder], DeltaRatio, 1) ->
  [Department,Product_name,Price,round(AmountToOrder*DeltaRatio)].

priceOfReservation([H|T]) ->
  priceOfReservation(H,1) + priceOfReservation(T);
priceOfReservation([]) -> 0.

priceOfReservation([_Department,_Product_name,Price,AmountToOrder], 1) ->
  Price * AmountToOrder.


reserve(ListOfProductsToReserve) ->
  gen_server:call({global,supplierServer},{reserveation,  ListOfProductsToReserve}). %List [{product_name,amount,price}]


getNumberOfCustomers() ->
  masterFunction:getNumberOfCustomers().



writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).



testReservation() ->

  ListOfProductsToReserve =  [{departmentProduct,{diary, milk, 5, 5, 50}},
    {departmentProduct,{meat, "chicken", 9, 5, 7}},
    {departmentProduct,{bakery, "bread", 9, 5, 77}},
    {departmentProduct,{diary, "yogurt", 1, 5, 3}},
    {departmentProduct,{diary, "cheese", 100, 5, 25}},
    {departmentProduct,{meat, "steak", 17, 5, 33}}],

  department:start(meat),
  department:start(dairy),
  department:start(bakery),
  setInitialBudget(),
  ErlMarketBudget = get(erlMarketBudget),

  RatioedList = getRatio(ListOfProductsToReserve, 1000),


  ratioToReserve(RatioedList, 1000, ErlMarketBudget).

