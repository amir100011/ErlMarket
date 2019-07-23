%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc describes ErlMarket purchase department
%%%
%%% @end
%%% Created : 07. Jul 2019 11:37
%%%-------------------------------------------------------------------
-module(purchaseDepartment).
-include_lib("records.hrl").
-define(DESIRED_RATIO, 0.5).
-define(SAVING_RATIO, 0.9).
-define(LOGGER_FILE_PATH, "../Logger-PurchaseDepartment.txt").
-define(DEPARTMENT_LIST, inventory:getDepartments()).
-author("amir").

%% API
-export([initPurchaseDepartment/0]).
-export([setBalance/2]).
-export([setInitialBudget/0]).
-export([testReservation/0]).
-export([sumAmount/2]).
-export([getRatio/2,ratioToReserve/3]).
-export([initPurchaseDepartmentData/0]).
-export([writeToLogger/2]).



%%----------------PRIMARY FUNCTION-------------------------
initPurchaseDepartment() ->
  global:register_name(purchaseDepartment,spawn(purchaseDepartment, initPurchaseDepartmentData, [])).


%%----------------INTERNAL FUNCTIONS-------------------------
initPurchaseDepartmentData()->
  setInitialBudget(),
  purchaseDepartmentRecursiveLoop().

%% @doc Forever loop that checks with each department if it has some products that need to be reserved from supplier.
purchaseDepartmentRecursiveLoop() ->
  writeToLogger("got to recursive loop"),
  receive
    {"add", AmountToAdd} ->
      setBalance("add",AmountToAdd), purchaseDepartmentRecursiveLoop();
    {"deduce",AmountToDeduct} ->
      setBalance("deduce",AmountToDeduct), purchaseDepartmentRecursiveLoop();
    {getBudget} -> io:fwrite("Budget is : ~p~n",[getBalance()]), purchaseDepartmentRecursiveLoop();
    "terminate" -> writeToLogger("purchaseDeartment terminated"), exit(normal)
  after 5000 ->
  %getNumberOfCustomers(), % Doesnt work without masterfunction thread
    receive
      {"numberOfCustomers",ReturnedNumberOfCustomers} ->
        NumberOfCustomers = ReturnedNumberOfCustomers,
        ErlMarketBudget = get(erlMarketBudget),
        ListOfValidProductsToReserve = getListOfProductsToReserve(?DEPARTMENT_LIST),
        ListOfValidProductsWithRatio = checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers),
        ratioToReserve(ListOfValidProductsWithRatio, NumberOfCustomers,ErlMarketBudget)
    after 1000 ->
      purchaseDepartmentRecursiveLoop()
    end,

    purchaseDepartmentRecursiveLoop()
  end.

%% @doc checks the ratio for each product (customer / number of products valid in depatment)
checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers) ->
  writeToLogger("checkProductStatus:ListOfValidProducts  - ",ListOfValidProductsToReserve),
  ListOfValidProductsWithRatio = getRatio(ListOfValidProductsToReserve, NumberOfCustomers),
  writeToLogger("checkProductStatus:ListOfValidProductsWithRatio  - ",ListOfValidProductsWithRatio),
  ListOfValidProductsWithRatio.

getRatio([H|T],NumberOfCustomers)->
  [getRatioSingleElement(H,NumberOfCustomers)] ++ getRatio(T,NumberOfCustomers);
getRatio([],_NumberOfCustomers)->[].

getRatioSingleElement({departmentProduct,Department, Product_name, Price, _Expiry_time, Amount},NumberOfCustomers) ->
  writeToLogger("getRatioSingleElement",[{departmentProduct,Department, Product_name, Price, _Expiry_time, Amount},NumberOfCustomers]),
  NumberOfProductsToOrder = (NumberOfCustomers * ?DESIRED_RATIO) - Amount,
  if  NumberOfProductsToOrder =< 0 -> AmountToOrder = 0;
    true -> AmountToOrder = NumberOfProductsToOrder
  end,
  [Department,Product_name,Price,AmountToOrder].



ratioToReserve(ListOfValidProductsWithRatio, NumberOfCustomers, ErlMarketBudget) ->
  CostOfReservation = priceOfReservation(ListOfValidProductsWithRatio),
    if CostOfReservation =< ErlMarketBudget ->
      writeToLogger("ratioToReserve Success: PriceOfReservation - " , CostOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
     %% reserve(ListOfValidProductsWithRatio, CostOfReservation),
%%      {A1,A2,A3} = now(),
%%      random:seed(A1, A2, A3),
%%      timer:sleep(timer:seconds(random:uniform())); %%THE PROCESS IS SLEEPING FOR RANDOM TIME SEED SAVES ITS DICTIONARY
      reserve(ListOfValidProductsWithRatio,CostOfReservation),
      ErlMarketBudget = get(erlMarketBudget);
      true ->
        writeToLogger("ratioToReserve Failed: PriceOfReservation - " , CostOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
        DeltaRatio = (?SAVING_RATIO * ErlMarketBudget) / CostOfReservation,
        ListOfValidProductsWithNewRatio =  editRatio(ListOfValidProductsWithRatio,DeltaRatio),
        ratioToReserve(ListOfValidProductsWithNewRatio, NumberOfCustomers, ErlMarketBudget)
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


reserve([H|T], CostOfReservation) ->
  setBalance("deduce", CostOfReservation),
  sendProductsToDepartment(tmp,tmp).
%[[diary,milk,5,450.0],[meat,"chicken",9,493.0],[bakery,"bread",9,423.0],[diary,"yogurt",1,497.0],[diary,"cheese",100,475.0],[meat,"steak",17,467.0]]



sendProductsToDepartment(Department, DebarmentAndList)->
  gen_server:call({global,Department},{restock, DebarmentAndList}). %List [{product_name,amount,price}]


%% @doc sumAmount return a list of departmentProducts where each product has the amount of all valid products of the same name
sumAmount(List, Department) ->
  Dict = sumAmountInternal(List, dict:new()),
  KeysAndValues = dict:to_list(Dict),
  sumAmount(KeysAndValues, [], Department).
sumAmount([],Ans,_) -> Ans;
sumAmount([{ProductName, [Amount, Price]}|T], Ans, Department)->
  FormattedOutput = #departmentProduct{department = Department,
    product_name = ProductName,
    price = Price,
    expiry_time = 0,
    amount = Amount
  },
  sumAmount(T, Ans ++ [FormattedOutput], Department).

sumAmountInternal([],Dict) -> Dict;
sumAmountInternal([H|T], Dict) ->
  [Name, Price, Amount] = H,
  case dict:is_key(Name, Dict) of
    true -> [CurrentAmount,_] = dict:fetch(Name, Dict),
      UpdatedDictToReturn = dict:store(Name,[CurrentAmount + Amount, Price], Dict);
    false -> UpdatedDictToReturn = dict:store(Name, [Amount, Price], Dict)
  end,
  sumAmountInternal(T, UpdatedDictToReturn).




%%----------------------GETTERS/SETTERS----------------------

%% @doc returns the current number of customers in ErlMarket
getNumberOfCustomers() ->
  global:send(masterFunction,{"getNumberOfCustomers"}).

%% @doc returns List of records %[{departmentProduct,department, product_name, price, expiry_time, amount}].
getListOfProductsToReserve([H|T]) ->
  getListOfProductsToReserveInternal(H) ++ getListOfProductsToReserve(T);
getListOfProductsToReserve([]) -> [].

getListOfProductsToReserveInternal(DepartmentName) ->
  ListNotOrganized = gen_server:call({global,DepartmentName},getTotalAmountOfValidProduct),
  sumAmount(ListNotOrganized, DepartmentName).

setInitialBudget() ->
  put(erlMarketBudget,10000),
  writeToLogger("setInitialBudget ", get(erlMarketBudget)).

setBalance(TypeOfAction , Amount) ->
  writeToLogger("setBalance: ", [TypeOfAction,Amount]),
  OldBalance = get(erlMarketBudget),
  writeToLogger("OldBalance: ", OldBalance),
  case TypeOfAction of
    "add" -> put(erlMarketBudget, OldBalance + Amount);
    "deduce" -> put(erlMarketBudget, OldBalance - Amount);
  _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
  end,
  NewBalance = get(erlMarketBudget),
  writeToLogger("updateBalance: OldBalance: ",OldBalance, " NewBalance:",NewBalance).

getBalance()-> get(erlMarketBudget).
%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment
writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).

writeToLogger(String, OldBalance) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~w~n ",[String, OldBalance]),
  file:close(S).

writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).


testReservation() ->

%%  ListOfProductsToReserve =  [{departmentProduct,diary, milk, 5, 5, 50},
%%    {departmentProduct,meat, "chicken", 9, 5, 7},
%%    {departmentProduct,bakery, "bread", 9, 5, 77},
%%    {departmentProduct,diary, "yogurt", 1, 5, 3},
%%    {departmentProduct,diary, "cheese", 100, 5, 25},
%%    {departmentProduct,meat, "steak", 17, 5, 33}],
%%  department:start(meat),
%%  department:start(dairy),
%%  department:start(bakery),
  initPurchaseDepartment().
%%  ErlMarketBudget = get(erlMarketBudget),
%%  purchaseDepartment ! {"add",5}.

%%  RatioedList = getRatio(ListOfProductsToReserve, 1000),
%%  ratioToReserve(RatioedList, 1000, ErlMarketBudget).

