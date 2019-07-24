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
  after 500 ->
  getNumberOfCustomers(), % Doesnt work without masterfunction thread
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
      writeToLogger("reserve - ", ListOfValidProductsWithRatio),
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
  priceOfReservationSingleElement(H) + priceOfReservation(T);
priceOfReservation([]) -> 0.

priceOfReservationSingleElement([_Department,_Product_name,Price,AmountToOrder]) ->
  Price * AmountToOrder.


reserve(ListOfValidProductsWithRatio, CostOfReservation) ->
  setBalance("deduce", CostOfReservation),
  lists:foreach(
    fun(N) ->
      ListOfProductsToAddToDepartment = addProducts(N,ListOfValidProductsWithRatio,[]),
      sendProductsToDepartment(N,ListOfProductsToAddToDepartment)
    end, ?DEPARTMENT_LIST).

addProducts(DepartmentName,[H|T],CallList) ->
  CallListToAdd = addProductsToCallList(DepartmentName,H,CallList),
  addProducts(DepartmentName,T,CallListToAdd);
addProducts(_DepartmentName,[],CallList) -> CallList.


addProductsToCallList(DepartmentName,[Department,Product,Price,Amount], CallList) ->

  if Department =:= DepartmentName ->
    ProductToAdd = [#departmentProduct{department = DepartmentName,
      product_name = Product,
      price = Price,
      expiry_time = get_time() + rand:uniform(),
      amount = Amount}],
    CallList++ProductToAdd;

    true -> CallList
  end.

get_time() -> 1000. %%FIXME global timer
% sendProductsToDepartment(tmp,tmp).
%[[Department,ProductName,Price,expiry],[meat,[115,116,101,97,107],80,0],[dairy,[99,104,101,101,115,101],10,0],[dairy,[109,105,108,107],5,0],[dairy,[121,111,103,117,114,116],3,0],[bakery,[98,114,101,97,100],8,0],[bakery,[98,117,110,115],20,0]]



sendProductsToDepartment(Department, DebarmentAndList)->
  writeToLogger("restock ",DebarmentAndList),
  gen_server:cast({global,Department},{restock, DebarmentAndList}). %List [{product_name,amount,price}]


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
  put(erlMarketBudget,1000000),
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

  department:start(meat),
  department:start(dairy),
  department:start(bakery),

  ListOfProductsToReserve =
    [[dairy, "milk", 5, 5, 50],
    [dairy, "yogurt", 1, 5, 3],
    [dairy, "cheese", 100, 5, 25],
    [meat, "steak", 17, 5, 33],
    [meat, "chicken", 9, 5, 7],
    [bakery, "bread", 9, 5, 77]],



  reserveTmp(ListOfProductsToReserve).

reserveTmp(RatioedList)->
  lists:foreach(
    fun(N) ->
      List = addProductsAMIR(N,RatioedList,[]),
      sendProductsToDepartment(N,List)
    end, ?DEPARTMENT_LIST).

addProductsAMIR(DepartmentName,[H|T],CallList) ->
  CallListToAdd = addProductsToCallListAMIR(DepartmentName,H,CallList),
  addProductsAMIR(DepartmentName,T,CallListToAdd);
addProductsAMIR(_DepartmentName,[],CallList) -> CallList.


addProductsToCallListAMIR(DepartmentName,[Department,Product,Price,Expiry,Amount], CallList) ->

  if Department =:= DepartmentName ->
    ProductToAdd = [#departmentProduct{department = DepartmentName,
    product_name = Product,
    price = Price,
    expiry_time = Expiry,
    amount = Amount}],
    CallList++ProductToAdd;

    true -> CallList
  end.
%%
%%addProductsDor([]) -> done;
%%addProductsDor([H|T]) ->
%%  Product_Name = H#departmentProduct.product_name,
%%  RequestedAmount = H#departmentProduct.amount,
%%  ExpiryTime = H#departmentProduct.expiry_time,
%%  addProductsDor(T).
%%
%%
%%


%%   RatioedList = getRatio(Tmp, 1000),
%%  initPurchaseDepartment().
%%  ErlMarketBudget = get(erlMarketBudget),
%%  purchaseDepartment ! {"add",5}.
%%  ratioToReserve(RatioedList, 1000, ErlMarketBudget).