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
-define(INTERVAL, 500). % 500 milliSeconds
-behavior(gen_server).
-author("amir").

%% API
-export([init/1, handle_call/3, handle_cast/2, start/0, handle_info/2]).
-export([setBalance/2]).
-export([setInitialBudget/0]).
-export([testReservation/0]).
-export([sumAmount/2]).
-export([getRatio/2,ratioToReserve/3]).
-export([writeToLogger/2]).


%%---------------------------------------------------------
%%                  GEN_SERVER FUNCTIONS
%%---------------------------------------------------------


start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  setInitialBudget(),
  erlang:send_after(?INTERVAL, self(), trigger),
  {ok, []}.

handle_info(trigger,State) ->
  writeToLogger("handle_info"),
  NumberOfCustomers = getNumberOfCustomers(),
  ErlMarketBudget = get(erlMarketBudget),
  ListOfValidProductsToReserve = getListOfProductsToReserve(?DEPARTMENT_LIST),
  ListOfValidProductsWithRatio = checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers),
  ratioToReserve(ListOfValidProductsWithRatio, NumberOfCustomers,ErlMarketBudget),
  erlang:send_after(?INTERVAL, self(), trigger), %% define new timer
  {noreply,State}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({updateBalance,Action,Amount}, State) ->
  setBalance(Action,Amount),
  {noreply,State};

handle_cast(terminate, State) ->
  writeToLogger("purchaseDeartment terminated"),
  exit(normal),
  {noreply,State}.


%%---------------------------------------------------------
%%                  PRIMARY FUNCTION
%%---------------------------------------------------------

%% @doc returns List of records %[{departmentProduct,department, product_name, price, expiry_time, amount}].
getListOfProductsToReserve([H|T]) ->
  getListOfProductsToReserveInternal(H) ++ getListOfProductsToReserve(T);
getListOfProductsToReserve([]) -> [].


%% @doc checks the ratio for each product (customer / number of products valid in depatment)
checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers) ->
  writeToLogger("checkProductStatus:ListOfValidProducts  - ",ListOfValidProductsToReserve),
  ListOfValidProductsWithRatio = getRatio(ListOfValidProductsToReserve, NumberOfCustomers),
  writeToLogger("checkProductStatus:ListOfValidProductsWithRatio  - ",ListOfValidProductsWithRatio),
  ListOfValidProductsWithRatio.


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

%%---------------------------------------------------------
%%                  SECONDARY FUNCTIONS
%%---------------------------------------------------------

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

reserve(ListOfValidProductsWithRatio, CostOfReservation) ->
  setBalance(deduce, CostOfReservation),
  lists:foreach(
    fun(N) ->
      ListOfProductsToAddToDepartment = addProducts(N,ListOfValidProductsWithRatio,[]),
      sendProductsToDepartment(N,ListOfProductsToAddToDepartment)
    end, ?DEPARTMENT_LIST).

addProducts(DepartmentName,[H|T],CallList) ->
  CallListToAdd = addProductsToCallList(DepartmentName,H,CallList),
  addProducts(DepartmentName,T,CallListToAdd);
addProducts(_DepartmentName,[],CallList) -> CallList.

%%---------------------------------------------------------
%%                  INTERNAL FUNCTIONS
%%---------------------------------------------------------

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


%%---------------------------------------------------------
%%            GETTERS AND SETTERS FUNCTIONS
%%---------------------------------------------------------

%% @doc returns the current number of customers in ErlMarket
getNumberOfCustomers() ->
  gen_server:call({global,masterFunction}, getNumberOfCustomers).
%%    masterFunction:castFunc(getNumberOfCustomers).
  % global:send(masterFunction,{"getNumberOfCustomers"}).

getListOfProductsToReserveInternal(DepartmentName) ->
  ListNotOrganized = gen_server:call({global,DepartmentName},getTotalAmountOfValidProduct),
  sumAmount(ListNotOrganized, DepartmentName).

get_time() -> 1000. %%FIXME global timer

setInitialBudget() ->
  put(erlMarketBudget,1000000),
  writeToLogger("setInitialBudget ", get(erlMarketBudget)).

setBalance(TypeOfAction , Amount) ->
  writeToLogger("setBalance: ", [TypeOfAction,Amount]),
  OldBalance = get(erlMarketBudget),
  writeToLogger("OldBalance: ", OldBalance),
  case TypeOfAction of
    add -> put(erlMarketBudget, OldBalance + Amount);
    deduce -> put(erlMarketBudget, OldBalance - Amount);
  _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
  end,
  NewBalance = get(erlMarketBudget),
  writeToLogger("updateBalance: OldBalance: ",OldBalance, " NewBalance:",NewBalance).

sendProductsToDepartment(Department, DebarmentAndList)->
  writeToLogger("restock ",DebarmentAndList),
  gen_server:cast({global,Department},{restock, DebarmentAndList}).


%%---------------------------------------------------------
%%                 WRITE TO LOGGER FUNCTIONS
%%---------------------------------------------------------


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

%%---------------------------------------------------------
%%                 TEST FUNCTIONS
%%---------------------------------------------------------


testReservation() ->

  purchaseDepartment:start().

%%  department:start(meat),
%%  department:start(dairy),
%%  department:start(bakery),
%%
%%  ListOfProductsToReserve =
%%    [[dairy, "milk", 5, 5, 50],
%%    [dairy, "yogurt", 1, 5, 3],
%%    [dairy, "cheese", 100, 5, 25],
%%    [meat, "steak", 17, 5, 33],
%%    [meat, "chicken", 9, 5, 7],
%%    [bakery, "bread", 9, 5, 77]],
%%
%%
%%
%%  reserveTmp(ListOfProductsToReserve).
%%
%%reserveTmp(RatioedList)->
%%  lists:foreach(
%%    fun(N) ->
%%      List = addProductsAMIR(N,RatioedList,[]),
%%      sendProductsToDepartment(N,List)
%%    end, ?DEPARTMENT_LIST).
%%
%%addProductsAMIR(DepartmentName,[H|T],CallList) ->
%%  CallListToAdd = addProductsToCallListAMIR(DepartmentName,H,CallList),
%%  addProductsAMIR(DepartmentName,T,CallListToAdd);
%%addProductsAMIR(_DepartmentName,[],CallList) -> CallList.
%%
%%
%%addProductsToCallListAMIR(DepartmentName,[Department,Product,Price,Expiry,Amount], CallList) ->
%%
%%  if Department =:= DepartmentName ->
%%    ProductToAdd = [#departmentProduct{department = DepartmentName,
%%    product_name = Product,
%%    price = Price,
%%    expiry_time = Expiry,
%%    amount = Amount}],
%%    CallList++ProductToAdd;
%%
%%    true -> CallList
%%  end.
%%%%
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

