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
-define(DESIRED_RATIO, 2).
-define(SAVING_RATIO, 0.8).
-define(LOGGER_FILE_PATH, "../Logger-PurchaseDepartment.txt").
-define(INTERVAL, 2000). % 5000 milliSeconds
-define(MAX_NUMBER_OF_REQUESTS, 1000).
-behavior(gen_server).
-author("amir").

%% API
-export([init/1, handle_call/3, handle_cast/2, start/0, handle_info/2, callFunc/1]).
-export([setBalance/1]).
-export([setInitialBudget/0]).
-export([testReservation/0]).
-export([sumAmount/2]).
-export([getRatio/2,ratioToReserve/4]).
-export([writeToLogger/2, castFunc/1, terminate/2]).


%%---------------------------------------------------------
%%                  GEN_SERVER FUNCTIONS
%%---------------------------------------------------------
%% @doc interface function for using gen_server cast
castFunc(Message) ->
  try gen_server:cast({global, ?MODULE}, Message) of
    AnsFromServer-> AnsFromServer  % usually no reply just ok or some atom
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
     case interface:callFunc(isFinished) of
         false -> castFunc(Message);
         true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
     end
  end.


callFunc(Message) ->
  try gen_server:call({global,?MODULE}, Message) of
    AnsFromServer -> AnsFromServer
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable ," ~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      case interface:callFunc(isFinished) of
                false ->     Ans = callFunc(Message),
                             Ans;
                true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
      end
  end.

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  setInitialBudget(),
  resetIterationCounter(),
  erlang:send_after(?INTERVAL, self(), trigger),
  {ok, 0}.

handle_info(trigger, State) when is_integer(State)->
  writeToLogger("handle_info"),
  Time = State,
  NumberOfCustomers = getNumberOfCustomers(),
  ErlMarketBudget = getBalanceWithAccumulatedChanges(),
  ListOfValidProductsToReserve = getListOfProductsToReserve(?DEPARTMENT_LIST, Time),
  ListOfValidProductsWithRatio = checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers),
  ratioToReserve(ListOfValidProductsWithRatio, Time, NumberOfCustomers,ErlMarketBudget),
  erlang:send_after(?INTERVAL, self(), trigger), %% define new timer
  {noreply, State};

handle_info(trigger, State) when is_atom(State)->
  writeToLogger("trigger shut down"),
  castFunc(terminate),
  {noreply, State};

handle_info(MSG, State)->
  io:fwrite("purchase Department gets ~p~n",[MSG]),
  {noreply, State}.

handle_call(pid, _From, State) ->
  Reply= self(),
  {reply, Reply, State}.


handle_cast({updateTime,Time}, _) ->
  {noreply, Time};
handle_cast({updateBalance,Action,Amount}, State) ->
  %writeToLogger("handleCast balance", [Action, Amount]),
  CurrentIteration = updateIterationCounter(),
  if CurrentIteration < 1000 -> accumulateChanges(Action,Amount);
    true -> resetIterationCounter(), setBalance(Amount)
  end,

  {noreply,State};

handle_cast(terminate, State) when is_integer(State) ->
  %writeToLogger("purchaseDepartment begin termination sequence"),
  {noreply, stoptrigger};

handle_cast(terminate, State) when is_atom(State) ->
  writeToLogger("purchaseDepartment terminated"),
  {stop, normal, State}.



terminate(Reason, _State) ->
  io:fwrite("~p says bye bye ~n",[?MODULE]),
  ets:delete(budget),
  writeToLogger("purchaseDepartment Termination Reason: ", [Reason]),
  ok.
%%---------------------------------------------------------
%%                  PRIMARY FUNCTION
%%---------------------------------------------------------

%% @doc returns List of records %[{departmentProduct,department, product_name, price, expiry_time, amount}].
getListOfProductsToReserve([H|T], Time) ->
  getListOfProductsToReserveInternal(H, Time) ++ getListOfProductsToReserve(T, Time);
getListOfProductsToReserve([], _) -> [].


%% @doc checks the ratio for each product (customer / number of products valid in depatment)
checkProductStatus(ListOfValidProductsToReserve, NumberOfCustomers) ->
  %writeToLogger("checkProductStatus:ListOfValidProducts  - ",ListOfValidProductsToReserve),
  ListOfValidProductsWithRatio = getRatio(ListOfValidProductsToReserve, NumberOfCustomers),
  %writeToLogger("checkProductStatus:ListOfValidProductsWithRatio  - ",ListOfValidProductsWithRatio),
  ListOfValidProductsWithRatio.


ratioToReserve(ListOfValidProductsWithRatio, Time, NumberOfCustomers, ErlMarketBudget) ->
  CostOfReservation = priceOfReservation(ListOfValidProductsWithRatio),
  if CostOfReservation =< ErlMarketBudget ->
   % writeToLogger("ratioToReserve Success: PriceOfReservation - " , CostOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
    reserve(ListOfValidProductsWithRatio, Time, CostOfReservation),
    %writeToLogger("reserve - ", ListOfValidProductsWithRatio),
    ErlMarketBudgetNew = getBalanceWithAccumulatedChanges(),
    interface:castFunc({budgetVsExpense, ErlMarketBudgetNew, CostOfReservation});
    true ->
      %writeToLogger("ratioToReserve Failed: PriceOfReservation - " , CostOfReservation, " ErlMarketBudget - ", ErlMarketBudget),
      DeltaRatio = (?SAVING_RATIO * ErlMarketBudget) / CostOfReservation,
      ListOfValidProductsWithNewRatio =  editRatio(ListOfValidProductsWithRatio,DeltaRatio),
      ratioToReserve(ListOfValidProductsWithNewRatio, Time, NumberOfCustomers, ErlMarketBudget)
  end.

%%---------------------------------------------------------
%%                  SECONDARY FUNCTIONS
%%---------------------------------------------------------

getRatio([H|T],NumberOfCustomers)->
  [getRatioSingleElement(H,NumberOfCustomers)] ++ getRatio(T,NumberOfCustomers);
getRatio([],_NumberOfCustomers)->[].

  getRatioSingleElement({departmentProduct,Department, Product_name, _, _Expiry_time, Amount},NumberOfCustomers) ->
  Price = inventory:getProdcutPrice(Product_name),  % Todo Change this function at amir to support sales
  %writeToLogger("getRatioSingleElement",[{departmentProduct,Department, Product_name, Price, _Expiry_time, Amount},NumberOfCustomers]),
  NumberOfProductsToOrder = round((NumberOfCustomers * ?DESIRED_RATIO) - Amount) + 1,
  if  NumberOfProductsToOrder =< 1 -> AmountToOrder = 0;
    true -> AmountToOrder = NumberOfProductsToOrder
  end,
  [Department,Product_name, Price,AmountToOrder].

reserve(ListOfValidProductsWithRatio, Time, CostOfReservation) ->
  updateIterationCounter(),
  accumulateChanges(deduce,CostOfReservation/2),
  lists:foreach(
    fun(N) ->
      ListOfProductsToAddToDepartment = addProducts(N,Time,ListOfValidProductsWithRatio,[]),
      sendProductsToDepartment(N,ListOfProductsToAddToDepartment)
    end, ?DEPARTMENT_LIST).

addProducts(DepartmentName,Time,[H|T],CallList) ->
  CallListToAdd = addProductsToCallList(DepartmentName,Time,H,CallList),
  addProducts(DepartmentName,Time,T,CallListToAdd);
addProducts(_DepartmentName,_Time,[],CallList) -> CallList.

%%---------------------------------------------------------
%%                  INTERNAL FUNCTIONS
%%---------------------------------------------------------

editRatio([H|T],DeltaRatio) ->
  [editRatio(H,DeltaRatio,1)] ++ editRatio(T,DeltaRatio);
editRatio([],_DeltaRatio) -> [].

editRatio([Department,Product_name,Price,AmountToOrder], DeltaRatio, 1) ->
  [Department,Product_name,Price,round(AmountToOrder*DeltaRatio) + 1].

priceOfReservation([H|T]) ->
  priceOfReservationSingleElement(H) + priceOfReservation(T);
priceOfReservation([]) -> 0.

priceOfReservationSingleElement([_Department,_Product_name,Price,AmountToOrder]) ->
  Price * AmountToOrder.

addProductsToCallList(DepartmentName,Time,[Department,Product,Price,Amount], CallList) ->

  if Department =:= DepartmentName andalso  Amount > 0 ->
    ProductToAdd =
      [#departmentProduct{
        department = DepartmentName,
        product_name = Product,
        price = Price,
        expiry_time =  Time + round(500*rand:uniform()),
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
  try masterFunction:callFunc(getNumberOfCustomers) of
    Ans -> Ans
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"FROM ~p: MasterFunction is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      getNumberOfCustomers()
  end.

getListOfProductsToReserveInternal(DepartmentName, Time) ->
  %ListNotOrganized = gen_server:call({global,DepartmentName}, {getTotalAmountOfValidProduct, Time}),
  ListNotOrganized = department:callFunc(DepartmentName,{getTotalAmountOfValidProduct, Time}), % TODO change at amir
  sumAmount(ListNotOrganized, DepartmentName).

getBalanceWithAccumulatedChanges()->
  [{_Budget,ErlMarketBudget}] = ets:lookup(budget,erlMarketBudget),
  Budget = ErlMarketBudget + get(erlMarketBudgetChanges),
  %writeToLogger("getBalanceWithAccumulatedChanges", [Budget]),
  Budget.

%% @doc initialize process's dictionary fields and creates ETS table for the budget
setInitialBudget() ->
  put(erlMarketBudgetChanges,0),
  put(erlMarketBudget,1000000),
  ets:new(budget,[set, public, named_table]),
  ets:insert(budget,{erlMarketBudget, 1000000}).
  %writeToLogger("setInitialBudget ", get(erlMarketBudgetChanges)).

%% @doc updates the ErlMarket budget in ETS table and also in process dictionary
setBalance(Amount) ->
  OldBalance = get(erlMarketBudget),
  NewBalance = OldBalance + Amount,
  ets:insert(budget,{erlMarketBudget, NewBalance}),
  %writeToLogger("SetBalance [OldBudget,TotalChangesAmount,NewBudget]",[OldBalance, Amount, ets:lookup(budget,erlMarketBudget)]),
  put(erlMarketBudget, NewBalance),
  put(erlMarketBudgetChanges, 0).

%% @doc updates the total amount of change in the ErlMarket's budget
accumulateChanges(TypeOfAction , Amount) ->
  %writeToLogger("accumulateChanges: ", [TypeOfAction,Amount]),
  %writeToLogger("numberOfIteration: " ,[get(iterationCounter)]),
  OldChangesAccumulator = get(erlMarketBudgetChanges),
  case TypeOfAction of
    add -> put(erlMarketBudgetChanges, OldChangesAccumulator + Amount);
    deduce -> put(erlMarketBudgetChanges, OldChangesAccumulator - Amount);
  _TypeOfAction -> writeToLogger("wierd got: ",TypeOfAction)
  end,
  UpdatedChangesAccumulator = get(erlMarketBudgetChanges),
  writeToLogger("accumulateChanges: OldChangesAccumulator: ",OldChangesAccumulator, " UpdatedChangesAccumulator:",UpdatedChangesAccumulator).

sendProductsToDepartment(Department, DepartmentAndList)->
  writeToLogger("restock ",DepartmentAndList),
  department:castFunc(Department, {restock, DepartmentAndList}).  % TODO change at amir
  %gen_server:cast({global,Department},{restock, DepartmentAndList}).

resetIterationCounter() ->
  put(iterationCounter , 0).

updateIterationCounter() ->
  put(iterationCounter , get(iterationCounter) + 1).



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

writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).

%%---------------------------------------------------------
%%                 TEST FUNCTIONS
%%---------------------------------------------------------


testReservation() ->
  setInitialBudget(),
  setBalance(100),
  setBalance(-150),
  [{_Budget,ErlMarketBudget}] = ets:lookup(budget,erlMarketBudget),
  ErlMarketBudget.
%%  purchaseDepartment:start().



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

