%%%-------------------------------------------------------------------
%%% @author
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2019 14:22
%%%-------------------------------------------------------------------
-module(cashierServer).
-behavior(gen_server).
-define(LOGGER_FILE_PATH, "../Logger-CashierServer.txt").
-author("amir").
-include_lib("records.hrl").
%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([start/0,callFunc/1,castFunc/1, payToMaxAndReturnTheRest/2, payToMaxAndReturnTheRest/4]).
%%
%%-export([testPay/0]).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, []}.


%%callFunc(ListOfProductsAndAmounts, CustomerBalance) ->
%%  gen_server:cast({global,?MODULE}, {pay,ListOfProductsAndAmounts, CustomerBalance}).
callFunc(MSG) ->
  try gen_server:call({global, ?MODULE}, MSG) of
    AnsFromServer -> AnsFromServer
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable ," ~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      case interface:callFunc(isFinished) of
          false ->     Ans = callFunc(MSG),
                       Ans;
           true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
      end
  end.
%% @doc interface function for using gen_server cast
castFunc(Message) ->
  try  gen_server:cast({global, ?MODULE}, Message) of
    AnsFromServer-> AnsFromServer  % usually no reply just ok or some atom
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      case interface:callFunc(isFinished) of
            false -> castFunc(Message);
            true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
      end
  end.



%% @doc returns its own PID for watchdog monitoring
handle_call(pid, _From, State) ->
  Reply= self(),
  {reply, Reply, State}.


handle_cast(terminate, State) ->
  %terminate(0,0),
  {stop, normal, State};

%% @doc  checks if the customer can pay or not, if not pays to the maximum available, the rest are going back to the relevant department
handle_cast({pay,ListOfProductsAndAmounts,CustomerBalance}, State) ->
  %writeToLogger("Handle Cast - CashierServer ",[ListOfProductsAndAmounts,CustomerBalance]),
  pay(ListOfProductsAndAmounts, CustomerBalance),
 {noreply, State}.

handle_info(MSG, State)->
  io:fwrite("cashierServer Department gets ~p~n",[MSG]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:fwrite("~p says bye bye ~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


printBoughtListToLogger(ListOfBoughtProducts) ->
  writeToLogger("Bought Products ~p ~n",[ListOfBoughtProducts]).

pay(ListOfProductsAndAmounts,CustomerBalance)->
  %writeToLogger("payFunction ",[ListOfProductsAndAmounts,CustomerBalance]),
  {CanPay, AmountToPay} = canPay(ListOfProductsAndAmounts,CustomerBalance),
  case CanPay of
    canPay ->
      updateErlMarketBalance(AmountToPay),
      printBoughtListToLogger(ListOfProductsAndAmounts);
    cannotPay ->
      spawn(cashierServer, payToMaxAndReturnTheRest, [ListOfProductsAndAmounts, CustomerBalance])
  end.


canPay(ListOfProductsAndAmounts,CustomerBalance) ->
  SumOfAllProducts = sumOfAllProducts(ListOfProductsAndAmounts),
  if SumOfAllProducts =< CustomerBalance -> {canPay, SumOfAllProducts};
    true -> {cannotPay,0}
  end.
payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance)->
  spawn_monitor(?MODULE, payToMaxAndReturnTheRest, [ListOfProductsAndAmounts, CustomerBalance, dict:new(), []]),
  receive
    {'DOWN', _, process, _, normal} -> done;
    {'DOWN', _, process, PID, Reason} ->
      writeToLogger(variable, "Process ~p Died because ~p , respawning a new process~n",[PID, Reason]),
      payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance)
  end.
  %payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance, dict:new(), []).

payToMaxAndReturnTheRest([],_CustomerBalance, Dict, ListOfProducts) ->
  case dict:is_key(amountToPay, Dict) of % when the customer didnt buy anything this will be false
    true ->  {Amount, UpdatedDict} = dict:take(amountToPay, Dict),
             updateErlMarketBalance(Amount);
    false -> UpdatedDict = Dict
  end,
  writeToLogger(variable, "Bought Products by Customer is : ~p ~n", [ListOfProducts]), % for debugging
  returnProducts(dict:fetch_keys(UpdatedDict), UpdatedDict); % return products to departments
payToMaxAndReturnTheRest([H|T],CustomerBalance, Dict, ListOfProducts) ->
  {CanPay, AmountToPay} = canPay([H],CustomerBalance),
  case CanPay of
    canPay ->
      NewCustomerBalance = CustomerBalance - AmountToPay,
      %writeToLogger("Customer:Oldbalance - ",CustomerBalance, " NewBalance - ", NewCustomerBalance),
      case dict:is_key(amountToPay, Dict) of
        false -> DictUpdated = dict:store(amountToPay, AmountToPay, Dict);
        true -> OldAmount = dict:fetch(amountToPay, Dict),
                DictUpdated = dict:store(amountToPay, AmountToPay + OldAmount, Dict)
      end,
      %updateErlMarketBalance(AmountToPay),
      payToMaxAndReturnTheRest(T, NewCustomerBalance, DictUpdated, ListOfProducts ++ [H]);
    cannotPay ->
      DepartmentName = H#departmentProduct.department,
      case dict:is_key(DepartmentName, Dict) of
        false -> DictUpdated = dict:store(DepartmentName, [H], Dict);
        true ->  DictUpdated = dict:append(DepartmentName, H, Dict)
      end,
      %returnProductToDepartment(H),
      payToMaxAndReturnTheRest(T, CustomerBalance, DictUpdated, ListOfProducts)
  end.

returnProducts([],_) -> done;
returnProducts([H|T], Dict) ->

  {ListOfProduct, UpdatedDict} = dict:take(H, Dict),
  returnProductToDepartment(ListOfProduct, H),
  returnProducts(T, UpdatedDict).

returnProductToDepartment(DepartmentProducts, Department) ->
  department:castFunc(Department, {return, DepartmentProducts}). % TODO change at amir
  %gen_server:cast({global,Department},{return, DepartmentProducts}).

%returnProductToDepartment({departmentProduct,Department,ProductName,_PriceForEach, Expiry, Amount}) ->
%  gen_server:cast({global,Department},{return,[{departmentProduct,Department,ProductName,_PriceForEach, Expiry, Amount}]}).

sumOfAllProducts([H|T]) ->
  costOfSingleProduct(H) + sumOfAllProducts(T);
sumOfAllProducts([]) -> 0.

costOfSingleProduct({departmentProduct,_department,_productName,PriceForEach,_expiryDate, Amount}) ->
  Amount * PriceForEach.

updateErlMarketBalance(AmountToAdd) ->
  purchaseDepartment:castFunc({updateBalance, add, AmountToAdd}).
  %gen_server:cast({global,purchaseDepartment}, {updateBalance, add, AmountToAdd}).

%%------------------WRITING TO LOGGER------------------

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).

writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).


%%------------------TEST FUNCTIONS------------------

%%testPay() ->
%%  inventory:initInventory(node()),
%%  cashierServer:start(),
%%  purchaseDepartment:initPurchaseDepartment(),
%%  department:start(meat),
%%  department:start(dairy),
%%  department:start(bakery),
%%  [ShoppingList, Balance] =
%%    [[{departmentProduct,dairy,"milk",5,40,2},
%%      {departmentProduct,dairy,"yogurt",3,40,3},
%%      {departmentProduct,dairy,"cheese",10,60,5},
%%      {departmentProduct,meat,"chicken",40,500,5},
%%      {departmentProduct,meat,"steak",80,500,2},
%%      {departmentProduct,bakery,"bread",8,100,10}],
%%      6108.332946432113],
%%  {AvailableProductsToPurchase, _} = customer:initCustomer(),
%%  MeatListProductsPrepurchase = department:callFunc(meat, getProducts),
%%  DairyListProductsPrepurchase = department:callFunc(dairy, getProducts),
%%  BakeryListProductsPrepurchase = department:callFunc(bakery, getProducts),
%%  pay(AvailableProductsToPurchase, 0),
%%  timer:sleep(500),
%%  %gen_server:cast({global,?MODULE},{pay, AvailableProductsToPurchase, 40}),
%%  MeatListProductsPostpurchase = department:callFunc(meat, getProducts),
%%  DairyListProductsPostpurchase = department:callFunc(dairy, getProducts),
%%  BakeryListProductsPostpurchase = department:callFunc(bakery, getProducts),
%%  A = 5.
%%
