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
-author("amir").

%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([start/0,callFunc/2]).

-export([testPay/0]).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, []}.


callFunc(ListOfProductsAndAmounts, CustomerBalance) ->
  gen_server:call({global,?MODULE}, {pay,ListOfProductsAndAmounts, CustomerBalance}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @doc  checks if the customer can pay or not, if not pays to the maximum available, the rest are going back to the relevant department
handle_cast({pay,ListOfProductsAndAmounts,CustomerBalance}, State) ->
  writeToLogger("Handle Cast - CashierServer ",[ListOfProductsAndAmounts,CustomerBalance]),
  {CanPay, AmountToPay} = canPay(ListOfProductsAndAmounts,CustomerBalance),
  case CanPay of
    canPay ->
      updateErlMarketBalance(AmountToPay);
    cannotPay ->
      payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance)
  end,
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


canPay(ListOfProductsAndAmounts,CustomerBalance) ->
  SumOfAllProducts = sumOfAllProducts(ListOfProductsAndAmounts),
  if SumOfAllProducts =< CustomerBalance -> {canPay,SumOfAllProducts};
    true -> {cannotPay,0}
  end.

payToMaxAndReturnTheRest([],_CustomerBalance) ->[];
payToMaxAndReturnTheRest([H|T],CustomerBalance) ->
  {CanPay, AmountToPay} = canPay([H],CustomerBalance),
  case CanPay of
    canPay ->
      NewCustomerBalance = CustomerBalance - AmountToPay,
      writeToLogger("Customer:Oldbalance - ",CustomerBalance, " NewBalance - ", NewCustomerBalance),
      updateErlMarketBalance(AmountToPay),
      payToMaxAndReturnTheRest(T,NewCustomerBalance);
    cannotPay ->
      returnProductToDepartment(H),
      payToMaxAndReturnTheRest(T, CustomerBalance)
  end.


returnProductToDepartment({product,Department,ProductName,_PriceForEach, Amount}) ->
  gen_server:cast({global,Department},{return,ProductName,Amount,500}).%TODO: add expiry date

sumOfAllProducts([H|T]) ->
  sumOfAllProducts(H,1) + sumOfAllProducts(T);
sumOfAllProducts([]) -> 0.

sumOfAllProducts({product,_department,_productName,PriceForEach, Amount},1) ->
  Amount * PriceForEach.

updateErlMarketBalance(AmountToAdd) ->
  purchaseDepartment:setBalance("add", AmountToAdd).


%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment
writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file("../Log.txt", io_lib:format("~p.~n", [List]), [append]).

writeToLogger(String) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).


%%------------------TEST FUNCTIONS------------------

testPay() ->
  cashierServer:start(),
  purchaseDepartment:setInitialBudget(),
  ShoppingList =
    [{product,meat,"chicken",40,500},
      {product,meat,"steak",80,500},
      {product,dairy,"milk",5,40},
      {product,dairy,"yogurt",3,40},
      {product,dairy,"cheese",10,60},
      {product,bakery,"bread",8,100},
      {product,bakery,"buns",20,120}],

  department:start(meat),
  department:start(dairy),
  department:start(bakery),
  gen_server:cast({global,?MODULE},{pay,ShoppingList, 50000000}).

