%%%-------------------------------------------------------------------
%%% @author amir
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


callFunc(ListOfProductsAndAmounts, CostumerBalance) ->
  gen_server:call({global,?MODULE}, {pay,ListOfProductsAndAmounts, CostumerBalance}).

handle_call({pay,ListOfProductsAndAmounts, CostumerBalance}, _From, State) ->
  % checks if the customer can pay or not, if not pays to the maximum available, the rest are going back to the relevant department
  {CanPay, AmountToPay} = canPay(ListOfProductsAndAmounts,CostumerBalance),
  case CanPay of
    canPay ->
      pay(AmountToPay);
    cannotPay ->
      payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CostumerBalance)
  end,

  Reply = CanPay,  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {reply, Reply, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


canPay(ListOfProductsAndAmounts,CostumerBalance) ->
  SumOfAllProducts = sumOfAllProducts(ListOfProductsAndAmounts),
  if SumOfAllProducts =< CostumerBalance -> {canPay,SumOfAllProducts};
    true -> {cannotPay,0}
  end.

payToMaxAndReturnTheRest([],_CostumerBalance) ->[];
payToMaxAndReturnTheRest([H|T],CostumerBalance) ->
  {CanPay, AmountToPay} = canPay([H],CostumerBalance),
  case CanPay of
    canPay ->
      NewCostumerBalance = CostumerBalance - AmountToPay,
      writeNewBalanceToFile(CostumerBalance, NewCostumerBalance),
      pay(AmountToPay),
      payToMaxAndReturnTheRest(T,NewCostumerBalance);
    cannotPay ->
      returnProductToDepartment(H),
      payToMaxAndReturnTheRest(T, CostumerBalance)
  end.


pay(AmountToPay) ->
  updateErlMarketBalance(AmountToPay).

returnProductToDepartment({product,Department,ProductName,_PriceForEach, Amount}) ->
  gen_server:cast({global,Department},{return,ProductName,Amount,500}).%TODO: add expiry date

sumOfAllProducts([H|T]) ->
  sumOfAllProducts(H,1) + sumOfAllProducts(T);
sumOfAllProducts([]) -> 0.

sumOfAllProducts({product,_department,_productName,PriceForEach, Amount},1) ->
  Amount * PriceForEach.

updateErlMarketBalance(AmountToAdd) ->
  purchaseDepartment:updateBalance(AmountToAdd).


writeNewBalanceToFile(OldCustomerBalance, NewCustomerBalance) ->
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w~s~w ~n",["Customer:Oldbalance - ",OldCustomerBalance, " NewBalance - ", NewCustomerBalance]),
  file:close(S).

testPay() ->
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
  purchaseDepartment:setInitialBudget(),
  payToMaxAndReturnTheRest(ShoppingList, 985).





