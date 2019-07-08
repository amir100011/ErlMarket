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
  CanPay = canPay(ListOfProductsAndAmounts,CostumerBalance),
  case CanPay of
    canPay ->
      pay(ListOfProductsAndAmounts);
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
  canPay.



payToMaxAndReturnTheRest(ListOfProductsAndAmounts,CostumerBalance) ->
    null.

pay(ListOfProductsAndAmounts) ->
  AmountToAdd = sumOfAllProducts(ListOfProductsAndAmounts),
  updateErlMarketBalance(AmountToAdd).

sumOfAllProducts([]) -> 0;
sumOfAllProducts([H|T]) ->
  Sum = sumOfAllProducts(H,1) + sumOfAllProducts(T),
  Sum.

sumOfAllProducts({product,_department,_productName,PriceForEach, Amount},1) ->
  Sum = Amount * PriceForEach,
  {ok, S} = file:open("../Log.txt", [append]),
  io:format(S,"~s~w ~n",["Payment:Add to balance - ",Sum]),
  file:close(S).

updateErlMarketBalance(AmountToAdd) ->
  purchaseDepartment:updateBalance(AmountToAdd).

testPay() ->
  ShoppingList =
[{product,"meat","chicken",40,500},
 {product,"meat","steak",80,500},
 {product,"dairy","milk",5,40},
 {product,"dairy","yogurt",3,40},
 {product,"dairy","cheese",10,60},
 {product,"bakery","bread",8,100},
 {product,"bakery","buns",20,120}],

  pay(ShoppingList).





