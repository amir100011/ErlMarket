%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2019 4:48 PM
%%%-------------------------------------------------------------------
-module(customer).
-author("dorliv").

%% API
-export([test_initCostumer/0, goShopping/1]).
-include_lib("records.hrl").
-record(customer, {customer_id, budget, shopping_list}).
-define(MAXIMUM_BUDGET, 1000).
%-import(inventory,[initInventory/1, get_products_from_department/1]).

createShoppingListEle(H)->
 % {RandomAmount, State} = random:uniform_s(10,random:seed()),
  RandomAmount = rand:uniform(10),
  Element = #shoppinlistelement{department_name = H#product.department,
                                product_name = H#product.product_name,
                                price = H#product.price,
                                amount = round(RandomAmount)},
  Element.

randomly_chose_products([], Ans) -> Ans;
randomly_chose_products([H|T], Ans) when Ans =:= [] ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
                                randomly_chose_products(T, [ShoppingListElement]);
    true -> randomly_chose_products(T, Ans)
  end;
randomly_chose_products([H|T], Ans) ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
                                randomly_chose_products(T, Ans ++ [ShoppingListElement]);
    true -> randomly_chose_products(T, Ans)
  end.


initBudget() -> (1 - rand:uniform()) * ?MAXIMUM_BUDGET.

initCostumer() ->
  Node = node(),
  inventory:initInventory(Node),
  Budget = initBudget(),
  ShoppingList = fillShoppingList(),
  Costumer = #customer{customer_id = self(), budget = Budget, shopping_list = ShoppingList},
  spawn(customer, goShopping, [Costumer]).

  %goShopping(Costumer),
  %terminate().
goShopping(Costumer)->
  put(customer_info, Costumer),
  io:fwrite("~p ~n",[Costumer]),
  A = 5.

fillShoppingList()->
  Departments = inventory:getDepartments(),
  fillFromDepartment(Departments,[]).


fillFromDepartment([], Ans) -> Ans;
fillFromDepartment([H| T], Ans) when Ans =:= [] ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = randomly_chose_products(ProductsFromDepartment, []),
  fillFromDepartment(T,ChosenProducts);
fillFromDepartment([H| T], Ans) ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = randomly_chose_products(ProductsFromDepartment, []),
  TMP = Ans ++ ChosenProducts,
  fillFromDepartment(T, TMP).

getBudget()->
  CostumerInfo = get(customer_info),
  CostumerInfo#customer.budget.


terminate() ->
  masterFunction:updateNumberOfCostumers(-1).

test_initCostumer()->
  initCostumer().









