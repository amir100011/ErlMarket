%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2019 4:48 PM
%%%-------------------------------------------------------------------
-module(costumer).
-author("dorliv").

%% API
-export([test_initCostumer/0]).
-record(costumer, {customer_id, budget, shopping_list}).
-define(MAXIMUM_BUDGET, 1000).
%-import(inventory,[initInventory/1, get_products_from_department/1]).


randomly_chose_products([], Ans) -> Ans;
randomly_chose_products([H|T], Ans) when Ans =:= [] ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> randomly_chose_products(T, [H]);
    true -> randomly_chose_products(T, Ans)
  end;
randomly_chose_products([H|T], Ans) ->
  AddToShoppingListRV =  rand:uniform(),
  TMP = Ans ++ [H],
  if
    AddToShoppingListRV< 0.8 -> randomly_chose_products(T, TMP);
    true -> randomly_chose_products(T, Ans)
  end.


initBudget() -> (1 - rand:uniform()) * ?MAXIMUM_BUDGET.

initCostumer() ->
  Node = node(),
  inventory:initInventory(Node),
  Budget = initBudget(),
  ShoppingList = fillShoppingList(),
  Costumer = #costumer{customer_id = self(), budget = Budget, shopping_list = ShoppingList},
  A = Costumer#costumer.budget,
  B = 5,
  ShoppingList.
  %goShopping(Costumer),
  %terminate().


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


terminate() ->
  masterFunction:updateNumberOfCostumers(-1).

test_initCostumer()->
  initCostumer().









