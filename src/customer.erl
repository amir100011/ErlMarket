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

%% @doc create a shoppinglist element for the customer's shopping list
createShoppingListEle(H)->
 % {RandomAmount, State} = random:uniform_s(10,random:seed()),
  RandomAmount = rand:uniform(10),
  Element = #shoppinlistelement{department_name = H#product.department,
                                product_name = H#product.product_name,
                                price = H#product.price,
                                amount = round(RandomAmount)},
  Element.

%% @doc create the shopping list randomly from the list of products
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

%% @doc for debuggging TODO delete before submission
getList(Department)->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(Department)]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  ListAns.

%% @doc initialize the customer and spawn a customer process that shops in ErlMarket
initCostumer() ->
  Node = node(),
  inventory:initInventory([Node]), % TODO once we have nodes we should initialize the Inventory once for all nodes, so in future design we delete this line
  Budget = initBudget(),
  ShoppingList = fillShoppingList(),
  ReshuffledShoppingList = [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])],
  Costumer = #customer{customer_id = self(), budget = Budget, shopping_list = ReshuffledShoppingList},
  spawn(customer, goShopping, [Costumer]).


  %goShopping(Costumer),
  %terminate().


%% TODO need to implement costumer goes shopping func
goShopping(Costumer)->
  put(customer_info, Costumer),
  io:fwrite("~p ~n",[Costumer]),
  A = 5.

%% @doc fill the customer's shopping list
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


%% @doc this function should be used by the customer process before shopping at a certain department
getDepartmentProductFromShoppingList([],_) -> [];
getDepartmentProductFromShoppingList([H|T], DepartmentName) ->
  ProductDepartmentName = H#shoppinlistelement.department_name,
  case ProductDepartmentName of
    DepartmentName -> getDepartmentProductFromShoppingList(T, DepartmentName) ++ [H];
    _ -> getDepartmentProductFromShoppingList(T, DepartmentName)
  end.





