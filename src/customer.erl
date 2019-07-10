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
-export([testInit/0, goShopping/1]).
-include_lib("records.hrl").
-define(DEPARTMENT_LIST, inventory:getDepartments()).
-record(customer, {customer_id, budget, shopping_list}).
-define(MAXIMUM_BUDGET, 1000).



%%----------------PRIMARY FUNCTION-------------------------

%% @doc initialize the customer and spawn a customer process that shops in ErlMarket
initCostumer() ->
  inventory:initInventory([node()]), % TODO once we have nodes we should initialize the Inventory once for all nodes, so in future design we delete this line
  Costumer = #customer{customer_id = self(), budget = initBudget(), shopping_list = createShuffledShoppingList()},
  spawn(customer, goShopping, [Costumer]).

%% TODO need to implement costumer goes shopping func
goShopping(Customer)->
  put(customerInfo, Customer),
  AvailiableProductsToPurchase = getProductsFromDepartments(Customer#customer.shopping_list,shuffleList(?DEPARTMENT_LIST)),
  writeToLogger("customerInfo: ", Customer),
  pay().
  %terminate().



%%----------------INTERNAL FUNCTIONS-------------------------


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
randomlyChooseProducts([], Ans) -> Ans;
randomlyChooseProducts([H|T], Ans) when Ans =:= [] ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
                                randomlyChooseProducts(T,[ShoppingListElement]);
    true -> randomlyChooseProducts(T, Ans)
  end;
randomlyChooseProducts([H|T], Ans) ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
                                randomlyChooseProducts(T, Ans ++ [ShoppingListElement]);
    true -> randomlyChooseProducts(T, Ans)
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

%% @doc creates a random shuffled shopping list for the customer's process
createShuffledShoppingList()->
  Departments = inventory:getDepartments(),
  OrderedShoppingList = chooseRandomProductsFromDepartment(Departments,[]),
  shuffleList(OrderedShoppingList).

shuffleList(ShoppingList) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])].


chooseRandomProductsFromDepartment([], Ans) -> Ans;
chooseRandomProductsFromDepartment([H| T], Ans) when Ans =:= [] ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = randomlyChooseProducts(ProductsFromDepartment, []),
  chooseRandomProductsFromDepartment(T,ChosenProducts);
chooseRandomProductsFromDepartment([H| T], Ans) ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = randomlyChooseProducts(ProductsFromDepartment, []),
  TMP = Ans ++ ChosenProducts,
  chooseRandomProductsFromDepartment(T, TMP).


getBudget()->
  CostumerInfo = get(customer_info),
  CostumerInfo#customer.budget.


terminate() ->
  masterFunction:updateNumberOfCostumers("terminame").

testInit()->
%%  department:start(meat),
%%  department:start(dairy),
%%  department:start(bakery),
%%  initCostumer().


  ShoppingList = [{shoppinlistelement,bakery,"buns",20,4},
    {shoppinlistelement,meat,"steak",80,4},
    {shoppinlistelement,meat,"chicken",40,4},
    {shoppinlistelement,dairy,"yogurt",3,2},
    {shoppinlistelement,bakery,"bread",8,5},
    {shoppinlistelement,dairy,"cheese",10,9},
    {shoppinlistelement,dairy,"milk",5,10}],

X = getProductsFromDepartments(ShoppingList,[meat,dairy,bakery]),
R = 5.



getProductsFromDepartments(ShoppingList, [H|T]) ->
  [getAllProductsInShoppingListThatBelongToDepartment(ShoppingList, H)]++getProductsFromDepartments(ShoppingList,T);
  getProductsFromDepartments(_ShoppingList,[]) -> [].


%% @doc this function should be used by the customer process before shopping at a certain department
%TODO try minmize it to O(N) instead of O(N^2) by clearing the department elements from the original shopping list
getAllProductsInShoppingListThatBelongToDepartment([],_DepartmentName) -> [];
getAllProductsInShoppingListThatBelongToDepartment([H|T], DepartmentName) ->
  ProductDepartmentName = H#shoppinlistelement.department_name,
  case ProductDepartmentName of
    DepartmentName -> getAllProductsInShoppingListThatBelongToDepartment(T, DepartmentName) ++ [H];
    _ -> getAllProductsInShoppingListThatBelongToDepartment(T, DepartmentName)
  end.


pay() ->
  writeToLogger("i'm paying"),
  gen_server:cast({global,cashierServer}, {pay,get(customerInfo)}).


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



