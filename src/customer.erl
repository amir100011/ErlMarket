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
-export([testInit/0, initCustomer/1, getBudget/0,goShopping/1]).
-include_lib("records.hrl").
-define(LOGGER_FILE_PATH, "../Logger-Customer.txt").
-record(customer, {customer_id, budget, shopping_list}).
-define(MAXIMUM_BUDGET, 600).
-define(MAXITERATIONS, 5).



%%----------------PRIMARY FUNCTION-------------------------

%% @doc initialize the customer and spawn a customer process that shops in ErlMarket
initCustomer(TimeStamp) ->
  spawn(customer, goShopping, [TimeStamp]).

%% @doc the life cycle of a customer
goShopping(TimeStamp)->
  timer:sleep(round(rand:uniform() * 2000)),
  %writeToLogger("reached to goShopping"),
  Customer = #customer{customer_id = self(), budget = initBudget(), shopping_list = createShuffledShoppingList()},
  put(customerInfo, Customer),
  %writeToLogger("customerInfo: ", Customer),
  UniqueShuffledDepartmentList = shuffleList(?DEPARTMENT_LIST),
  OrderShoppingListByDepartments = getProductsFromDepartments(Customer#customer.shopping_list,UniqueShuffledDepartmentList),
  put(requesIteration, 0),
  AvailableProductsToPurchase = takeTheProductsFromTheDifferentDepartments(OrderShoppingListByDepartments, UniqueShuffledDepartmentList, TimeStamp),
  writeToLogger(variable, "customerID ~p has AvailableProductsToPurchase: ~n ~p ~n ", [self(), AvailableProductsToPurchase]),
  %{AvailableProductsToPurchase, Customer}.
  pay(AvailableProductsToPurchase,Customer#customer.budget),
  lists:foreach(fun(E) -> department:castFunc(E, getProducts) end, ?DEPARTMENT_LIST),
  terminate().



%%----------------INTERNAL FUNCTIONS-------------------------


%% @doc create a shoppinglist element for the customer's shopping list
createShoppingListEle(H)->
 % {RandomAmount, State} = random:uniform_s(10,random:seed()),
  RandomAmount = rand:uniform(6),
  Element = #shoppinlistelement{department_name = H#product.department,
                                product_name = H#product.product_name,
                                price = H#product.price,
                                amount = round(RandomAmount)},
  Element.

%% @doc creates a random shuffled shopping list for the customer's process
createShuffledShoppingList()->
  OrderedShoppingList = chooseRandomProductsFromDepartment(?DEPARTMENT_LIST,[]),
  shuffleList(OrderedShoppingList).

%% @doc chooses for each existing product in the department if include it in the shopping list or not
chooseRandomProductsFromDepartment([], Ans) -> Ans;
chooseRandomProductsFromDepartment([H|T], Ans) when Ans =:= [] ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = chooseRandomProductsFromDepartmentInternal(ProductsFromDepartment, []),
  chooseRandomProductsFromDepartment(T,ChosenProducts);
chooseRandomProductsFromDepartment([H|T], Ans) ->
  {atomic, ProductsFromDepartment} =  inventory:getProductsFromDepartment(H),
  ChosenProducts = chooseRandomProductsFromDepartmentInternal(ProductsFromDepartment, []),
  TMP = Ans ++ ChosenProducts,
  chooseRandomProductsFromDepartment(T, TMP).

%% @doc create the shopping list randomly from the list of products
chooseRandomProductsFromDepartmentInternal([], Ans) -> Ans;
chooseRandomProductsFromDepartmentInternal([H|T], Ans) when Ans =:= [] ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
      chooseRandomProductsFromDepartmentInternal(T,[ShoppingListElement]);
    true -> chooseRandomProductsFromDepartmentInternal(T, Ans)
  end;
chooseRandomProductsFromDepartmentInternal([H|T], Ans) ->
  AddToShoppingListRV =  rand:uniform(),
  if
    AddToShoppingListRV< 0.8 -> ShoppingListElement = createShoppingListEle(H),
      chooseRandomProductsFromDepartmentInternal(T, Ans ++ [ShoppingListElement]);
    true -> chooseRandomProductsFromDepartmentInternal(T, Ans)
  end.

%% @doc this function takes a list and shuffles its elements - returns a shuffled list
shuffleList(ShoppingList) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])].

%% @doc returns available purchasable products from all departments according to the shopping list
getProductsFromDepartments(_ShoppingList,[]) -> [];
getProductsFromDepartments(ShoppingList, [H|T]) ->
  [getAllProductsInShoppingListThatBelongToDepartment(ShoppingList, H)]++getProductsFromDepartments(ShoppingList,T).



%% @doc this function should be used by the customer process before shopping at a certain department
%TODO try minimizing it to O(N) instead of O(N^2) by clearing the department elements from the original shopping list
getAllProductsInShoppingListThatBelongToDepartment([H|T], DepartmentName) ->
  ProductDepartmentName = H#shoppinlistelement.department_name,
  case ProductDepartmentName of
    DepartmentName -> getAllProductsInShoppingListThatBelongToDepartment(T, DepartmentName) ++ [H];
    _ -> getAllProductsInShoppingListThatBelongToDepartment(T, DepartmentName)
  end;
getAllProductsInShoppingListThatBelongToDepartment([],_DepartmentName) -> [].

%% @doc as the name states we send a call request to the appropriate department to fetch the products in the shopping list
takeTheProductsFromTheDifferentDepartments(OrderedShoppingList, [H|T], TimeStamp) ->  % TODO change at amir
  LastElementInList =  lists:nth(1,OrderedShoppingList),
  NewList = lists:delete(LastElementInList,OrderedShoppingList),
  RequestIterations = get(requesIteration),
  if
    LastElementInList =:= [] ->
      takeTheProductsFromTheDifferentDepartments(NewList, T,TimeStamp);
    RequestIterations >= ?MAXITERATIONS ->  % To avoid deadlock
      AnsFromServer = department:callFunc(H, {purchaseandleave,LastElementInList, TimeStamp}),
      AnsFromServer ++ takeTheProductsFromTheDifferentDepartments(NewList, T, TimeStamp); % products found
    true ->
       %AnsFromServer = gen_server:call({global,H},{purchase,LastElementInList,TimeStamp}),
       AnsFromServer = department:callFunc(H, {purchase,LastElementInList, TimeStamp}),
       if
         AnsFromServer == noProducts ->
                  put(requesIteration, RequestIterations + 1),
                 % if products were not found in department wait for them to restock,
                  timer:sleep(2500),
                  takeTheProductsFromTheDifferentDepartments(OrderedShoppingList, [H|T], TimeStamp); % try again
         true ->  put(requesIteration, 0),
                  AnsFromServer ++ takeTheProductsFromTheDifferentDepartments(NewList, T, TimeStamp) % products found
       end
  end;

takeTheProductsFromTheDifferentDepartments(_OrderedShoppingList, [], _) -> [].

%% @doc pay to ErlMart through the cashier server
pay(AvailableProductsToPurchase, Balance) ->
  %writeToLogger("i'm paying"),
  %gen_server:cast({global,cashierServer}, {pay, AvailableProductsToPurchase,Balance}).
  cashierServer:castFunc({pay, AvailableProductsToPurchase,Balance}).



%%------------------GETTERS/SETTERS------------------

initBudget() -> (1 - rand:uniform()) * ?MAXIMUM_BUDGET.

getBudget()->
  CostumerInfo = get(customer_info),
  CostumerInfo#customer.budget.


terminate() ->
  masterFunction:castFunc(customerOut).
  %global:send(masterFunction,{customerOut}).





%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment

writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).

%%------------------TEST FUNCTIONS------------------


%%testInit()->
%%  department:start(meat),
%%  department:start(dairy),
%%  department:start(bakery),
%%  Y = 5,
%%  initCustomer(0),
%%  Y.


%%  ShoppingList = [{shoppinlistelement,bakery,"buns",20,4},
%%    {shoppinlistelement,meat,"steak",80,4},
%%    {shoppinlistelement,meat,"chicken",40,4},
%%    {shoppinlistelement,dairy,"yogurt",3,2},
%%    {shoppinlistelement,bakery,"bread",8,5},
%%    {shoppinlistelement,dairy,"cheese",10,9},
%%    {shoppinlistelement,dairy,"milk",5,10}],
%%
%%  X = getProductsFromDepartments(ShoppingList,[meat,dairy,asd,bakery]),
%%  Y = takeTheProductsFromTheDifferentDepartments(X,[meat,dairy,asd,bakery]),
%%  Y.


%%
%%%% @doc for debuggging
%%getList(Department)->
%%  F = fun() ->
%%    Q = qlc:q([E || E <- mnesia:table(Department)]),
%%    qlc:e(Q)
%%      end,
%%  {atomic, ListAns} = mnesia:transaction(F),
%%  ListAns.
