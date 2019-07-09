%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 18:21
%%%-------------------------------------------------------------------
-module(department).
-behavior(gen_server).
-author("amir").

-record(state, {}).
-include_lib("records.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([start/1,callFunc/2, castFunc/2]).

%% @doc create a global gen_server process that deals with the department backend
start(Name) ->
  gen_server:start({global, Name}, ?MODULE, Name, []).


%% @doc the gen_server process preforms this initalization
init(_Args) ->
  inventory:initInventory([node()]), % TODO once we have nodes we should initialize the Inventory once for all nodes, so in future design we delete this line
  put(server_name, _Args), % for future reference to the department mnesia table
  {ok, normal}.

%% @doc interface function for using gen_server call
callFunc(ServerName, Message) ->
  gen_server:call({global, ServerName}, Message).

%% @doc interface function for using gen_server cast
castFunc(ServerName, Message) ->
  gen_server:cast({global, ServerName}, Message).

%% @doc handle_call is a synchronic kind of call to the server where the sender waits for a reply,
%% TODO in future work we want to execute each call to the server with a thread to enhance performances and prevent starvation
handle_call(getProducts, _From, State) ->
  % get Products that are currently in
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  {reply, ListAns, State};

handle_call(getTotalAmountOfValidProduct, _From, State) ->
  % returns the valid Products in the department
  TimeStamp = 50, %  TODO get timestamp
  F = fun() ->
    Q = qlc:q([[E#departmentProduct.product_name, E#departmentProduct.price, E#departmentProduct.amount]
      || E <- mnesia:table(get(server_name)), E#departmentProduct.expiry_time >= TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  {reply, ListAns, State};


handle_call({purchase, ListOfProducts}, _From, State) ->
  % a purchase request has been made, the department removes the products that exist in the inventory
  RemovedProducts = removeProducts(ListOfProducts),
  {reply, RemovedProducts, State};


handle_call(_, _, _) ->
  % debugging purposes % TODO delete before submisson
  io:fwrite("why imm here?").

%% @doc handle_call is a asynchronic kind of call to the server where the sender doesn't wait for a reply,
%% TODO in future work we want to execute each cast to the server with a thread to enhance performances and prevent starvation
handle_cast({return, ListOfProduct}, State) ->
  % a return/new shipment of products have been made, this function adds the products to the table,
  % or update the amount of existing similar products
  addProducts(ListOfProduct),
  {noreply, State};

handle_cast({sale,_}, State) when State =:= onSale -> {noreply, State};
  %  a request to go on sale in the department
handle_cast({sale, Discount}, State)->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  executeSale(ListAns, Discount),
  {noreply, onSale};

handle_cast(cancelSale, State) when State =:= normal -> {noreply, State};
% a request to stop sthe sale in the department
handle_cast(cancelSale, State)->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  cancelSale(ListAns),
  {noreply, normal};



handle_cast(terminate, State) ->
  terminate(0,0),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:fwrite("~p says bye bye ~n",[atom_to_list(server_name)]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @doc change the price value of each product during sale
executeSale([],_) -> done;
executeSale([H|T], Discount)  when Discount < 1 ->
  Price = H#departmentProduct.price,
  NewPrice = (1 - Discount) * Price,
  NewPriceInt = round(NewPrice),
  New = H#departmentProduct{price = NewPriceInt},
  Department = H#departmentProduct.department,
  mnesia:dirty_delete_object(Department, H),
  mnesia:dirty_write(Department, New),
  executeSale(T, Discount).


%% @doc return the normal price of each product from the inventory product mnesia table
cancelSale([]) -> done;
cancelSale([H|T]) ->
  F = fun() ->
    Q = qlc:q([E#product.price || E <- mnesia:table(product),
               E#product.product_name =:= H#departmentProduct.product_name]),
    qlc:e(Q)
      end,
  {atomic, [NormalPrice]} = mnesia:transaction(F),
  Department = H#departmentProduct.department,
  New = H#departmentProduct{price = NormalPrice},
  mnesia:dirty_delete_object(Department, H),
  mnesia:dirty_write(Department, New),
  cancelSale(T).

% a helper function to get a random element from a list
getRandomElement(T) when not is_list(T) -> T;
getRandomElement(T) when length(T) == 1 -> hd(T);
getRandomElement([H|T]) ->
  RV = rand:uniform(),
  if
    RV > 0.5 -> H;
    true -> getRandomElement(T)
  end.


%% @doc this function is used when a purchase is made and we need o update the department mnesia table,
%% it is a private function used by removeProducts
updateAmountOrDeleteProduct(Product, RequestedAmount)->
  DepartmentName = Product#departmentProduct.department,
  ProductAmountInDepartment = Product#departmentProduct.amount,
  mnesia:dirty_delete_object(DepartmentName, Product),
  RemovedProduct = Product#departmentProduct{amount = RequestedAmount},
  if
    RequestedAmount =:= ProductAmountInDepartment -> done;
    RequestedAmount < ProductAmountInDepartment ->   New = Product#departmentProduct{amount = ProductAmountInDepartment - RequestedAmount},
      mnesia:dirty_write(DepartmentName, New)
  end,
  RemovedProduct.


%% @doc this function is used when a purchase is made and we need to update the department wares
removeProducts([]) -> [];
removeProducts([H|T]) ->
  Product_Name = H#shoppinlistelement.product_name,
  RequestedAmount = H#shoppinlistelement.amount,
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name)), E#departmentProduct.product_name =:= Product_Name,
      E#departmentProduct.amount >= RequestedAmount ]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  if
    ListAns =:= [] -> removeProducts(T) ;
    true ->
      ProductChosenRandomlyFromAvailableProducts = getRandomElement(ListAns),
      Product = updateAmountOrDeleteProduct(ProductChosenRandomlyFromAvailableProducts, RequestedAmount),
      removeProducts(T) ++ [Product]
  end.


%% @doc this function is used when the casheer return products that the client cannot afford or when the purchase
%% department buys new products for the department.
addProducts([]) -> done;
addProducts([H|T]) ->
  Product_Name = H#departmentProduct.product_name,
  RequestedAmount = H#departmentProduct.amount,
  ExpiryTime = H#departmentProduct.expiry_time,
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name)), E#departmentProduct.product_name =:= Product_Name, E#departmentProduct.expiry_time == ExpiryTime ]),
    qlc:e(Q)
      end,
  {atomic, ProductTmp} = mnesia:transaction(F),
  if
    length(ProductTmp) == 1 ->
                        Product = hd(ProductTmp),
                        CurrentAmount = Product#departmentProduct.amount,
                        UpdateProduct = Product#departmentProduct{amount = CurrentAmount + RequestedAmount},
                        mnesia:dirty_delete_object(get(server_name), Product) ;
    true -> UpdateProduct = H
  end,
  mnesia:dirty_write(get(server_name), UpdateProduct),
  addProducts(T).