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
-define(LOGGER_FILE_PATH, "../Logger-Department.txt").
-author("amir").

-record(state, {}).
-include_lib("records.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3, getProductsForDrawingHistogram/1, departmentTerminate/0]).

-export([start/1,callFunc/2, castFunc/2, removedExpiredProducts/2]).

%% @doc create a global gen_server process that deals with the department backend
start(Name) ->
  gen_server:start({global, Name}, ?MODULE, Name, []).


%% @doc the gen_server process preforms this initalization
init(_Args) ->
  put(server_name, _Args), % for future reference to the department mnesia table
  {ok, normal}.

%% @doc interface function for using gen_server call

callFunc(ServerName, Message) ->
  try gen_server:call({global, ServerName}, Message) of
      AnsFromServer -> AnsFromServer
  catch
    exit:Error -> timer:sleep(200),
                   writeToLogger(variable ,"Department ~p is not responding becuase ~p, resending message ~p ~n",[ServerName, Error, Message]),
                   Ans = callFunc(ServerName, Message),
                   Ans
  end.


  %% @doc interface function for using gen_server cast
castFunc(ServerName, Message) ->
  try  gen_server:cast({global, ServerName}, Message) of
    AnsFromServer-> AnsFromServer  % usually no reply just ok or some atom
  catch
    exit:Error -> timer:sleep(200),
      writeToLogger(variable,"Department ~p is not responding becuase ~p, resending message ~p ~n",[ServerName, Error, Message]),
      castFunc(ServerName, Message)
  end.



getProductList(_, [], Ans) -> Ans;
getProductList(ListAns, [H|T], Ans) ->
  ProductName = H#product.product_name,
  Pred = fun(E) -> lists:nth(1, E) == ProductName end,  % checking if product exist
  List = lists:filter(Pred, ListAns),
  if
    List == [] -> NewElem = [ProductName, H#product.price, 0], % no product so we create a new element
                  getProductList(ListAns, T, Ans ++ [NewElem]);
    true ->       getProductList(ListAns, T, Ans)
  end.

%% @doc handle_call is a synchronic kind of call to the server where the sender waits for a reply,
handle_call(getProducts, _From, State) ->
  % get Products that are currently in
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  {reply, ListAns, State};

handle_call({getTotalAmountOfValidProduct, TimeStamp}, _From, State) ->
  % returns the valid Products in the department
  F = fun() ->
    Q = qlc:q([[E#departmentProduct.product_name, E#departmentProduct.price, E#departmentProduct.amount]
      || E <- mnesia:table(get(server_name)), E#departmentProduct.expiry_time >= TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  {atomic, DepartmentProd} = inventory:getProductsFromDepartment(get(server_name)),
  ProductList = getProductList(ListAns, DepartmentProd, ListAns),
  spawn(?MODULE, removedExpiredProducts, [get(server_name), TimeStamp]),
  {reply, ProductList, State};


handle_call({purchase, ListOfProducts, TimeStamp}, _From, State) ->
  % a purchase request has been made, the department removes the products that exist in the inventory
  RemovedProducts = removeProducts(ListOfProducts, [], false, TimeStamp),
  {reply, RemovedProducts, State};

handle_call({purchaseandleave, ListOfProducts, TimeStamp}, _From, State) ->
  % a purchase request has been made, the department removes the products that exist in the inventory
  RemovedProducts = removeProducts(ListOfProducts, [], true, TimeStamp),
  {reply, RemovedProducts, State};

handle_call(pid, _From, State) ->
  Reply= self(),
  {reply, Reply, State}.

%% @doc handle_call is a asynchronic kind of call to the server where the sender doesn't wait for a reply,
handle_cast({return, ListOfProduct}, State) ->
  % a return shipment of products have been made, this function adds the products to the table,
  % or update the amount of existing similar products
  addProducts(ListOfProduct),
  {noreply, State};

handle_cast({restock, ListOfProduct}, {onSale, Discount})  ->
  % a new shipment of products have been made, this function adds the products to the table,
  % or update the amount of existing similar products
  ListOfProductDiscount = createDiscountedProducts(ListOfProduct, Discount),
 % writeToLogger("restock ON SALE ",[?MODULE, ListOfProductDiscount]),
  addProducts(ListOfProductDiscount),
  {noreply, {onSale, Discount}};

handle_cast({restock, ListOfProduct}, State) ->
  % a new shipment of products have been made, this function adds the products to the table,
  % or update the amount of existing similar products
  %writeToLogger("restock ",[?MODULE,ListOfProduct]),
  addProducts(ListOfProduct),
  {noreply, State};

handle_cast({sale,_}, State) when is_tuple(State) -> {noreply, State};
  %  a request to go on sale in the department
handle_cast({sale, Discount}, State)->
  io:fwrite("Erl market is going on sale with ~p precent discount~n",[Discount * 100 ]),
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  executeSale(ListAns, Discount),
  {noreply, {onSale, Discount}};


handle_cast(cancelSale, {onSale, _})->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  cancelSale(ListAns),
  {noreply, normal};
handle_cast(cancelSale, State) when State =:= normal -> {noreply, normal};


handle_cast(getProducts, State) ->
  % get Products that are currently in
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  %writeToLogger(variable, "Department ~p Inventory is:  ~p ~n", [get(server_name) , ListAns]),
  {noreply, State};

handle_cast(terminate, State) ->
  %terminate(0,0),
  {stop, normal, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:fwrite("Department ~p got message ~p",[get(server_name), _Info]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:fwrite("~p says bye bye ~n",[get(server_name)]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @doc change the price value of each product during sale
executeSale([],_) -> done; % TODO change at amir all the funciton
executeSale([H|T], Discount)  when Discount =< 1 ->
  Price = H#departmentProduct.price,
  NewPrice = (1 - Discount) * Price,
  NewPriceInt = round(NewPrice),
  New = H#departmentProduct{price = NewPriceInt},
  Department = H#departmentProduct.department,
  %mnesia:dirty_delete_object(Department, H),
  %mnesia:dirty_write(Department, New),
  F = fun() -> mnesia:delete_object(Department, H , write), mnesia:write(Department, New, write) end,
  mnesia:transaction(F),
  executeSale(T, Discount).


%% @doc return the normal price of each product from the inventory product mnesia table
cancelSale([]) -> done;  % TODO change at amir all the funciton
cancelSale([H|T]) ->
  F = fun() ->
    Q = qlc:q([E#product.price || E <- mnesia:table(product),
               E#product.product_name =:= H#departmentProduct.product_name]),
    qlc:e(Q)
      end,
  {atomic, [NormalPrice]} = mnesia:transaction(F),
  Department = H#departmentProduct.department,
  New = H#departmentProduct{price = NormalPrice},
  F2 = fun() -> mnesia:delete_object(Department, H , write), mnesia:write(Department, New, write) end,
  mnesia:transaction(F2),
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


createDiscountedProducts([], _)-> [];
createDiscountedProducts([H|T], Discount) ->
  Price = H#departmentProduct.price,
  NewPrice = (1 - Discount) * Price,
  NewPriceInt = round(NewPrice),
  DiscountedProduct = H#departmentProduct{price = NewPriceInt},
  [DiscountedProduct] ++ createDiscountedProducts(T, Discount).



%% @doc this function is used when a purchase is made and we need o update the department mnesia table,
%% it is a private function used by removeProducts
updateAmountOrDeleteProduct(Product, RequestedAmount)->
  DepartmentName = Product#departmentProduct.department,
  ProductAmountInDepartment = Product#departmentProduct.amount,
  %mnesia:dirty_delete_object(DepartmentName, Product),
  mnesia:transaction(fun() -> mnesia:delete_object(DepartmentName, Product, write) end),
  RemovedProduct = Product#departmentProduct{amount = RequestedAmount},
  if
    RequestedAmount == ProductAmountInDepartment -> done;
    RequestedAmount < ProductAmountInDepartment ->   New = Product#departmentProduct{amount = ProductAmountInDepartment - RequestedAmount},
                                                     mnesia:transaction(fun()-> mnesia:write(DepartmentName, New, write) end)
  end,
  RemovedProduct.


%% @doc this function is used when a purchase is made and we need to update the department wares
removeProducts([], Ans, _, _) -> Ans;
removeProducts([H|T], Ans, FlagToBuyWhatThereIs, TimeStamp) when FlagToBuyWhatThereIs == false ->
  Product_Name = H#shoppinlistelement.product_name,
  RequestedAmount = H#shoppinlistelement.amount,
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name)), E#departmentProduct.product_name =:= Product_Name,
      E#departmentProduct.amount >= RequestedAmount, E#departmentProduct.expiry_time > TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  if
    ListAns =:= [] -> removeProducts([], noProducts, FlagToBuyWhatThereIs, TimeStamp); % send the customer a delay message to wait for product
    true ->
      ProductChosenRandomlyFromAvailableProducts = getRandomElement(ListAns),
      Product = updateAmountOrDeleteProduct(ProductChosenRandomlyFromAvailableProducts, RequestedAmount),
      removeProducts(T, Ans ++ [Product], FlagToBuyWhatThereIs, TimeStamp)
  end;

removeProducts([H|T], Ans, FlagToBuyWhatThereIs, TimeStamp) when FlagToBuyWhatThereIs == true ->
  Product_Name = H#shoppinlistelement.product_name,
  RequestedAmount = H#shoppinlistelement.amount,
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name)), E#departmentProduct.product_name == Product_Name,
      E#departmentProduct.amount >= RequestedAmount, E#departmentProduct.expiry_time > TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  if
    ListAns =:= [] -> removeProducts(T, Ans, FlagToBuyWhatThereIs, TimeStamp);
    true ->
      ProductChosenRandomlyFromAvailableProducts = getRandomElement(ListAns),
      Product = updateAmountOrDeleteProduct(ProductChosenRandomlyFromAvailableProducts, RequestedAmount),
      removeProducts(T, Ans ++ [Product], FlagToBuyWhatThereIs, TimeStamp)
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
                        mnesia:transaction(fun() -> mnesia:delete_object(get(server_name), Product, write) end);
    true -> UpdateProduct = H
  end,
  mnesia:transaction(fun() -> mnesia:write(get(server_name), UpdateProduct, write)  end),
  addProducts(T).


removedExpiredProducts(DepartmentName, TimeStamp) ->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(DepartmentName), E#departmentProduct.expiry_time < TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ExpiredItemsList} = mnesia:transaction(F),
  DeleteF = fun() ->
                    deleteElements(DepartmentName, ExpiredItemsList)
            end,
  mnesia:transaction(DeleteF).

deleteElements(_, [])-> ok;
deleteElements(DepartmentName, [H|T])->
  mnesia:delete_object(DepartmentName, H, write),
  deleteElements(DepartmentName, T).


departmentTerminate()->
  Pid = callFunc(dairy, pid),
  exit(Pid, "Fuck You").

getProductsForDrawingHistogram(DepartmentName) when DepartmentName == unified->
 Ans = lists:flatmap(fun(E) -> getProductsForDrawingHistogram(E) end, ?DEPARTMENT_LIST),
 Ans;
getProductsForDrawingHistogram(DepartmentName) when is_atom(DepartmentName) ->
  callFunc(DepartmentName, getProducts).
%%------------------WRITING TO LOGGER------------------

%% @doc these functions write to ../LOG.txt file all important actions in purchaseDepartment
writeToLogger(String, IntegerCost, String2, IntegerCurrentBalance) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~w~s~w ~n",[String, IntegerCost, String2, IntegerCurrentBalance]),
  file:close(S).
writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).
writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).
writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).
