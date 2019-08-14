%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2019 5:23 PM
%%%-------------------------------------------------------------------
-module(inventory).
-author("dorliv").
%% API
-export([initInventory/0, getProductsFromDepartment/1, getDepartments/0, fillInventory/0, getProdcutPrice/1]).
-define(Filename, "Inventory.txt").
-define(LOGGER_FILE_PATH, "../Logger-Inventory.txt").

-include_lib("records.hrl").


getDepartments()->
  Fun = fun() -> mnesia:all_keys(product) end,
  {atomic, Ans} = mnesia:transaction(Fun),
  Ans.


%% @doc initialize the inventory for all the nodes in NodeList
%% this function creates the initial tables and fills them with initial value
%%
%%startMnesia()->
%%  io:fwrite("node ~p start mneisa ~n",[node()]),
%%  mnesia:start().

%% @doc initialize the inventory of our Market with initial values
initInventory()->
  Ans = mnesia:create_schema(?NodeList),
  io:fwrite("creating schame returns ~p ~n",[Ans]),
  rpc:multicall(?NodeList, application, start, [mnesia]),
  mnesia:create_table(product,[{type, bag} ,{disc_copies, ?NodeList}, {attributes, record_info(fields, product)}]),
  mnesia:create_table(department,[{attributes, record_info(fields, department)}, {disc_copies, ?NodeList}]),
  mnesia:create_table(dairy,[
    {type,bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)},
    {disc_copies, ?NodeList}]
  ),
  mnesia:create_table(meat,[
    {type,bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)},
    {disc_copies, ?NodeList}]
  ),
  mnesia:create_table(bakery,[
    {type, bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)},
    {disc_copies, ?NodeList}]
 ),
  mnesia:create_table(nodeList,[
    {type, set},
    {record_name, processesAllocationToNodes},
    {attributes,record_info(fields,processesAllocationToNodes)},
    {disc_copies, ?NodeList}]
  ),
  fillInventory().


%% @doc reads from inventory.txt the different kinds of products we have in our ErlMarket
fillInventory()->
  {Ack, File} = file:open(?Filename,[read]),
  if
    Ack =:= error -> io:fwrite("Error: reading file failed");
    true ->
      read_lines(File)
  end.





read_lines(File) ->
  Line = process_line(file:read_line(File)),
  read_line(Line, File).

read_line(eof, _) -> done;
read_line(Line, File) ->
  [Department, Product, Price, Time_To_Be_Expired] = string:split(Line, " ", all),

  T = fun() ->
        X = #product{department = list_to_atom(Department),
                     product_name = Product,
                     price = list_to_integer(Price),
                     expiry_time = list_to_integer(Time_To_Be_Expired)

        },
        mnesia:write(X)
      end,
  mnesia:transaction(T),
  T2 = fun() ->
    X2 = #departmentProduct{department = list_to_atom(Department),
                            product_name = Product,
                            price = list_to_integer(Price),
                            expiry_time = list_to_integer(Time_To_Be_Expired),
                            amount = 1000
    },
    mnesia:write(list_to_atom(Department), X2, write)
      end,
  mnesia:transaction(T2),
  read_lines(File).

process_line(eof) -> eof;
process_line({error,_}) -> io:fwrite("Error: reading line failed");
process_line({ok,Data}) -> string:tokens(Data, "\n").

%% @doc get the different types of products in Department
getProductsFromDepartment(Department)->
  R = fun()->
    mnesia:read(product, Department)
      end,
  mnesia:transaction(R).

getProdcutPrice(ProductName) ->
  F = fun() ->
      Q = qlc:q([E#product.price || E <- mnesia:table(product), E#product.product_name == ProductName]),
      qlc:e(Q)
      end,
  {atomic, [Price]} = mnesia:transaction(F),
  Price.

%%
%%test()->
%%  initInventory([node()]),
%%  PriceOfProduct = getProdcutPrice("buns"),
%%  io:fwrite("~p~n",[PriceOfProduct]).

%%getDepartmentProductsFromDepartment(Department)->
%%  % get Products that are currently in
%%  F = fun() ->
%%    Q = qlc:q([E || E <- mnesia:table(Department)]),
%%    qlc:e(Q)
%%      end,
%%  {atomic, ListAns} = mnesia:transaction(F),
%%   ListAns.
%%
%%
%%
%%%% @doc change the price value of each product during sale
%%executeSale([],_) -> done;
%%executeSale([H|T], Discount)  when Discount < 1 ->
%%  Price = H#departmentProduct.price,
%%  NewPrice = (1 - Discount) * Price,
%%  NewPriceInt = round(NewPrice),
%%  New = H#departmentProduct{price = NewPriceInt},
%%  Department = H#departmentProduct.department,
%%  %mnesia:dirty_delete_object(Department, H),
%%  %mnesia:dirty_write(Department, New),
%%  F = fun() -> mnesia:delete_object(Department, H , write), mnesia:write(Department, New, write) end,
%%  mnesia:transaction(F),
%%  executeSale(T, Discount).
%%
%%
%%%% @doc return the normal price of each product from the inventory product mnesia table
%%cancelSale([]) -> done;
%%cancelSale([H|T]) ->
%%  F = fun() ->
%%    Q = qlc:q([E#product.price || E <- mnesia:table(product),
%%      E#product.product_name =:= H#departmentProduct.product_name]),
%%    qlc:e(Q)
%%      end,
%%  {atomic, [NormalPrice]} = mnesia:transaction(F),
%%  Department = H#departmentProduct.department,
%%  New = H#departmentProduct{price = NormalPrice},
%%  F2= fun() -> mnesia:delete_object(Department, H , write), mnesia:write(Department, New, write) end,
%%  mnesia:transaction(F2),
%%  cancelSale(T).
%%
%%
%%test() ->
%%  initInventory([node()]),
%%  ListProducts = getDepartmentProductsFromDepartment(dairy),
%%  io:fwrite("Products : ~p ~n",[ListProducts]),
%%  executeSale(ListProducts, 0.5),
%%  ListProducts2 = getDepartmentProductsFromDepartment(dairy),
%%  io:fwrite("Products : ~p ~n",[ListProducts2]),
%%  cancelSale(ListProducts2),
%%  ListProducts3 = getDepartmentProductsFromDepartment(dairy),
%%  io:fwrite("Products : ~p ~n",[ListProducts3]).

