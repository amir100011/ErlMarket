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
-export([initInventory/1, getProductsFromDepartment/1, getDepartments/0, test_mnesia/0]).
-define(Filename, "Inventory.txt").
-include_lib("records.hrl").


getDepartments()->
  mnesia:dirty_all_keys(product).

initInventory(NodeList)->
  mnesia:create_schema(NodeList),
  mnesia:start(),
  mnesia:create_table(product,[{type, bag} ,{attributes, record_info(fields, product)}]),
  mnesia:create_table(department,[{attributes, record_info(fields, department)}]),
  mnesia:create_table(dairy,[
    {type,bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)}]
  ),
  mnesia:create_table(meat,[
    {type,bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)}]
  ),
  mnesia:create_table(bakery,[
    {type, bag},
    {record_name, departmentProduct},
    {attributes,record_info(fields,departmentProduct)}]
  ),
  mnesia:info(),
  fillInventory().



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

getProductsFromDepartment(Department)->
  R = fun()->
    mnesia:read(product, Department)
      end,
  mnesia:transaction(R).


test_mnesia()->
  Node = node(),
  initInventory([Node]),
  R = fun()->
        mnesia:read(product, dairy)
      end,
  Ans = mnesia:transaction(R),
  io:fwrite("Ans ~p ~n", [Ans]),
  List = getProductsFromDepartment(dairy),
  io:fwrite("Ans ~p ~n", [List]).

