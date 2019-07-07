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
-export([initInventory/1, get_products_from_department/1, getDepartments/0, test_mnesia/0]).
-define(Filename, "Inventory.txt").
-record(product, {department, product_name, price, expiry_time}).
-record(department, {department_name, department_pid}).


getDepartments()->
  mnesia:dirty_all_keys(product).

initInventory(NodeList)->
  mnesia:create_schema(NodeList),
  mnesia:start(),
  mnesia:create_table(product,[{type, bag} ,{attributes, record_info(fields, product)}]),
  mnesia:create_table(department,[{attributes, record_info(fields, department)}]),
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
        X = #product{department = Department,
                     product_name = Product,
                     price = Price,
                     expiry_time = Time_To_Be_Expired

        },
        mnesia:write(X)
      end,
  mnesia:transaction(T),
  read_lines(File).

process_line(eof) -> eof;
process_line({error,_}) -> io:fwrite("Error: reading line failed");
process_line({ok,Data}) -> string:tokens(Data, "\n").

get_products_from_department(Department)->
  R = fun()->
    mnesia:read(product, Department)
      end,
  mnesia:transaction(R).


test_mnesia()->
  Node = node(),
  initInventory([Node]),
  R = fun()->
        mnesia:read(product, "dairy")
      end,
  Ans = mnesia:transaction(R),
  io:fwrite("Ans ~p ~n", [Ans]).

