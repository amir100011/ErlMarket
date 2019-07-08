%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2019 2:49 PM
%%%-------------------------------------------------------------------
-module(test_logic).
-author("dorliv").
-include_lib("records.hrl").
%% API
-export([init/1, handle_call/4, testLogic/0]).

executeSale([],_) -> done;
executeSale([H|T], Discount)  when Discount < 1 ->
  Price = H#departmentProduct.price,
  NewPrice = (1 - Discount) * Price,
  NewPriceInt = round(NewPrice),
  New = H#departmentProduct{price = NewPriceInt},
  Department = H#departmentProduct.department,
  mnesia:delete_object(Department, H, write),
  mnesia:write(Department, New, write),
  executeSale(T, Discount).


init(_Args) ->
  TimeStamp = 0,

  inventory:initInventory([node()]),
  put(server_name, _Args),
  {ok, []}.



handle_call(getTotalAmountOfValidProduct, ServerName, _From, State) ->
  % return amount of valid productdepartmentProduct
  TimeStamp = 50, %  TODO get timestamp
  F = fun() ->
    Q = qlc:q([[E#departmentProduct.product_name, E#departmentProduct.price, E#departmentProduct.amount]
      || E <- mnesia:table(get(server_name)), E#departmentProduct.expiry_time =< TimeStamp]),
    qlc:e(Q)
      end,
  {atomic,ListAns} = mnesia:transaction(F),
  Reply = sumAmount(ListAns, dict:new()),  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {reply, Reply, State}.





sumAmount([],Dict) -> Dict;
sumAmount([H|T], Dict) ->
  [Name, Price, Amount] = H,
  case dict:is_key(Name, Dict) of
    true -> [CurrentAmount,_] = dict:fetch(Name, Dict),
      Dict2 = dict:store(Name,[CurrentAmount + Amount, Price], Dict);
    false -> Dict2 = dict:store(Name, [Amount, Price], Dict)
  end,
  sumAmount(T, Dict2).


testLogic()->
  init(dairy),
  {Atom, Reply, State }= handle_call(getTotalAmountOfValidProduct, dairy, 10, 10),
  io:fwrite("~p ~n",[Reply]).



