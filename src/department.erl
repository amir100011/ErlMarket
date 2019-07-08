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

start(Name) ->
  io:fwrite("node is ~p and process is ~p ~n",[node(), self()]),
  gen_server:start({global, Name}, ?MODULE, Name, []).

init(_Args) ->
  inventory:initInventory([node()]),
  io:fwrite("node is ~p and process is ~p ~n",[node(), self()]),
  put(server_name, _Args),
  {ok, normal}.


callFunc(ServerName, Message) ->
  gen_server:call({global, ServerName}, Message).

castFunc(ServerName, Message) ->
  gen_server:cast({global, ServerName}, Message).


handle_call(getProducts, _From, State) ->
  % return amount of valid productdepartmentProduct
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  {reply, ListAns, State};

handle_call(getTotalAmountOfValidProduct, _From, State) ->
  % return amount of valid productdepartmentProduct
  TimeStamp = 50, %  TODO get timestamp
  F = fun() ->
    Q = qlc:q([[E#departmentProduct.product_name, E#departmentProduct.price, E#departmentProduct.amount]
      || E <- mnesia:table(get(server_name)), E#departmentProduct.expiry_time >= TimeStamp]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  Reply = sumAmount(ListAns, dict:new()),  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {reply, Reply, State};

handle_call(_, _, _) ->
  % return amount of valid productdepartmentProduct
  io:fwrite("why imm here?").

handle_cast({sale,_}, State) when State =:= onSale -> {noreply, State};
handle_cast({sale, Discount}, State)->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(get(server_name))]),
    qlc:e(Q)
      end,
  {atomic, ListAns} = mnesia:transaction(F),
  executeSale(ListAns, Discount),
  {noreply, onSale};

handle_cast(cancelSale, State) when State =:= normal -> {noreply, State};
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




sumAmount([],Dict) -> Dict;
sumAmount([H|T], Dict) ->
  [Name, Price, Amount] = H,
  case dict:is_key(Name, Dict) of
    true -> [CurrentAmount,_] = dict:fetch(Name, Dict),
      Dict2 = dict:store(Name,[CurrentAmount + Amount, Price], Dict);
    false -> Dict2 = dict:store(Name, [Amount, Price], Dict)
  end,
  sumAmount(T, Dict2).

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
