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

-export([start/1,callFunc/1]).

start(Name) ->
  gen_server:start({local, Name}, ?MODULE, [Name], []).

init(_Args) ->
  TimeStamp = 0,
  inventory:initInventory(self()),
  mnesia:create_table(_Args,[{type, bag}, {attributes, record_info(fields, product_info)}]),
  initializeInventory(_Args, TimeStamp),
  put(server_name, _Args),
  {ok, []}.


callFunc(ServerName) ->
  gen_server:call(ServerName, {getTotalAmountOfValidProduct, ServerName}).

handle_call({getTotalAmountOfValidProduct, ServerName}, _From, State) ->
  % return amount of valid product
  TimeStamp = 100, %  TODO get timestamp
  F = fun() ->
    Q = qlc:q([[E#product_info.product_name, E#product_info.price, E#product_info.amount]
      || E <- mnesia:table(get(server_name)), E#product_info.expiry_time >= TimeStamp]),
    qlc:e(Q)
      end,
  ListAns = mnesia:transaction(F),
  Reply = sumAmount(ListAns, dict:new()),  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {reply, Reply, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





initializeInventory(Department_Name, TimeStamp)->
  {atomic, Products} = inventory:getProductsFromDepartment(Department_Name),
  initializeInventory(Department_Name, Products, TimeStamp).

initializeInventory(_, [], []) -> done;
initializeInventory(Department_Name, [H|T], TimeStamp) ->
  T = fun() ->
    X = #product_info{product_name = H#product.product_name,
      price = H#product.price,
      expiry_time = H#product.expiry_time + TimeStamp,
      amount = 1000
    },
    mnesia:write(Department_Name, X, write)
      end,
  mnesia:transaction(T),
  initializeInventory(Department_Name, T, TimeStamp).

sumAmount([],Dict) -> Dict;
sumAmount([H|T], Dict) ->
  [Name, Price, Amount] = H,
  case dict:is_key(Name, Dict) of
    true -> [CurrentAmount,_] = dict:fetch(Name, Dict),
      Dict2 = dict:store(Name,[CurrentAmount + Amount, Price], Dict);
    false -> Dict2 = dict:store(Name, [Amount, Price], Dict)
  end,
  sumAmount(T, Dict2).

