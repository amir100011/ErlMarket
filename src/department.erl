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
  gen_server:start({global, Name}, ?MODULE, [Name], []).

init(_Args) ->
  inventory:initInventory([node()]),
  put(server_name, _Args),
  {ok, []}.


callFunc(ServerName) ->
  gen_server:call(ServerName, {getTotalAmountOfValidProduct, ServerName}).


handle_call(getTotalAmountOfValidProduct, _From, State) ->
  % return amount of valid productdepartmentProduct
  TimeStamp = 50, %  TODO get timestamp
  F = fun() ->
    Q = qlc:q([[E#departmentProduct.product_name, E#departmentProduct.price, E#departmentProduct.amount]
      || E <- mnesia:table(get(server_name)), E#departmentProduct.expiry_time >= TimeStamp]),
    qlc:e(Q)
      end,
  {atomic,ListAns} = mnesia:transaction(F),
  Reply = sumAmount(ListAns, dict:new()),  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {reply, Reply, State}.


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

