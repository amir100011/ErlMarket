%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2019 4:35 PM
%%%-------------------------------------------------------------------
-module(compiler).
-author("dorliv").

%% API
-export([compiler/0]).

compiler()->
  io:fwrite("~p~n", [compile:file(inventory, debug_info)]),
  io:fwrite("~p~n", [compile:file(cashierServer, debug_info)]),
  io:fwrite("~p~n", [compile:file(purchaseDepartment, debug_info)]),
  io:fwrite("~p~n", [compile:file(masterFunction, debug_info)]),
  io:fwrite("~p~n", [compile:file(customer, debug_info)]),
  io:fwrite("~p~n", [compile:file(department, debug_info)]),
  io:fwrite("~p~n", [compile:file(interface, debug_info)]),
  inventory:initInventory(node()),
  interface:start().