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
-compile(export_all).

compiler()->
  io:fwrite("~p~n", [compile:file(inventory, debug_info)]),
  io:fwrite("~p~n", [compile:file(cashierServer, debug_info)]),
  io:fwrite("~p~n", [compile:file(purchaseDepartment, debug_info)]),
  io:fwrite("~p~n", [compile:file(masterFunction, debug_info)]),
  io:fwrite("~p~n", [compile:file(customer, debug_info)]),
  io:fwrite("~p~n", [compile:file(department, debug_info)]),
  io:fwrite("~p~n", [compile:file(interface, debug_info)]),
  inventory:initInventory([node()]),
  interface:start(self()).

connect() ->
  net_kernel:connect_node('server@amir-Inspiron-5559'),
  net_kernel:connect_node('server2@amir-Inspiron-5559'),
  net_kernel:connect_node('watchdog2@amir-Inspiron-5559'),
  io:fwrite("~p~n", [compile:file(server, debug_info)]),
  io:fwrite("~p~n", [compile:file(watchdog, debug_info)]),
  watchdog:init().

compileTest(ModuleName) ->
  io:fwrite("~p~n", [compile:file(interface, debug_info)]),
  io:fwrite("~p~n", [compile:file(watchdog, debug_info)]),
  io:fwrite("~p~n", [compile:file(inventory, debug_info)]),
  io:fwrite("~p~n", [compile:file(cashierServer, debug_info)]),
  io:fwrite("~p~n", [compile:file(purchaseDepartment, debug_info)]),
  io:fwrite("~p~n", [compile:file(masterFunction, debug_info)]),
  io:fwrite("~p~n", [compile:file(customer, debug_info)]),
  io:fwrite("~p~n", [compile:file(department, debug_info)]),
  watchdog:start(node(),ModuleName).