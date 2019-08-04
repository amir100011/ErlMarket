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
-compile(export_all).
-include_lib("records.hrl").

c()->
  net_kernel:connect_node('amir@132.72.52.249'),
  rpc:multicall(?NodeList, compiler, compile, []).

compile()->
  io:fwrite("~p~n", [compile:file(watchdog, debug_info)]),
  io:fwrite("~p~n", [compile:file(inventory, debug_info)]),
  io:fwrite("~p~n", [compile:file(cashierServer, debug_info)]),
  io:fwrite("~p~n", [compile:file(purchaseDepartment, debug_info)]),
  io:fwrite("~p~n", [compile:file(masterFunction, debug_info)]),
  io:fwrite("~p~n", [compile:file(customer, debug_info)]),
  io:fwrite("~p~n", [compile:file(department, debug_info)]),
  io:fwrite("~p~n", [compile:file(interface, debug_info)]).


start()->

  interface:start().
