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
-export([c/0,compile/0,start/0]).
-include_lib("records.hrl").

%% @doc compile all the modules in each noded
c()->
  lists:foreach(fun(NodeName) ->
  net_kernel:connect_node(NodeName) end, ?NodeList),
  rpc:multicall(?NodeList, compiler, compile, []).

%% @doc compile all the modules
compile()->
  io:fwrite("~p~n", [compile:file(watchdog, debug_info)]),
  io:fwrite("~p~n", [compile:file(inventory, debug_info)]),
  io:fwrite("~p~n", [compile:file(cashierServer, debug_info)]),
  io:fwrite("~p~n", [compile:file(purchaseDepartment, debug_info)]),
  io:fwrite("~p~n", [compile:file(masterFunction, debug_info)]),
  io:fwrite("~p~n", [compile:file(customer, debug_info)]),
  io:fwrite("~p~n", [compile:file(department, debug_info)]),
  io:fwrite("~p~n", [compile:file(interface, debug_info)]).

%% @doc start the interface
start()->
  interface:start().