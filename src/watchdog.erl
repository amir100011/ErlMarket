%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 17:57
%%%-------------------------------------------------------------------
-module (watchdog).
-define(LOGGER_FILE_PATH, "../Logger-watchdog.txt").
-define(TIMER, timerSuperviserProcess).
-define(TERMINATOR, terminator).
-define(SECURITY1, security1).
-define(SECURITY2, security2).
-define(INTERVAL, 2500).
-include_lib("records.hrl").
%-compile (export_all).
-export([start/1,startWatchdog/1, loop/0, chooseInWhichNodeOpenTheFallenProcess/0, raise/1, shuffleList/1, shuffleNodes/1,
        monitorNewProcess/1,getModuleNameAndProcessName/1, getParameters/1, spawnDepartmentServer/3, spawnRegularServer/2,
        writeToMnesia/3, waitForCustomerToLeave/0]).

start (ListOfNodesAndModules) ->
  spawn(?INTERFACE_NODE,?MODULE,startWatchdog,[ListOfNodesAndModules]).

%% @doc starts monitoring the given processes
startWatchdog (ListOfNodesAndModules) ->
  writeToLogger ("Watchdog: Starting:", node()),
  MasterMonitorRef = erlang:monitor(process,global:whereis_name(masterFunction)),%%monitor masterFunction
  watchdog:writeToMnesia(MasterMonitorRef,masterFunction,[]),
  global:register_name (watchdog, self ()),
  interface:castFunc({monitor, self()}),
  monitorNewProcess (ListOfNodesAndModules),
  loop().

%% @doc receive recursive block for detecting failures
loop () ->
  io:fwrite("registerd_names() -->  ~p~n", [global:registered_names()]),
  receive
    {'DOWN', _MonitorRef, _Type, _Object, normal} ->
      writeToLogger("RECEIVED ",[normal]);
    {'DOWN', MonitorRef, _Type, _Object, Info} ->
      writeToLogger("INFO", [Info]),
      {ModuleName,Name} = getModuleNameAndProcessName(MonitorRef),
      ChosenNode = chooseInWhichNodeOpenTheFallenProcess(),
      writeToLogger("raised",[ChosenNode, ModuleName,Name]),
      monitorNewProcess([[ChosenNode,ModuleName,Name]]),
      loop();
    _MSG ->
      writeToLogger("RECEIVED ~p~n", [_MSG]),loop ()
  end.

%% @doc randonly chooses wherte to open the new failed process (except to the watchdog self node).
%% if and only if one node remains open it on this node.
chooseInWhichNodeOpenTheFallenProcess() ->
  NodesTMP = nodes(),
  if NodesTMP =:= [] ->
    node();
    true->
      lists:nth(1,shuffleList(NodesTMP))
  end.

%% @doc when watchdog itself fails the interface module restarts it with this function
raise(ServerPID)->
  writeToLogger ("Watchdog: Starting @ ~p.~n", [node()]),
  global:unregister_name(watchdog),
  global:register_name(watchdog, self ()),
  erlang:monitor(process,ServerPID),
  loop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffleList(ShoppingList) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])].


shuffleNodes([]) -> [node()];
shuffleNodes(NodeList) ->
  shuffleList(NodeList).

%% @doc monitors new single process
monitorNewProcess(ListOfNodesAndModules) ->
  io:fwrite("monitoring ~p~n", [ListOfNodesAndModules]),
  lists:foreach(
    fun(NodeNameAndModuleName) ->
      {NodeName,ModuleName,Name} = getParameters(NodeNameAndModuleName),
      if Name =:= [] ->
        MonitorRef = spawnRegularServer(NodeName,ModuleName);
        true ->
          MonitorRef = spawnDepartmentServer(NodeName,ModuleName,Name)
      end,
      writeToMnesia(MonitorRef, ModuleName, Name)
    end,
    ListOfNodesAndModules).

%% @doc gets the pid of monitored processes from Mnesia using qlc
getModuleNameAndProcessName(MonitorRef) ->
  F = fun() ->
    Q =
      qlc:q([E || E <- mnesia:table(nodeList), E#processesAllocationToNodes.monitorRef =:= MonitorRef]),
    qlc:e(Q)
      end,
  {atomic, [Reply]} = mnesia:transaction(F),
  io:fwrite("Reply = ~p~n",[Reply]),
  ReplyAsList = tuple_to_list(Reply),
  io:fwrite("Reply = ~p~n",[ReplyAsList]),
  {lists:nth(3,ReplyAsList),lists:nth(4,ReplyAsList)}.

getParameters(NodeNameAndModuleName) ->
  {lists:nth(1,NodeNameAndModuleName), lists:nth(2,NodeNameAndModuleName), lists:nth(3,NodeNameAndModuleName)}.

%% @doc spawn unique module anme server
spawnRegularServer(NodeName,ModuleName) ->
  writeToLogger("list", [NodeName,ModuleName]),
  spawn(NodeName,ModuleName,start,[]),
  timer:sleep(500), % 0.5 seconds
  %ServerPID = gen_server:call({global,ModuleName},pid),
  case ModuleName of
    masterFunction -> ServerPID = masterFunction:callFunc(pid);
    purchaseDepartment -> ServerPID = purchaseDepartment:callFunc(pid);
    cashierServer -> ServerPID = cashierServer:callFunc(pid)
  end,
  erlang:monitor(process, ServerPID).

%% @doc starts unique server name but module name is shared
spawnDepartmentServer(NodeName,ModuleName,Name)->
  spawn(NodeName,ModuleName,start,[Name]),
  timer:sleep(500), % 0.5 seconds
  writeToLogger("list", [NodeName,ModuleName,Name]),
  ServerPID = gen_server:call({global,Name},pid),
  erlang:monitor(process,ServerPID).

writeToMnesia(MonitorRef,ModuleName,Name) ->
  T = fun() ->
    X =
      #processesAllocationToNodes{
        monitorRef = MonitorRef,
        moduleName = ModuleName,
        processName = Name
      },
    mnesia:write(nodeList, X, write)
      end,
  mnesia:transaction(T).

%% @doc kills the system on exit
waitForCustomerToLeave()->
  erlang:send_after(?INTERVAL, self(), closeShop), %% define new timer
  NumberOfCustomers = masterFunction:callFunc(getNumberOfCustomers),
  writeToLogger(variable, "Shop is closed: ~p  Customer remain ~n",[NumberOfCustomers]),
  if
    NumberOfCustomers =/= 0 ->
      waitForCustomerToLeave();
    true ->
      writeToLogger("Shop is closed: all customers left~n"),
      global:send(?TIMER, {terminate}),
      exit(normal)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            WRITE TO LOGGER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).

writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).