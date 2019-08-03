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
-compile (export_all).

start (ListOfNodesAndModules) ->
  spawn(?MODULE,startWatchdog,[ListOfNodesAndModules]).

startWatchdog (ListOfNodesAndModules) ->
  writeToLogger ("Watchdog: Starting:", node()),
  global:register_name (watchdog, self ()),
  monitorNewProcess (ListOfNodesAndModules),
  loop().

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

chooseInWhichNodeOpenTheFallenProcess() ->
  NodesTMP = nodes(),
  if NodesTMP =:= [] ->
    node();
    true->
      lists:nth(1,shuffleList(NodesTMP))
  end.


raise(ServerPID,ModuleName)->  % TODO delete Module name from caller and from here
  writeToLogger ("Watchdog: Starting @ ~p.~n", [node()]),
  global:unregister_name(watchdog),
  global:register_name(watchdog, self ()),
  erlang:monitor(process,ServerPID),
  loop().


shuffleList(ShoppingList) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])].


shuffleNodes([]) -> [node()];
shuffleNodes(NodeList) ->
  shuffleList(NodeList).

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


spawnRegularServer(NodeName,ModuleName) ->
  spawn(NodeName,ModuleName,start,[]),
  timer:sleep(500), % 0.5 seconds
  writeToLogger("list", [NodeName,ModuleName]),
  ServerPID = gen_server:call({global,ModuleName},pid),
  erlang:monitor(process, ServerPID).

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