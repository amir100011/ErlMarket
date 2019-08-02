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
-include_lib("records.hrl").
-compile (export_all).

start(ListOfNodesAndModules) ->
  spawn(?MODULE, startInternal, [ListOfNodesAndModules]).

startInternal (ListOfNodesAndModules) ->
  writeToLogger ("Watchdog: Starting:", node()),
  global:register_name (watchdog, self ()),
  monitorNewProcess (ListOfNodesAndModules),
  loop().

loop () ->
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


raise(ServerPID,ModuleName)->
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

writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).


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
  io:fwrite("getParameters: ~p~n", [NodeNameAndModuleName]),
  {lists:nth(1,NodeNameAndModuleName), lists:nth(2,NodeNameAndModuleName), lists:nth(3,NodeNameAndModuleName)}.


spawnRegularServer(NodeName,ModuleName) ->
  spawn(NodeName,ModuleName,start,[]),
  timer:sleep(500), % 0.5 seconds
  writeToLogger("list", [NodeName,ModuleName]),
  ServerPID = gen_server:call({global,ModuleName},pid),
  erlang:monitor(process,ServerPID).

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

%%  Fun = fun() -> mnesia:all_keys(nodeList) end,
%%  {atomic, Ans} = mnesia:transaction(Fun),
%%  io:fwrite("czxcz ~p~n",[Ans]).


%%
%%  tmp()->
%%DeadOrAlive = lists:nth(1,Args),
%%case  DeadOrAlive of
%%dead ->
%%writeToLogger("handle info Watchdog dead"),
%%Nodes = shuffleNodes(nodes()),
%%MonitorRef =  erlang:monitor(process,spawn(lists:nth(1,Nodes),watchdog,raise,[self(),?MODULE])),
%%T = fun() ->
%%mnesia:write(nodeList, {MonitorRef,watchdog},write)
%%end,
%%mnesia:transaction(T);
%%alive ->
%%writeToLogger("handle info Watchdog alive"),
%%MonitorRef =  erlang:monitor(process,lists:nth(2,Args)),
%%T = fun() ->
%%mnesia:write(nodeList, {MonitorRef,watchdog},write)
%%end,
%%mnesia:transaction(T)
%%end.
