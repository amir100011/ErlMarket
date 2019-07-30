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
-compile (export_all).


start (NodeName, ModuleName) ->
  writeToLogger ("Watchdog: Starting:", [node(),ModuleName,NodeName] ),
  global:register_name (watchdog, self ()),
  spawn(NodeName,ModuleName,start,[self()]),
  timer:sleep(5000), % 5 seconds
  ServerPID = gen_server:call({global,ModuleName},pid),
  erlang:monitor(process,ServerPID),
  loop (ModuleName).

loop (ModuleName) ->
  receive
    {'DOWN', _MonitorRef, _Type, _Object, normal} ->
      writeToLogger("RECEIVED ",[normal]);
    {'DOWN', _MonitorRef, _Type, _Object, Info} ->
      writeToLogger("INFO", [Info]),
      NodesTMP = nodes(),
      if NodesTMP =:= [] ->
        Nodes = [node()];
        true->
          Nodes =  shuffleList(NodesTMP)
      end,
      spawn(lists:nth(1,Nodes),ModuleName,start,[self()]),
      timer:sleep(5000), % 5 seconds
      ServerPID = gen_server:call({global,ModuleName},pid),
      erlang:monitor(process,ServerPID),
      writeToLogger("raised",[lists:nth(1,Nodes)]),
      loop(ModuleName);
    _MSG ->
      writeToLogger("RECEIVED ~p~n", [_MSG]),loop (ModuleName)
  end.

raise(ServerPID,ModuleName)->
  writeToLogger ("Watchdog: Starting @ ~p.~n", [node() ] ),
  global:unregister_name(watchdog),
  global:register_name(watchdog, self ()),
  erlang:monitor(process,ServerPID),
  loop(ModuleName).


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
  lists:foreach(
    fun(NodeNameAndModuleName) ->
      NodeName = lists:nth(1,NodeNameAndModuleName),
      ModuleName = lists:nth(2,NodeNameAndModuleName),
      Name = lists:nth(3,NodeNameAndModuleName),
      if Name =:= [] ->
        spawn(NodeName,ModuleName,start,[]),
        timer:sleep(500), % 0.5 seconds
        writeToLogger("list", NodeNameAndModuleName),
        ServerPID = gen_server:call({global,ModuleName},pid),
        MonitorRef = erlang:monitor(process,ServerPID),
        T =
          fun() ->
            mnesia:write(nodeList, {MonitorRef,ModuleName},write),
            mnesia:write(nodeList, {ModuleName,MonitorRef},write)
          end,
        mnesia:transaction(T);
        true ->
          spawn(NodeName,ModuleName,start,[Name]),
          writeToLogger("list", NodeNameAndModuleName),
          timer:sleep(500), % 0.5 seconds
          ServerPID = gen_server:call({global,Name},pid),
          MonitorRef = erlang:monitor(process,ServerPID),
          T =
            fun() ->
              mnesia:write(nodeList, {MonitorRef,ModuleName},write),
              mnesia:write(nodeList, {ModuleName,MonitorRef},write)
            end,
          mnesia:transaction(T)
      end
    end,
    ListOfNodesAndModules).


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
