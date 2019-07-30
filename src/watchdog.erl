%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 17:57
%%%-------------------------------------------------------------------
-module (watchdog).
-define(MONITORED_NODE, 'server@amir-Inspiron-5559').
-define(LOGGER_FILE_PATH, "../Logger-watchdog.txt").
-compile (export_all).


init (ModuleName) ->
  writeToLogger ("Watchdog: Starting @ ~p.~n", [node () ] ),
  global:register_name (watchdog, self ()),
  spawn(?MONITORED_NODE,ModuleName,start,[self()]),
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

writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).