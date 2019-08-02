%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2019 19:03
%%%-------------------------------------------------------------------
-module (server).
-compile (export_all).
-define(LOGGER_FILE_PATH, "../Logger-server.txt").

start (WatchdogPID) ->
  writeToLogger ("Server: Starting up.",[node()]),
  Exists = global:whereis_name(testServer),
  case Exists of
    undefined ->
      writeToLogger("undefined"),
      init(WatchdogPID);
    _defined ->
      writeToLogger("defined"),
      global:unregister_name (testServer),
      init(WatchdogPID)
  end.


init (WatchdogPID) ->
  writeToLogger("init"),
  erlang:monitor(process,WatchdogPID),
  global:register_name (testServer, self()),
  loop().


loop() ->
  writeToLogger("loop"),
  checkWatchdog(),
  loop().


checkWatchdog () ->
  writeToLogger("waitingToMessageFromWatchdog"),
  receive
    {'DOWN', _MonitorRef, _Type, _Object, _Info} ->
      writeToLogger("raised"),
      Nodes = shuffleList(nodes()),
      erlang:monitor(process,spawn(lists:nth(1,Nodes),watchdog,raise,[self()])),
      writeToLogger("going back to loop"),
      loop();
    _MSG -> writeToLogger("RECEIVED ~p~n", [_MSG]),loop ()
  end.


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

%%stop () ->
%%  whereis (server) ! stop.
%%loop (Status, Watchdog) ->
%%  {NewStatus, NewWatchdog} = receive
%%                               stop -> {stop, none};
%%                               kill_dog ->
%%                                 Watchdog ! die,
%%                                 {Status, Watchdog};
%%                               {nodedown, 'two@erlang.enzo'} ->
%%                                 io:format ("Server: Watchdog node has gone down.~n"),
%%                                 {down, Watchdog};
%%                               {'EXIT', Watchdog, noconnection} ->
%%                                 {Status, Watchdog};
%%                               {'EXIT', Watchdog, Reason} ->
%%                                 io:format ("Server: Watchdog has died of ~p.~n", [Reason] ),
%%                                 {Status, spawn_link ('two@erlang.enzo', watchdog, init, [] ) };
%%                               _ -> {Status, Watchdog}
%%                             after 2000 ->
%%      case Status of
%%        down -> checkNode ();
%%        up -> {up, Watchdog}
%%      end
%%                             end,
%%  case NewStatus of
%%    stop -> ok;
%%    _ -> loop (NewStatus, NewWatchdog)
%%  end.
%%
%%checkNode () ->
%%  case lists:any (fun (Node) -> Node =:= 'watchdog@amir-Inspiron-5559' end, nodes () ) of
%%    false ->
%%      io:format ("Server: Watchdog node is still down.~n"),
%%      {down, none};
%%    true ->
%%      io:format ("Server: Watchdog node has come online.~n"),
%%      global:sync (), %% not sure if this is necessary
%%      case global:whereis_name (watchdog) of
%%        undefined ->
%%          io:format ("Watchdog process is dead"),
%%          Watchdog = spawn_link ('two@erlang.enzo', watchdog, init, [] );
%%        Watchdog ->
%%          io:format ("Watchdog process is still alive")
%%      end,
%%      {up, Watchdog}
%%  end.