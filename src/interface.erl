%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 11:00 AM
%%%-------------------------------------------------------------------
-module(interface).
-author("dorliv").
-include_lib("wx/include/wx.hrl").
-include_lib("records.hrl").
-define(SIZE,{1028, 1028}).
-define(LOGGER_FILE_PATH, "../Logger_interface.txt").
-behaviour(gen_server).
-record(state, {counter, button, start, histogramProcess, histogram, histogramButton, sale, finished, timecnt}).
%% API
-export([start/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1, castFunc/1, callFunc/1]).
-define(HISTOGRAM_PATH, "src/").
-define(STARTBUTTON, 1).
-define(COUNTERBOX,2).
-define(DRAWBUTTON, 3).
-define(SALE,4).
-define(INTERVALBETWEENPLOTS, 2000).  % should be long because doc warn not to over use the python instance to avoid OS take over

%-record(state, {counter, button, start, histogramProcess, histogram, histogramButton}).

%% @doc start the interface module
start()->
  writeToLogger ("Server: Starting up.",[node()]),
  gen_server:start({global, ?MODULE}, ?MODULE, [] , []).  %FIXME delete after addition of continuation from previous state implementation


%% @doc initialize the interface
init([])->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, ?wxID_ANY, "ErlMarket",[{size, ?SIZE}]),
  Counter = wxTextCtrl:new(Frame, ?COUNTERBOX, [{value, "0"}]),
  Label = wxStaticText:new(Frame, ?wxID_ANY, "Number of Customers:"),
  Time = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, "0"}]),
  LabelTime = wxStaticText:new(Frame, ?wxID_ANY, "Time steps:"),
  Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(Counter, Font),
  StartButton = wxButton:new(Frame, ?STARTBUTTON, [{label, "Start"}]),
  DepartmentScrollButton = wxComboBox:new(Frame, ?wxID_ANY, [{choices,["dairy","meat","bakery", "unified"]}]),
  Sale =  wxButton:new(Frame, ?SALE, [{label, "Go on Sale"}]),
  wxComboBox:setValue(DepartmentScrollButton, "dairy"),
  put(department, dairy),
  DrawHistogramOfDepartmentProduct = wxButton:new(Frame, ?DRAWBUTTON, [{label, "Draw Histogram"}]),
  CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(CounterSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE},
    {border, 5}]),
  wxSizer:add(CounterSizer, Counter, [{proportion,1}, {flag, ?wxEXPAND bor ?
  wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer, LabelTime, [{flag, ?wxALL bor ?wxALIGN_CENTRE},
    {border, 5}]),
  wxSizer:add(CounterSizer, Time, [{proportion,1}, {flag, ?wxEXPAND bor ?
  wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer, DrawHistogramOfDepartmentProduct, [{proportion,1}, {flag, ?wxEXPAND bor ?
  wxALL}, {border, 5}]),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, StartButton, [{flag, ?wxEXPAND bor ?wxALL}, {border,
    5}]),
  wxSizer:add(MainSizer, DepartmentScrollButton, [{flag, ?wxEXPAND bor ?wxALL}, {border,
    5}]),
  wxSizer:add(MainSizer, Sale, [{flag, ?wxEXPAND bor ?wxALL}, {border,
    5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer, Frame),
  wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),
  wxButton:connect(Sale, command_button_clicked),
  wxButton:connect(StartButton, command_button_clicked),
  wxComboBox:connect(DepartmentScrollButton, command_combobox_selected),
  wxButton:connect(DrawHistogramOfDepartmentProduct, command_button_clicked),
  wxFrame:connect(Frame, close_window),
  wxFrame:show(Frame),
  {ok, P} = python:start([{python_path, ?HISTOGRAM_PATH},{python, "python3"}]), % initialize a python instance % TODO link to this process and recreate it if it falls and delete it if interface falls
  python:call(P, drawHistogram, plotProcessStart, []), % initialize the process that handles the plots
  State = #state{counter = Counter, button = StartButton, start = false,
              histogramProcess = P, histogram = false, histogramButton = DrawHistogramOfDepartmentProduct, sale = false,
              finished = true, timecnt = Time}, % initializing the state
  {ok, State}.

%% @doc the interface will monitor the PID
handle_call({monitor, PID}, _From, State) ->
  erlang:monitor(process, PID),
  {reply, ok, State};

handle_call(pid, _From, State) ->
  Reply = self(),
  {reply, Reply, State};

%% @doc finished means that the store is close and that there are no customers in the market
handle_call(isFinished, _From,  #state{ finished = Finished } = State) ->
  {reply, Finished, State}.

%% @doc sends data to the tensorboard buffer via python
handle_cast({budgetVsExpense, Budget, Expense}, #state{ histogramProcess = P} = State) ->
  writeToLogger("handleCast budgetVsExpense: ", [Budget, Expense]),
  python:call(P, drawHistogram, processBudgetVsExpenceFromErlang, [Budget, Expense]), % initialize the process that handles the plots
  {noreply, State};

%% @doc updates the time in the interface
handle_cast({updateTime, CurrTime}, #state{ timecnt = TimeCNT} = State) ->
  wxTextCtrl:setValue(TimeCNT, integer_to_list(CurrTime)),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.



%% @doc the interface is supposed to only monitor the watchdog process so this is why we don't care about the monitor ref
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info},State) ->
  writeToLogger("handle info raised because watcdog died"),
  Nodes = shuffleNodes(nodes()),
  erlang:monitor(process,spawn(lists:nth(1,Nodes), watchdog,raise,[self()])),
  {noreply, State};


%% @doc Pressing the Sale button initiates this method
handle_info(#wx{id = ?SALE , event = #wxCommand{type = command_button_clicked}},
    #state{start = false} = State) ->
  io:fwrite("ERROR : Go on Sale was pressed before system start~n"),
  {noreply, State};
handle_info(#wx{id = ?SALE , obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{start = true, sale = false} = State) ->
    lists:foreach(fun(E) -> department:castFunc(E, {sale, 0.5}) end, ?DEPARTMENT_LIST),
    wxButton:setLabel(Button, "Stop Sale"),
  {noreply, State#state{sale = true}};
handle_info(#wx{id = ?SALE , obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{start = true, sale = true} = State) ->
  lists:foreach(fun(E) -> department:castFunc(E, cancelSale) end, ?DEPARTMENT_LIST),
  wxButton:setLabel(Button, "Go on Sale"),
  {noreply, State#state{sale = false}};

%% @doc using the scroll box initiates this method
handle_info(#wx{obj = DepartmentScrollButton, event = #wxCommand{type = command_combobox_selected}}, State) ->
  Value = wxComboBox:getValue(DepartmentScrollButton),
  put(department, list_to_atom(Value)),
  {noreply, State};

%% @doc using the start/stop button initiates this method
handle_info(#wx{id = ?STARTBUTTON, event = #wxCommand{type = command_button_clicked}},
    #state{start = false, finished = false} = State) ->
  io:fwrite("Please wait for system to shut down~n"), % cannot start the Market before it is finished meaning while customers are still inside
  {noreply, State};
handle_info(#wx{id = ?STARTBUTTON , obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{counter = Counter, start = false, finished = true} = State) ->
  inventory:initInventory(),
  spawn(?MASTER_SERVER_NODE, masterFunction, start, []),  % start the Market
  wxTextCtrl:setEditable(Counter, false),
  wxButton:setLabel(Button, "Stop"),
  erlang:send_after(1000, self(), updateCounter),
  {noreply, State#state{start = true, finished = false}};
%%  @doc pressing stop closes the shop
handle_info(#wx{id = ?STARTBUTTON, event = #wxCommand{type = command_button_clicked}},
    #state{start = true} = State) ->
  masterFunction:castFunc(closeShop),  % closing the Market, stop from customers from entering the market
  {noreply, State#state{start = false}};

%% @doc pressing the draw histogram initializes this method
handle_info(#wx{id = ?DRAWBUTTON ,obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{start = true, histogram = false} = State) ->
  erlang:send_after(1000, self(), plotHistogram),
  wxButton:setLabel(Button, "Stop Drawing Histogram"),
  %writeToMnesia(true, true),
  {noreply, State#state{histogram = true}};
handle_info(#wx{id = ?DRAWBUTTON ,obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{histogram = true} = State) ->
  wxButton:setLabel(Button, "Draw Histogram"),
  %writeToMnesia(false, Start),
  {noreply, State#state{histogram = false}};
handle_info(#wx{id = ?DRAWBUTTON, event = #wxCommand{type = command_button_clicked}}, #state{ start = false} = State) ->
  io:fwrite("ERROR : Draw Histogram was pressed before system start~n"),
  {noreply, State};



%% @doc keeps a tab on the number of customers in the shop
handle_info(updateCounter, #state{counter = Counter, start = true} = State) ->
  try masterFunction:callFunc(getNumberOfCustomers) of
    NumberOfCustomers-> wxTextCtrl:setValue(Counter, integer_to_list(NumberOfCustomers)),
      erlang:send_after(1000, self(), updateCounter),  % update every second
      {noreply, State}
  catch
    exit:_ -> erlang:send_after(1000, self(), updateCounter),  % update every second
      {noreply, State}
  end;
handle_info(updateCounter, #state{button = Button, counter = Counter, start = false,
  histogram = false, timecnt = TimeButton} = State) ->
  try masterFunction:callFunc(getNumberOfCustomers) of
    N when is_integer(N) -> wxTextCtrl:setValue(Counter, integer_to_list(N)),
      erlang:send_after(1000, self(), updateCounter),  % update every second
      {noreply, State}
  catch
    exit:_ -> wxTextCtrl:setValue(Counter, "0"),
      wxTextCtrl:setValue(TimeButton, "0"),
      wxTextCtrl:setEditable(Counter, false),
      wxButton:setLabel(Button, "Start"),
      {noreply, State#state{finished = true}};
      %rpc:multicall(?NodeList, application, stop, [mnesia]);
    Throw -> exit(Throw)
  end;
handle_info(updateCounter, #state{button = Button, counter = Counter, start = false,
  histogramButton = HistogramButton, histogramProcess = P , histogram = true} = State) ->
  try masterFunction:callFunc(getNumberOfCustomers) of
    N when is_integer(N) -> wxTextCtrl:setValue(Counter, integer_to_list(N)),
      erlang:send_after(1000, self(), updateCounter),  % update every second
      {noreply, State}
  catch
    exit:_ -> wxTextCtrl:setValue(Counter, "0"),
      wxTextCtrl:setEditable(Counter, false),
      python:call(P, drawHistogram, plotProcessStop, []), % closes plotProcess
      wxButton:setLabel(Button, "Start"),
      wxButton:setLabel(HistogramButton, "Draw Histogram"),
      %writeToMnesia(false, false),
      {noreply, State#state{histogram = false, finished = true}};
      %rpc:multicall(?NodeList, application, stop, [mnesia]);
    Throw -> exit(Throw)
  end;


%% @doc plot the histogram by sending data to matplotlib via python
handle_info(plotHistogram, #state{histogram = true , histogramProcess = P} = State) ->
  DepartmentName = get(department),
  try department:getProductsForDrawingHistogram(DepartmentName) of  % if the department gets turned off before the histogram status was changed
    ProductFromDepartment ->  python:call(P, drawHistogram, processDepartmentDataFromErlang, [atom_to_list(DepartmentName), ProductFromDepartment]),
      erlang:send_after(?INTERVALBETWEENPLOTS, self(), plotHistogram),  % update plot every interval
      {noreply, State}
  catch
    exit:_ ->
      {noreply, State}
    %  we do not change the state in this case
    % this is intentional because we want to get to update Counter where we close the drawHistogram in an orderly fashion
  end;
handle_info(plotHistogram, #state{histogram = false, histogramProcess = P} = State) -> % stop sending message to self after pressing stop drawing histogram
  python:call(P, drawHistogram, processStopHistogram, []),
  {noreply, State};

%% @doc closing the interface initiates this method
handle_info(#wx{event=#wxClose{}}, #state{start = true} = State) ->
  masterFunction:castFunc(closeShop),
  io:fwrite("Interface says bye bye ~n"),
  {stop, normal, State#state{histogram = false}};
handle_info(#wx{event=#wxClose{}}, #state{start = false} = State) ->
  io:fwrite("Interface says bye bye ~n"),
  {stop, normal, State};

handle_info(MSG, State)->
  io:fwrite("interface gets ~p~n",[MSG]),
  {noreply, State}.


terminate(_Reason, #state{histogramProcess = P} = _) ->
  wx:destroy(),
  python:call(P, drawHistogram, plotProcessStop, []), % closes plotProcess
  timer:sleep(2500),
  python:stop(P), % terminating python instance
  ok.

%% @doc interface function for using gen_server cast
castFunc(Message) ->
  gen_server:cast({global, ?MODULE}, Message).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc interface function for using gen_server cast
callFunc(Message) ->
  gen_server:cast({global, ?MODULE}, Message).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           HELPER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffleList(ShoppingList) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- ShoppingList])].


shuffleNodes([]) -> [node()];
shuffleNodes(NodeList) ->
  shuffleList(NodeList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       WRITE TO LOGGER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


writeToLogger(String) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s ~n",[String]),
  file:close(S).

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).
