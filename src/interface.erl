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
-define(SIZE,{1028, 1028}).

-behaviour(gen_server).

%% API
-export([start/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1]).
-define(STARTBUTTON, 1).
-define(COUNTERBOX,2).
-define(DRAWBUTTON, 3).
-define(INTERVALBETWEENPLOTS, 2500).  % should be long because doc warn not to over use the python instance to avoid OS take over
-record(state, {counter, button, start, histogramProcess, histogram, histogramButton}).


start()->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init([])->
      Wx = wx:new(),
      Frame = wxFrame:new(Wx, ?wxID_ANY, "ErlMarket",[{size, ?SIZE}]),
      Counter = wxTextCtrl:new(Frame, ?COUNTERBOX, [{value, "0"}]),
      Label = wxStaticText:new(Frame, ?wxID_ANY, "Number of Customers:"),
      Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
      wxTextCtrl:setFont(Counter, Font),
      StartButton = wxButton:new(Frame, ?STARTBUTTON, [{label, "Start"}]),
      DepartmentScrollButton = wxComboBox:new(Frame, ?wxID_ANY, [{choices,["dairy","meat","bakery", "unified"]}]),
      wxComboBox:setValue(DepartmentScrollButton, "dairy"),
      put(department, dairy),
      DrawHistogramOfDepartmentProduct = wxButton:new(Frame, ?DRAWBUTTON, [{label, "Draw Histogram"}]),

      CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
      wxSizer:add(CounterSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE},
        {border, 5}]),
      wxSizer:add(CounterSizer, Counter, [{proportion,1}, {flag, ?wxEXPAND bor ?
      wxALL}, {border, 5}]),
      wxSizer:add(CounterSizer, DrawHistogramOfDepartmentProduct, [{proportion,1}, {flag, ?wxEXPAND bor ?
      wxALL}, {border, 5}]),
      MainSizer = wxBoxSizer:new(?wxVERTICAL),
      wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
      wxSizer:add(MainSizer, StartButton, [{flag, ?wxEXPAND bor ?wxALL}, {border,
        5}]),
      wxSizer:add(MainSizer, DepartmentScrollButton, [{flag, ?wxEXPAND bor ?wxALL}, {border,
      5}]),
      wxWindow:setSizer(Frame, MainSizer),
      wxSizer:setSizeHints(MainSizer, Frame),
      wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),

      wxButton:connect(StartButton, command_button_clicked),
      wxComboBox:connect(DepartmentScrollButton, command_combobox_selected),
      wxButton:connect(DrawHistogramOfDepartmentProduct, command_button_clicked),
      wxFrame:connect(Frame, close_window),
      wxFrame:show(Frame),
      {ok, P} = python:start([{python_path, "/home/dorliv/Desktop/Erlang/histogram"},{python, "python3"}]), % initialize a python instance
      {ok, #state{counter = Counter, button = StartButton, start = false,
        histogramProcess = P, histogram = false, histogramButton = DrawHistogramOfDepartmentProduct}}.


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.
handle_cast(_Msg, State) ->
  {noreply, State}.





handle_info(#wx{obj = DepartmentScrollButton, event = #wxCommand{type = command_combobox_selected}}, State) ->
  Value = wxComboBox:getValue(DepartmentScrollButton),
  put(department, list_to_atom(Value)),
  {noreply, State};


handle_info(#wx{id = ?STARTBUTTON , obj = Button, event = #wxCommand{type = command_button_clicked}}, #state{counter = Counter, start = false} = State) ->
    masterFunction:start(),
    wxTextCtrl:setEditable(Counter, false),
    wxButton:setLabel(Button, "Stop"),
    erlang:send_after(1000, self(), updateCounter),
    {noreply, State#state{start = true}};

%%  @doc pressing stop closes the shop
handle_info(#wx{id = ?STARTBUTTON, event = #wxCommand{type = command_button_clicked}}, #state{start = true} = State) ->
  masterFunction:castFunc(closeShop),
  {noreply, State#state{start = false}};


handle_info(#wx{id = ?DRAWBUTTON ,obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{start = true, histogramProcess = P, histogram = false} = State) ->
  python:call(P, drawHistogram, plotProcessStart, []), % initialize the process that handles the plots
  erlang:send_after(1000, self(), plotHistogram),
  wxButton:setLabel(Button, "Stop Drawing Histogram"),
  {noreply, State#state{histogram = true}};

handle_info(#wx{id = ?DRAWBUTTON ,obj = Button, event = #wxCommand{type = command_button_clicked}},
    #state{histogramProcess = P, histogram = true} = State) ->
  python:call(P, drawHistogram, plotProcessStop, []), % closes plotProcess
  wxButton:setLabel(Button, "Draw Histogram"),
  {noreply, State#state{histogram = false}};

handle_info(#wx{id = ?DRAWBUTTON, event = #wxCommand{type = command_button_clicked}}, #state{ start = false} = State) ->
  io:fwrite("ERROR : Draw Histogram was pressed before system start~n"),
  {noreply, State};




handle_info(updateCounter, #state{counter = Counter, start = true} = State) ->
    try masterFunction:callFunc(getNumberOfCustomers) of
      NumberOfCustomers-> wxTextCtrl:setValue(Counter, integer_to_list(NumberOfCustomers)),
                          erlang:send_after(1000, self(), updateCounter),  % update every second
                          {noreply, State}
    catch
      exit:_ -> erlang:send_after(1000, self(), updateCounter),  % update every second
                {noreply, State}
    end;



handle_info(updateCounter, #state{button = Button, counter = Counter, start = false, histogram = false} = State) ->
  try masterFunction:callFunc(getNumberOfCustomers) of
    N when is_integer(N) -> wxTextCtrl:setValue(Counter, integer_to_list(N)),
      erlang:send_after(1000, self(), updateCounter),  % update every second
      {noreply, State}
  catch
    exit:_ -> wxTextCtrl:setValue(Counter, "0"),
             wxTextCtrl:setEditable(Counter, false),
              wxButton:setLabel(Button, "Start"),
            {noreply, State};
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
                   {noreply, State#state{histogram = false}};
    Throw -> exit(Throw)
  end;



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



handle_info(plotHistogram, #state{histogram = false} = State) -> % stop sending message to self after pressing stop drawing histogram
  {noreply, State};


handle_info(#wx{event=#wxClose{}}, #state{start = true} = State) ->
  masterFunction:castFunc(closeShop),
  io:fwrite("Interface says bye bye ~n"),
  {stop, normal, State#state{histogram = false}};

handle_info(#wx{event=#wxClose{}}, #state{start = false} = State) ->
  io:fwrite("Interface says bye bye ~n"),
  {stop, normal, State}.

terminate(_Reason, #state{histogramProcess = P, histogram = true} = State) ->
  wx:destroy(),
  python:call(P, drawHistogram, plotProcessStop, []), % closes plotProcess
  python:stop(P), % terminating python instance
  ok;

terminate(_Reason, #state{histogramProcess = P, histogram = false} = State) ->
  wx:destroy(),
  python:stop(P), % terminating python instance
  ok.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

