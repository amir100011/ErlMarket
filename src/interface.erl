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
-record(state, {counter, button, start}).


start()->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init([])->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "ErlMarket",[{size, ?SIZE}]),
    Panel = wxPanel:new(Frame),
    Counter = wxTextCtrl:new(Frame, ?COUNTERBOX, [{value, "0"}]),
    Label = wxStaticText:new(Frame, ?wxID_ANY, "Number of Customers:"),
    Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxTextCtrl:setFont(Counter, Font),
    StartButton = wxButton:new(Frame, ?STARTBUTTON, [{label, "Start"}]),
    CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(CounterSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE},
      {border, 5}]),
    wxSizer:add(CounterSizer, Counter, [{proportion,1}, {flag, ?wxEXPAND bor ?
    wxALL}, {border, 5}]),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, StartButton, [{flag, ?wxEXPAND bor ?wxALL}, {border,
      5}]),
    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),
    wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),
    wxButton:connect(StartButton, command_button_clicked),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    {ok, #state{counter = Counter, button = StartButton, start=false}}.


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.
handle_cast(_Msg, State) ->
  {noreply, State}.




handle_info(#wx{obj = Button, event = #wxCommand{type = command_button_clicked}}, #state{counter = Counter, start = false} = State) ->
    masterFunction:start(),
    wxTextCtrl:setEditable(Counter, false),
    wxButton:setLabel(Button, "Stop"),
    erlang:send_after(1000, self(), updateCounter),
    {noreply, State#state{start = true}};

%%  @doc pressing stop closes the shop
handle_info(#wx{obj = _, event = #wxCommand{type = command_button_clicked}}, #state{counter = _, start = true} = State) ->
  masterFunction:castFunc(closeShop),
  {noreply, State#state{start = false}};


handle_info(updateCounter, #state{counter = Counter, start = true} = State) ->
    NumberOfCustomers = masterFunction:callFunc(getNumberOfCustomers),
    wxTextCtrl:setValue(Counter, integer_to_list(NumberOfCustomers)),
    erlang:send_after(1000, self(), updateCounter),  % update every second
    {noreply, State};

handle_info(updateCounter, #state{button = Button, counter = Counter, start = false} = State) ->
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
%%  case NumberOfCustomers of
%%    0 -> wxTextCtrl:setValue(Counter, "0"),
%%         wxTextCtrl:setEditable(Counter, false),
%%         wxButton:setLabel(Button, "Start"),
%%         {noreply, State};
%%    N ->   wxTextCtrl:setValue(Counter, integer_to_list(N)),
%%           erlang:send_after(1000, self(), updateCounter),  % update every second
%%           {noreply, State}
%%  end;


handle_info(#wx{event=#wxClose{}}, State) ->
  io:fwrite("Interface says bye bye ~n"),
  {stop, normal, State}.

terminate(_Reason, _State) ->
  wx:destroy(),
  ok.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

