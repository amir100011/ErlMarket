%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 10:43 AM
%%%-------------------------------------------------------------------
-module(department).
-author("dorliv").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include_lib("records.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args:: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Name) ->
  gen_server:start({global, Name}, ?MODULE, [Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------


-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Department_Name) ->
  TimeStamp = 0,
  %inventory:initInventory(self()),
  mnesia:create_table(Department_Name,[{type, bag}, {attributes, record_info(fields, product_info)}]),
  initializeInventory(Department_Name, TimeStamp),
  put(server_name, Department_Name),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(getTotalAmountOfValidProduct, _From, State) ->
  % return amount of valid product
  TimeStamp = 100, %  TODO get timestamp
  F = fun() ->
      Q = qlc:q([[E#product_info.product_name, E#product_info.price, E#product_info.amount]
        || E <- mnesia:table(get(server_name)), E#product_info.expiry_time >= TimeStamp]),
      qlc:e(Q)
        end,
  ListAns = mnesia:transaction(F),
  Reply = sumAmount(ListAns, dict:new()),  % returns a dictionary with key:=product_name and value:=[Amount,Price]
  {Reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



initializeInventory(Department_Name, TimeStamp)->
  {atomic, Products} = inventory:getProductsFromDepartment(Department_Name),
  initializeInventory(Department_Name, Products, TimeStamp).

initializeInventory(_, [], []) -> done;
initializeInventory(Department_Name, [H|T], TimeStamp) ->
  T = fun() ->
    X = #product_info{product_name = H#product.product_name,
                      price = H#product.price,
                      expiry_time = H#product.expiry_time + TimeStamp,
                      amount = 1000
    },
    mnesia:write(Department_Name, X, write)
      end,
  mnesia:transaction(T),
  initializeInventory(Department_Name, T, TimeStamp).

sumAmount([],Dict) -> Dict;
sumAmount([H|T], Dict) ->
  [Name, Price, Amount] = H,
  case dict:is_key(Name, Dict) of
    true -> [CurrentAmount,_] = dict:fetch(Name, Dict),
            Dict2 = dict:store(Name,[CurrentAmount + Amount, Price], Dict);
    false -> Dict2 = dict:store(Name, [Amount, Price], Dict)
  end,
  sumAmount(T, Dict2).







