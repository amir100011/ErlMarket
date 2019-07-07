%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 17:28
%%%-------------------------------------------------------------------
-module(department2).
-behavior(gen_server).
-author("amir").

%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([start/0, put/2, get/1, delete/1, ls/0]).

% public functions

start() ->
  gen_server:start({local, department}, ?MODULE, [], []).

%% @doc Adds a key-value pair to the database where `Key` is an atom()
%% and `Value` is a term().
put(Key, Value) ->
  gen_server:call(department, {put, Key, Value}).

%% @doc Fetches `Value` for a given `Key` where `Value`
%% is a term() and `Key` is an atom().
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

%% @doc Deletes a key-value pair from the database.
%% `Key` is an atom().
%% Returns the new state of the database or a tuple {error, string()}.
delete(Key) ->
  gen_server:call(?MODULE, {delete, Key}).

%% @doc Returns the current state of the database.
ls() ->
  gen_server:call(?MODULE, ls).

% gen_server callbacks

init(_Args) ->
  {ok, kv_db:new()}.

handle_call({put, Key, Value}, _From, State) ->
  NewState = kv_db:put(Key, Value, State),
  {reply, NewState, NewState};
handle_call({get, Key}, _From, State) ->
  {reply, kv_db:get(Key, State), State};
handle_call({delete, Key}, _From, State) ->
  NewState = kv_db:delete(Key, State),
  {reply, NewState, NewState};
handle_call(ls, _From, State) ->
  {reply, State, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.