%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 17:35
%%%-------------------------------------------------------------------
-module(kv_db).

-type db() :: [].
-type results() :: nonempty_list({Key::atom(), Value::term()}).
-type err() :: {'error', string()}.

-export_type([db/0, results/0, err/0]).
-export([new/0, put/3, get/2, delete/2, ls/1]).

-spec new() -> db().
new() -> [].

-spec put(Key::atom(), Value::term(), Db::db()) -> results().
put(Key, Value, []) when is_atom(Key) ->
  [{Key, Value}];
put(Key, Value, [{Key, _} | Db]) when is_atom(Key) ->
  [{Key, Value} | Db];
put(Key, Value, [Current | Db]) when is_atom(Key) ->
  [Current | put(Key, Value, Db)].

-spec get(Key::atom(), Db::db()) -> term() | err().
get(Key, []) when is_atom(Key) ->
  {error, "Key not found: " ++ atom_to_list(Key)};
get(Key, [{Key, Value} | _]) when is_atom(Key) ->
  Value;
get(Key, [_ | Db]) when is_atom(Key) ->
  get(Key, Db).

-spec delete(Key::atom(), Db::db()) -> (results() | nil()) | err().
delete(Key, []) ->
  {error, "Key not found: " ++ atom_to_list(Key)};
delete(Key, [{Key, _Value} | Db]) ->
  Db;
delete(Key, [Tuple | Db]) ->
  [Tuple | delete(Key, Db)].

-spec ls(db()) -> [{atom(), term()},...] | nil().
ls(Db) ->
  Db.
