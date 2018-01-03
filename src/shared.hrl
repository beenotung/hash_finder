% Internal
-record(manager, {
  reporter_pid
  , target_hash
  , current_int = 0
  , step_size = 1
  , worker_pids = sets:new() :: sets:set()
  , ready_worker_pids = sets:new() :: sets:set()
  , remote_master :: {master, atom()}
}).

-record(worker, {
  master_pid
  , worker_pid
  , target_hash
  , current_chars
  , end_chars
  , start_time
  , n
}).

-record(task, {
  master_pid
  , target_hash
  , start_int
  , end_int
}).

-define(INTERVAL, 10 * 1000 * 1000).

-define(debug(X), io:fwrite("[debug] [~p] ~p ~p~n", [?MODULE, self(), X])).
-define(print_expr(X), io:fwrite("[expr] ~p ~~> ~p~n", [??X, X])).
