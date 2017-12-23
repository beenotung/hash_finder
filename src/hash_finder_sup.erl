-module(hash_finder_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  wpool_sup:start_link(),
  N_Thread = erlang:system_info(schedulers_online),
  wpool:start_sup_pool(hash_finder, [{workers, N_Thread}]),
  hash_finder:find_test(),
  Procs = [],
  {ok, {{one_for_one, 1, 5}, Procs}}.
