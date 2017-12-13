-module(hash_finder_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([get_child/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
    {hash_finder, {hash_finder, start_link, []},
      permanent, 1000, worker, [hash_finder]}
  ],
  {ok, {{one_for_one, 1, 5}, Procs}}.

%%% ---------------------------------------------------
%%% Internal functions.
%%% ---------------------------------------------------

get_child(Name) ->
  utils:get_child(?MODULE, Name).
