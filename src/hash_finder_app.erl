-module(hash_finder_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  case os:getenv("AUTO") of
    "true" ->
      erlang:spawn(fun() -> hash_finder:find_test() end);
    false ->
      ok
  end,
  hash_finder_sup:start_link().

stop(_State) ->
  ok.
