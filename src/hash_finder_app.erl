-module(hash_finder_app).
-behaviour(application).

-export([start/2, start/0]).
-export([stop/1]).

start() ->
  io:fwrite("[log] start~n"),
  crypto:start(),
  inets:start(),
  ssl:start(),
%%  start([], []),
  hash_finder_manager:start_link(),
  io:fwrite("[log] ready~n").

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
