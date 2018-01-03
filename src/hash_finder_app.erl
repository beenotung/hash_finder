-module(hash_finder_app).
-behaviour(application).

-export([start/2, start/0]).
-export([stop/1]).

start() ->
  io:fwrite("[log] start~n"),
  crypto:start(),
  inets:start(),
  ssl:start(),
%%  Res = start([], []),
  Res = hash_finder_manager:start_link(),
  check_auto(),
  io:fwrite("[log] ready~n"),
  Res.

start(_Type, _Args) ->
  Res = hash_finder_sup:start_link(),
  check_auto(),
  Res.

stop(_State) ->
  ok.

check_auto() ->
  case os:getenv("AUTO") of
    "true" ->
      erlang:spawn(fun() -> hash_finder:find_test() end);
    false ->
      ok
  end.
