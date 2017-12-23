%%%
%%% Find the original message for a given hash
%%% target spec: not longer than 26 ascii
%%%
-module(hash_finder).

-include_lib("eunit/include/eunit.hrl").

% API
-export([find/1]).
% Debug
-export([debug/0, debug2/0]).

% Callback
-export([worker_loop/1]).

% Internal
-record(master, {
  reporter_pid
  , target_hash
  , current_int = 0
  , step_size = 1
}).

-record(worker, {
  master_pid
  , target_hash
  , current_chars
  , end_chars
  , start_time
}).

-define(ZERO, 32).
-define(MAX, 126).
-define(N_VALID_CHAR, (?MAX - ?ZERO + 1)).
-define(int_to_char(X), (X + ?ZERO)).

-define(INTERVAL, 200).

%%% --------------------------------------------------
%%% API Functions
%%% --------------------------------------------------
find(Hash) when is_list(Hash) ->
  Bin = hex:str_to_bin(Hash),
  find(Bin);
find(Hash) when is_binary(Hash) ->
  Reporter = self(),
  spawn(
    fun() ->
      find_route(Hash, Reporter)
    end),
  receive
    X -> X
  end.

%%% --------------------------------------------------
%%% Test Functions
%%% --------------------------------------------------
find_test() ->
  io:fwrite("testing ~p.~n", ["32"]),
  "32" = find("6364d3f0f495b6ab9dcf8d3b5c6e0b01"),
  io:fwrite("passed test on ~p.~n", ["32"]),
  Msg = find("424d640bf87ff260f9b263503fc78990"),
  io:fwrite("Found Res = ~p.~n", [Msg]),
  ok.

%%% --------------------------------------------------
%%% Internal Functions
%%% --------------------------------------------------
find_route(Target, Reporter) when is_binary(Target), is_pid(Reporter) ->
  io:fwrite("[debug] find_route(~p,~p)~n", [Target, Reporter]),
  master_loop(#master{
    reporter_pid = Reporter
    , target_hash = Target
  }).

master_loop(State = #master{step_size = Step, current_int = Start, target_hash = Target}) ->
  Master = self(),
  Size = proplists:get_value(size, wpool:stats(?MODULE)),
  F = fun(C_Int) ->
    start_worker(Master, Target, C_Int, C_Int + Step)
      end,
  End = Start - 1 + Size * Step,
  control:foreach(F, Start, Step, End),
  receive
    {found, Msg} ->
      io:fwrite("[log] Found, Msg = ~p.~n", [Msg]),
      wpool:stop(?MODULE),
      State#master.reporter_pid ! Msg;
    {not_found, Diff} ->
      % Calc new Step
      io:fwrite("Last task used ~p microseconds~n", [Diff]),
%%      io:fwrite("[debug] ~p/~p*~p~n", [?INTERVAL, Diff, Step]),
      New_Step = floor(?INTERVAL / (Diff + 1) * Step + 1),
%%      New_Step = Step,
      % Calc new Range
      New_Start = End + 1,
      New_End = New_Start + New_Step,
      % Start worker
      io:fwrite("[log] Start Worker of ~p tasks from ~p~n", [New_Step, int_to_chars(New_Start)]),
      start_worker(Master, Target, New_Start, New_End),
      % Loop
      master_loop(State#master{current_int = New_End + 1, step_size = New_Step})
  end.

start_worker(Reporter, Target, Start, End) when is_pid(Reporter), is_integer(Start), is_integer(End) ->
  io:fwrite("[debug] start_worker ~p~~>~p~n", [Start, End]),
  Start_Chars = int_to_chars(Start),
  End_Chars = int_to_chars(End),
  Worker = #worker{
    master_pid = Reporter
    , target_hash = Target
    , current_chars = Start_Chars
    , end_chars = End_Chars
    , start_time = erlang:timestamp()
  },
  wpool:cast(?MODULE, {?MODULE, worker_loop, [Worker]}).

worker_loop(State = #worker{
  current_chars = Msg,
  end_chars = Last_Msg,
  target_hash = Target_Hash,
  master_pid = Pid}) ->
  io:fwrite("~p~~>~p~n", [Msg, Last_Msg]),
%%  io:fwrite("."),
  case crypto:hash(md5, Msg) of
    Target_Hash ->
      Pid ! {found, Msg};
    _ ->
      case Msg of
        Last_Msg ->
          Now = erlang:timestamp(),
          Diff = timer:now_diff(Now, State#worker.start_time),
          Pid ! {not_found, Diff};
        _ ->
          Next_Msg = next_chars(Msg),
          worker_loop(State#worker{current_chars = Next_Msg})
      end
  end
.

next_chars([]) ->
  [?ZERO];
next_chars([?MAX | T]) ->
  [?ZERO | next_chars(T)];
next_chars([X | T]) ->
  [X + 1 | T].

int_to_chars(0) ->
  [];
int_to_chars(X) when X > 0 ->
  int_to_chars_iter(X, []).

int_to_chars_iter(0, Acc) ->
  Acc;
int_to_chars_iter(X, Acc) when X > 0 ->
  Int = X rem ?N_VALID_CHAR,
  Next = X div ?N_VALID_CHAR,
  C = ?int_to_char(Int),
  int_to_chars_iter(Next, [C | Acc]).

%%% --------------------------------------------------
%%% Debug Functions
%%% --------------------------------------------------
debug() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(?MODULE, '_', []),
  dbg:p(all, c).
debug2() ->
  dbg:tpl(?MODULE, '_', []).
