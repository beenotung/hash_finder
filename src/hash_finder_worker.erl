-module(hash_finder_worker).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("./shared.hrl").

%% API.
-export([start_link/0]).
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  sup_pid
  , worker_pid
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% gen_server.

init([Sup_Pid]) when is_pid(Sup_Pid) ->
  gen_server:cast(Sup_Pid, {ready, self()}),
  {ok, #state{sup_pid = Sup_Pid}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({start, Task}, State = #state{worker_pid = Old_Worker}) when is_record(Task, task) ->
  if
    is_pid(Old_Worker) ->
      stop_worker(Old_Worker, new_task);
    true ->
      ok
  end,
  Worker_Pid = start_task(Task),
  New_State = State#state{
    worker_pid = Worker_Pid
  },
  {noreply, New_State};
handle_cast(stop, #state{worker_pid = Worker, sup_pid = Sup}) ->
  stop_worker(Worker, cast_request),
  gen_server:cast(Sup, {ready, self()}),
  {noreply, #state{sup_pid = Sup}};
handle_cast(_Msg, State) ->
  io:fwrite("[~p] unknown cast: ~p~n", [?MODULE, _Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Functions
worker_loop(State = #worker{
  current_chars = Msg,
  end_chars = Last_Msg,
  target_hash = Target_Hash,
  master_pid = Master}) ->
  case crypto:hash(md5, Msg) of
    Target_Hash ->
      gen_server:cast(Master, {found, Msg});
    _ ->
      case Msg of
        Last_Msg ->
          Now = erlang:timestamp(),
          Diff = timer:now_diff(Now, State#worker.start_time),
          gen_server:cast(Master, {not_found, State#worker.worker_pid, State#worker.n, Diff});
        _ ->
          Next_Msg = hash_finder:next_chars(Msg),
          worker_loop(State#worker{current_chars = Next_Msg})
      end
  end
.

stop_worker(Pid, Reason) ->
%%  io:fwrite("[~p] ~p stop worker ~p~n", [?MODULE, self(), Pid]),
  erlang:unlink(Pid),
  erlang:exit(Pid, Reason).

start_task(Task) ->
  Worker = #worker{
    master_pid = Task#task.master_pid
    , worker_pid = self()
    , target_hash = Task#task.target_hash
    , current_chars = hash_finder:int_to_chars(Task#task.start_int)
    , end_chars = hash_finder:int_to_chars(Task#task.end_int)
    , start_time = erlang:timestamp()
    , n = Task#task.end_int - Task#task.start_int + 1
  },
%%  io:fwrite("[~p] ~p start ~p~~>~p~n", [?MODULE, self(), Worker#worker.current_chars, Worker#worker.end_chars]),
  spawn_link(fun() -> worker_loop(Worker) end).
