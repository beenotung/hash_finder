%%%
%%% manage worker pools to find one hash
%%% this gen_server do not handle multiple hash searching at the same time
%%%
-module(hash_finder_manager).
-behaviour(gen_server).
-include_lib("./shared.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
  case util:role() of
    master ->
      register(master, self()),
      {ok, #manager{worker_pids = []}};
    worker ->
      Master = {master, util:get_server_node()},
      Workers = start_worker(Master),
      State = #manager{
        worker_pids = Workers,
        remote_master = Master
      },
      {ok, State}
  end.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({find, Reporter, Hex}, State0) when is_pid(Reporter), is_binary(Hex) ->
  io:fwrite("[manager] finding ~p~n", [Hex]),
  Step = 1,
  State1 = State0#manager{
    reporter_pid = Reporter
    , target_hash = Hex
    , current_int = 0
    , step_size = Step
  },
  io:fwrite("number of ready workers: ~p~n", [sets:size(State1#manager.ready_worker_pids)]),
  New_Current_Int = sets:fold(
    fun(Worker, Current_Int) ->
      Task = #task{
        master_pid = get_master(State0)
        , target_hash = Hex
        , start_int = Current_Int
        , end_int = Current_Int + Step
      },
      gen_server:cast(Worker, {start, Task}),
      Current_Int + Step + 1
    end,
    State1#manager.current_int, State1#manager.ready_worker_pids
  ),
  State = State1#manager{current_int = New_Current_Int},
  {noreply, State};
handle_cast({ready, Worker}, State) when is_pid(Worker) ->
  io:fwrite("[manager] worker ~p ready~n", [Worker]),
  New_List = sets:add_element(Worker, State#manager.ready_worker_pids),
  New_State = State#manager{ready_worker_pids = New_List},
  {noreply, New_State};
handle_cast({found, Msg}, State) ->
  State#manager.reporter_pid ! Msg,
  lists:foreach(
    fun(Worker) -> gen_server:cast(Worker, stop) end
    , State#manager.worker_pids),
  New_State = #manager{
    worker_pids = State#manager.worker_pids
    , ready_worker_pids = State#manager.ready_worker_pids
  },
  {noreply, New_State};
handle_cast({not_found, Worker, Old_Step, Diff}, State = #manager{current_int = Start}) ->
%%  io:fwrite("[~p] worker ~p finished in ~p~n", [?MODULE, Worker, Diff]),
  Target_Diff = ?INTERVAL * length(State#manager.worker_pids),
  Step_Guess = Old_Step * Target_Diff / (Diff + 1),
  Alpha = 0.1,
  Step = floor((Old_Step * (1 - Alpha)) + (Step_Guess * Alpha)) + 1,
  Checkpoint = hash_finder:int_to_chars(Start),
  io:fwrite("[~p] checkpoint: [~p] ~p~n", [?MODULE, length(Checkpoint), Checkpoint]),
  End = Start + Step,
  Task = #task{
    master_pid = get_master(State)
    , target_hash = State#manager.target_hash
    , start_int = Start
    , end_int = End
  },
  gen_server:cast(Worker, {start, Task}),
  Default_Step = floor((Step + State#manager.step_size) / 2) + 1,
  New_State = State#manager{current_int = End + 1, step_size = Default_Step},
  {noreply, New_State};
handle_cast(_Msg, State) ->
  io:fwrite("[~p] unknown cast: ~p~n", [?MODULE, _Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  io:fwrite("[~p] unknown info: ~p~n", [?MODULE, _Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Functions

% start from step 1
% the step can be adjusted upon task progress feedback
start_worker(Master) ->
  N = erlang:system_info(schedulers),
%%  N = 100,
  start_worker(1, N, [], Master).
start_worker(C, N, Pids, _Master) when C > N ->
  Pids;
start_worker(C, N, Pids, Master) when C =< N ->
  {ok, Pid} = gen_server:start_link(hash_finder_worker, [Master], []),
  start_worker(C + 1, N, [Pid | Pids], Master).

get_master(#manager{remote_master = Remote_Master}) ->
  case util:role() of
    master ->
      self();
    worker ->
      Remote_Master
  end.
