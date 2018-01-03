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

report() ->
  gen_server:call({master, util:get_server_node()}, report).

%% gen_server.

init([]) ->
  case util:role() of
    master ->
      io:fwrite("[log] register master ~p~n", [self()]),
      register(master, self()),
      {ok, #manager{}};
    worker ->
      Master = {master, util:get_server_node()},
      erlang:monitor(process, Master),
      Worker_List = start_worker(Master),
      Worker_Set = sets:from_list(Worker_List),
      State = #manager{
        worker_pids = Worker_Set,
        remote_master = Master
      },
      {ok, State}
  end.

handle_call(report, _From, State) ->
  Current_Int = State#manager.current_int,
  Current_Str = hash_finder:int_to_chars(Current_Int),
  Report = #{
    n_worker => sets:size(State#manager.worker_pids)
    , n_node => length(nodes())
    , current_int => Current_Int
    , current_str => Current_Str
  },
  {reply, Report, State};
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
  Workers = State1#manager.ready_worker_pids,
  N_Worker = sets:size(Workers),
  io:fwrite("[log] number of ready workers: ~p~n", [N_Worker]),
  if N_Worker > 0 ->
    New_Current_Int = dispatch_to_worker(State1),
    State2 = State1#manager{current_int = New_Current_Int},
    {noreply, State2};
    N_Worker == 0 ->
      erlang:send_after(1000, self(), check_worker),
      {noreply, State1}
  end;
handle_cast({ready, Worker}, State) when is_pid(Worker) ->
  io:fwrite("[manager] worker ~p ready~n", [Worker]),
  New_State = State#manager{
    worker_pids = sets:add_element(Worker, State#manager.worker_pids)
    , ready_worker_pids = sets:add_element(Worker, State#manager.worker_pids)
  },
  New_Current_Int = dispatch_to_worker(State),
  {noreply, New_State#manager{current_int = New_Current_Int}};
handle_cast({found, Msg}, State) ->
  State#manager.reporter_pid ! Msg,
  sets:fold(
    fun(Worker, _) ->
      gen_server:cast(Worker, stop)
    end, ok, State#manager.worker_pids),
  New_State = State#manager{target_hash = undefined},
  {noreply, New_State};
handle_cast({not_found, Worker, Old_Step, Diff}, State = #manager{current_int = Start}) ->
  if is_binary(State#manager.target_hash) ->
%%    io:fwrite("[~p] worker ~p finished in ~p~n", [?MODULE, Worker, Diff]),
    N_Worker = sets:size(State#manager.worker_pids),
    Target_Diff = ?INTERVAL * N_Worker,
    Step_Guess = Old_Step * Target_Diff / (Diff + 1),
    Alpha = 0.1,
    Step = floor((Old_Step * (1 - Alpha)) + (Step_Guess * Alpha)) + 1,
    Checkpoint = hash_finder:int_to_chars(Start),
    io:fwrite("~p~n", [#{
      checkpoint=>Checkpoint
      , step=>Step
%%      , step_guess=>Step_Guess
%%      , target_diff=>Target_Diff
%%      , n_worker=>N_Worker
    }]),
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
    true ->
      {noreply, State}
  end;
handle_cast(_Msg, State) ->
  io:fwrite("[~p] unknown cast: ~p~n", [?MODULE, _Msg]),
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Master = {master, _}, Reason}, State) ->
  io:fwrite("[info] manager lost connection to ~p ~p~n", [Master, Reason]),
  erlang:self() ! reconnect,
  {noreply, State};
handle_info(check_worker, State = #manager{ready_worker_pids = Workers}) ->
  case sets:size(Workers) of
    0 ->
      erlang:send_after(1000, self(), check_worker),
      {noreply, State};
    N when N > 0 ->
      New_Current_Int = dispatch_to_worker(State),
      New_State = State#manager{current_int = New_Current_Int},
      {noreply, New_State}
  end;
handle_info(reconnect, State) ->
  case reconnect(State) of
    {ok, New_State} ->
      {noreply, New_State};
    more ->
      {noreply, State}
  end;
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
start_worker(C, N, Pid_List, _Master) when C > N ->
  Pid_List;
start_worker(C, N, Pid_List, Master) when C =< N ->
  {ok, Pid} = gen_server:start_link(hash_finder_worker, [Master], []),
  start_worker(C + 1, N, [Pid | Pid_List], Master).

get_master(#manager{remote_master = Remote_Master}) ->
  case util:role() of
    master ->
      self();
    worker ->
      Remote_Master
  end.

reconnect(State) ->
  Node = util:get_server_node(),
  util:println_iolist(["[log] ", util:now_iolist(), " try to reconnect to master ", io_lib:format("~p", [Node])]),
  case net_kernel:connect(Node) of
    true ->
      util:println_iolist(["[log] ", util:now_iolist(), " connected to master"]),
      Master = {master, Node},
      erlang:monitor(process, Master),
      Worker_List = start_worker(Master),
      Worker_Set = sets:from_list(Worker_List),
      New_State = State#manager{
        worker_pids = Worker_Set,
        remote_master = Master
      },
      {ok, New_State};
    false ->
      erlang:send_after(1000, self(), reconnect),
      more
  end.

dispatch_to_worker(State = #manager{
  target_hash = Hex
  , ready_worker_pids = Workers
  , step_size = Step}) ->
  New_Current_Int = sets:fold(
    fun(Worker, Current_Int) ->
      Task = #task{
        master_pid = get_master(State)
        , target_hash = Hex
        , start_int = Current_Int
        , end_int = Current_Int + Step
      },
      gen_server:cast(Worker, {start, Task}),
      Current_Int + Step + 1
    end,
    State#manager.current_int, Workers
  ),
  New_Current_Int.
