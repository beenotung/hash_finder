-module(hash_finder).
-behaviour(gen_server).

%%
%% target spec: not longer than 26 ascii
%%

%% API.
-export([start_link/0]).
-export([find/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  pool_pid
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

find(Str) when is_list(Str) ->
  gen_server:call(server(), {find, Str}).

%% gen_server.

init([]) ->
  io:fwrite("hash_finder start at ~w~n", [erlang:self()]),
  {ok, Pid} = wpool:start_sup_pool(?MODULE),
  erlang:link(Pid),
  {ok, #state{pool_pid = Pid}}.

handle_call({find, Str}, _From, State) ->
  find(Str, []),
  {reply, sent_to_pool, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:fwrite("info(~p,~p)~n", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
server() ->
  {ok, Pid} = hash_finder_sup:get_child(?MODULE),
  Pid.

-define(valid_chars, lists:seq(32, 126)).
-define(n_valid_chars, length(?valid_chars)).
-define(zero, 32).
-define(max, 126).
-define(int_to_char(X), X + ?zero).
-compile(export_all).

find(Target, []) ->
  io:fwrite("to find for ~p~n", [Target]),
  Stats = wpool:stats(),
  Size = proplists:get_value(size, Stats),
  io:fwrite("Size=~p~n", [Size]),
  todo.

%%  32 is like 0
%% 126 is like 9
%% but 'leading zero' has value
%% Like: 0 < 1 < 2 < 9 < 0,0 < 0,1 < 0,2 < 9,9 < 0,0,0 < 0,0,1
%% Example:
%%   32 + 1 ~~> 33
%%   126 + 1 ~~> 32,32
%%   126,32 + 1 ~~> 32,33
%%   126,126 + 1 ~~> 32,32,32
next([]) ->
  [?zero];
next([?max]) ->
  [32, 32];
next([126, X]) ->
  [32, X + 1].


valid_chars() ->
  ?valid_chars.

int_to_chars(0) ->
  [];
int_to_chars(X) when X > 0 ->
  int_to_chars_acc(X, []).

int_to_chars_acc(0, Acc) ->
  Acc;
int_to_chars_acc(X, Acc) when X > 0 ->
  Int = X rem ?n_valid_chars,
  Next = X div ?n_valid_chars,
  C = ?int_to_char(Int),
  io:fwrite("~p~n", [{Int, Next, C}]),
  int_to_chars_acc(Next, [C | Acc]).

debug() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(?MODULE, '_', []),
  dbg:p(all, c).
debug2() ->
  dbg:tpl(?MODULE, '_', []).
