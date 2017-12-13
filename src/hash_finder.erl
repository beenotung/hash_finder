%%
%% Find the original message for a given hash
%% target spec: not longer than 26 ascii
%%
-module(hash_finder).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API.
-export([start_link/0]).
-export([find/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Debug
-export([valid_chars/0]).
-export([debug/0, debug2/0]).

-record(state, {
}).

%%% ---------------------------------------------------
%%% API.
%%% ---------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

find_test() ->
  % the answer is "32"
  find("6364d3f0f495b6ab9dcf8d3b5c6e0b01", self()),
  % the answer is unknown
  find("424d640bf87ff260f9b263503fc78990", self()).

find(Hash, Reporter) when is_list(Hash), is_pid(Reporter) ->
%%  Upper_Hash = string:to_upper(Hash),
  Hash_Bin = hex:str_to_bin(Hash),
  find(Hash_Bin, Reporter);
find(Hash, Reporter) when is_binary(Hash), is_pid(Reporter) ->
  gen_server:cast(server(), {find, Hash, Reporter}).

%%% ---------------------------------------------------
%%% gen_server.
%%% ---------------------------------------------------
init([]) ->
  io:fwrite("hash_finder start at ~w~n", [erlang:self()]),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({dist_find, _Str, From}, State) ->
  int_to_chars(0),
  From ! {error, not_impl},
  {noreply, State};
handle_cast({find, Str, From}, State) ->
  From ! find_acc(Str, []),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ---------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------
server() ->
  {ok, Pid} = hash_finder_sup:get_child(?MODULE),
  Pid.

-define(valid_chars, lists:seq(32, 126)).
-define(n_valid_chars, length(?valid_chars)).
-define(zero, 32).
-define(max, 126).
-define(int_to_char(X), X + ?zero).

find_acc(Target_Hash, Msg) ->
  case crypto:hash(md5, Msg) of
    Target_Hash ->
      io:fwrite("found! it's ~p~n", [Msg]),
      Msg;
    _Msg_Hash ->
%%      io:fwrite("tried ~p but no luck, go ahead~n", [Msg]),
%%      io:fwrite("(~p) ~~> ~p =/= ~p~n", [Msg, _Msg_Hash, Target_Hash]),
      find_acc(Target_Hash, next(Msg))
  end.

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
  io:fwrite("expand 1 digit~n"),
  [?zero];
next([?max | T]) ->
  [?zero | next(T)];
next([X | T]) ->
  [X + 1 | T].

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
  int_to_chars_acc(Next, [C | Acc]).

%%% ---------------------------------------------------
%%% Debug functions
%%% ---------------------------------------------------
debug() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(?MODULE, '_', []),
  dbg:p(all, c).
debug2() ->
  dbg:tpl(?MODULE, '_', []).
