%%%
%%% Find the original message for a given hash
%%% target spec: not longer than 26 ascii
%%%
-module(hash_finder).

-include_lib("eunit/include/eunit.hrl").
-include_lib("./shared.hrl").

% API
-export([find/1]).
% Test
%%-export([chars_test/2]).
%%-export([next_n/2]).
%%-export([next_n_iter/2]).
%%-export([next_n_test/1]).
% Debug
-export([debug/0, debug2/0]).

% Internal
-export([int_to_chars/1]).
-export([next_chars/1]).

% Internal
-define(ZERO, 32).
-define(MAX, 126).
-define(N_VALID_CHAR, (?MAX - ?ZERO + 1)).
-define(int_to_char(X), (X + ?ZERO)).

%%% --------------------------------------------------
%%% API Functions
%%% --------------------------------------------------
find(Hash) when is_list(Hash) ->
  Bin = hex:str_to_bin(Hash),
  find(Bin);
find(Hash) when is_binary(Hash) ->
  Reporter = self(),
  {ok, Manager} = erlib:get_sup_child(hash_finder_sup, hash_finder_manager),
  gen_server:cast(Manager, {find, Reporter, Hash}),
  receive
    X when is_list(X) ->
      io:fwrite("received X: ~p~n", [X]),
      X
  end.

%%% --------------------------------------------------
%%% Test Functions
%%% --------------------------------------------------
find_test() ->
  io:fwrite("[test] testing ~p.~n", ["32"]),
  "32" = find("6364d3f0f495b6ab9dcf8d3b5c6e0b01"),
  io:fwrite("[test] passed test on ~p.~n", ["32"]),
  Msg = find("424d640bf87ff260f9b263503fc78990"),
  io:fwrite("Found Res = ~p.~n", [Msg]),
  ok.

chars_test() ->
  chars_test(1, 1),
  chars_test(1, 100).

chars_test(Start, N) ->
  End = Start + N,
  Start_Cs = int_to_chars(Start),
  End_Cs = int_to_chars(End),
  ?assertEqual(End_Cs, next_n(Start_Cs, N)).

next_n_test(X) ->
  ?assertEqual(next_n_iter("", X), next_n("", X)).
next_n_test() ->
  next_n_test(0),
  next_n_test(1),
  next_n_test(93),
  next_n_test(93),
  next_n_test(94),
  next_n_test(95),
  next_n_test(96),
  next_n_test(100),
  next_n_test(1000),
  next_n_test(10000),
  next_n_test(1000000).

%%% --------------------------------------------------
%%% Internal Functions
%%% --------------------------------------------------
next_chars([]) ->
  [?ZERO];
next_chars([?MAX | T]) ->
  [?ZERO | next_chars(T)];
next_chars([X | T]) ->
  [X + 1 | T].

next_n_iter(Cs, 0) ->
  Cs;
next_n_iter(Cs, N) when N > 0 ->
  next_n_iter(next_chars(Cs), N - 1).

next_n(Cs, 0) ->
  Cs;
next_n([], N) when N >= 0, N =< ?N_VALID_CHAR ->
  [N + ?ZERO - 1];
next_n([], N) when N >= 0, N > ?N_VALID_CHAR ->
  {Base_Int, N_Char} = n_char(N),
  Base_Cs = lists:duplicate(N_Char, ?ZERO),
  next_n(Base_Cs, N - Base_Int);
next_n([X | T], N) when X + N =< ?MAX ->
  [X + N | T];
next_n([X | T], N) when X + N > ?MAX ->
  H = (X - ?ZERO + N),
  Y = (H rem ?N_VALID_CHAR) + ?ZERO,
  R = H div ?N_VALID_CHAR,
  [Y | next_n(T, R)].

%% next_n helper
-spec n_char(pos_integer()) -> {Base, N_Char} when Base :: pos_integer(), N_Char :: pos_integer().
n_char(X) ->
  n_char(X, 0, 1, 0).
n_char(X, Base0, Pow0, N) ->
  Base1 = Base0 + Pow0,
  Pow1 = Pow0 * 95,
  if X < Base1 ->
    {Base0, N};
    true ->
      n_char(X, Base1, Pow1, N + 1)
  end.

int_to_chars(X) when X >= 0 ->
  next_n("", X).

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
