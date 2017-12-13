#!/usr/bin/env escript
main([N_Str])->
  N = list_to_integer(N_Str),
  control:foreach(
    fun(X)->
        S = hash_finder:int_to_chars(X),
        Next = hash_finder:next(S),
        io:format("~p ~~> ~p~n", [S, Next])
    end, N).
